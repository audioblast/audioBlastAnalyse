#Claiming is what keeps a fleet of agents off each other's work. The claim
#itself is made by the database, so what is checked here is that the agent asks
#for it correctly, and that it reads back what it was given in a way that works
#on every machine it runs on. The statements are read rather than run.

#The statement the agent claimed with
theClaim <- function(mocked) {
  return(mocked$executed[[1]])
}

test_that("claiming asks for a routine that answers with nothing", {
  #It is answering with rows that the client build on the NHM HPC cannot be
  #relied on for. `claim-tasks` answers with nothing, so one path serves every
  #machine and db_legacy has nothing left to do.
  mocked <- mockDB(fetchUnanalysedRecordings("db", "unp", "agent1"))

  expect_identical(theClaim(mocked)$sql, "CALL `claim-tasks`(?, ?, ?, ?, ?);")
  #Asked as a statement, not as a query: nothing is expected back from it
  expect_length(mocked$executed, 1)
})

test_that("the agent reads back what it won with a plain SELECT", {
  mocked <- mockDB(fetchUnanalysedRecordings("db", "unp", "agent1"))

  expect_length(mocked$queried, 1)
  expect_match(mocked$queried, "^SELECT ")
  expect_match(mocked$queried, "WHERE `tasks-progress`.`process` = ?;", fixed=TRUE)
})

test_that("a claim is read back with the recording it is of, or none where that has gone", {
  #`tasks-data` joins claims to `recordings`, and so hid a claim on a task whose
  #recording had been deleted: the agent neither did it nor cleared it, and a
  #run of them stopped agents as though the work were done. Read from the claims
  #themselves, each comes back, its rec_key NULL where its recording has gone.
  #`tasks` is not read either, so a task held twice over there comes back once.
  mocked <- mockDB(fetchDownloadableRecordings("db", "bio.acousti.ca", "agent1"))
  read <- mocked$queried

  expect_match(read, "FROM `tasks-progress` LEFT JOIN `recordings`", fixed=TRUE)
  expect_match(read, "`recordings`.`rec_key`", fixed=TRUE)
  expect_false(grepl("tasks-data", read, fixed=TRUE))
  expect_false(grepl("`tasks`", read, fixed=TRUE))
})

#Tasks claimed, as heldBy() reads them back: one of a recording that is there
#and one of a recording that has been deleted, with no file and no rec_key
someClaims <- function() {
  return(someRows(source=c("bio.acousti.ca", "bio.acousti.ca"), id=c("10753", "56106"),
                  file=c("https://files.audioblast.org/bioacoustica/a.wav", NA),
                  type=c("audio/x-wav", NA), Duration=c("5", NA),
                  task=c("recordings_calculated", "recordings_calculated"),
                  process=c("agent1", "agent1"), rec_key=c(7L, NA)))
}

test_that("a task claimed for a recording that has been deleted is cleared", {
  #It can never be done, so it is crossed off rather than given back for the
  #next agent to claim, and the tasks of recordings that are there are kept
  mocked <- mockDB(clearDeleted("db", someClaims(), "agent1"))

  expect_identical(mocked$value$id, "10753")
  expect_length(mocked$executed, 1)
  expect_identical(onlyStatement(mocked)$sql, "CALL `delete-task`(?, ?, ?, ?);")
  expect_identical(onlyStatement(mocked)$params,
                   list("agent1", "bio.acousti.ca", "56106", "recordings_calculated"))
})

test_that("nothing is cleared while every recording claimed is there", {
  claims <- someClaims()[1, , drop=FALSE]
  mocked <- mockDB(clearDeleted("db", claims, "agent1"))

  expect_identical(mocked$value, claims)
  expect_length(mocked$executed, 0)
  expect_identical(nrow(mockDB(clearDeleted("db", noTasks(), "agent1"))$value), 0L)
})

test_that("the claim says who is asking, for what, how much and from where", {
  #What is asked for by default depends on whether audiowaveform is installed
  local_mocked_bindings(hasAudiowaveform=function() FALSE, hasFfmpeg=function() FALSE)
  mocked <- mockDB(fetchUnanalysedRecordings("db", "unp", "agent1", n=25))

  expect_identical(theClaim(mocked)$params,
                   list("agent1", 25L, "unp", "abaR", "recordings_calculated"))
})

test_that("an agent asks only for the kinds of task it does", {
  #Without this an agent claims a soundscape task, finds doTask() does not do
  #it, and gives it straight back: ten claimed and ten released, round and
  #round. Most of what abaR is registered for is work it no longer runs.
  local_mocked_bindings(hasAudiowaveform=function() FALSE, hasFfmpeg=function() FALSE)
  expect_identical(tasksDone(), "recordings_calculated")

  mocked <- mockDB(fetchUnanalysedRecordings(
    "db", "unp", "agent1", tasks=c("recordings_calculated", "soundscapes_spec")))

  expect_true("recordings_calculated,soundscapes_spec" %in% theClaim(mocked)$params)
})

test_that("a task list is written as FIND_IN_SET reads it", {
  #FIND_IN_SET matches the list exactly, so a stray space would quietly cost
  #the agent that kind of work
  expect_identical(taskList(c("one ", " two")), "one,two")
  expect_identical(taskList("one"), "one")
  expect_error(taskList(character()), "at least one")
})

test_that("the agent names itself as tasks-agents names it", {
  #`tasks-agents` is PRIMARY KEY (`task`), so a second R agent cannot be
  #registered alongside this one: an agent narrows its work by asking for
  #particular tasks, not by calling itself something else.
  expect_identical(agentName(), "abaR")

  mocked <- mockDB(fetchUnanalysedRecordings("db", "unp", "agent1"))
  expect_true("abaR" %in% theClaim(mocked)$params)
})

test_that("a recording is claimed for all of its outstanding tasks at once", {
  #The web path downloads the file, so every task of that recording is worth
  #claiming while it is in hand
  local_mocked_bindings(hasAudiowaveform=function() FALSE, hasFfmpeg=function() FALSE)
  mocked <- mockDB(fetchDownloadableRecordings("db", "bio.acousti.ca", "agent1"))

  expect_identical(theClaim(mocked)$sql, "CALL `claim-tasks-by-file`(?, ?, ?, ?);")
  expect_identical(theClaim(mocked)$params,
                   list("agent1", "bio.acousti.ca", "abaR", "recordings_calculated"))

  #Where it can make waveform peaks it makes them from the same download
  local_mocked_bindings(hasAudiowaveform=function() TRUE, hasFfmpeg=function() FALSE)
  mocked <- mockDB(fetchDownloadableRecordings("db", "bio.acousti.ca", "agent1"))
  expect_identical(theClaim(mocked)$params,
                   list("agent1", "bio.acousti.ca", "abaR", "recordings_calculated,waveform_peaks"))
})

test_that("a source or id is bound, never written into the statement", {
  #An id holding a quote would otherwise end the string it was written into
  mocked <- mockDB(fetchUnanalysedRecordings("db", "o'brien", "agent1"))

  expect_false(grepl("o'brien", theClaim(mocked)$sql, fixed=TRUE))
  expect_true("o'brien" %in% theClaim(mocked)$params)
  expect_false(grepl("'", mocked$queried, fixed=TRUE))
})

test_that("there is one way of claiming, not one per client library", {
  #db_legacy existed because claiming was done twice over, by hand on the HPC
  #and by routine everywhere else, and the two drifted apart. Neither claim
  #takes a legacy argument any more.
  expect_false("legacy" %in% names(formals(fetchUnanalysedRecordings)))
  expect_false("legacy" %in% names(formals(fetchDownloadableRecordings)))
})

test_that("a database that cannot be asked means no work, not no answer", {
  #abdbGetQuery() gives NULL once its retries are spent, and analyse() asks
  #nrow() of whatever it gets back. Answering NULL stopped the agent with an
  #error instead of letting it ask again.
  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) 1L,
    dbGetQuery=function(conn, statement, params=NULL, ...) stop("The database has gone away"),
    backoff=function() c(0, 0))

  ss <- suppressWarnings(fetchUnanalysedRecordings("db", "unp", "agent1"))

  expect_true(is.data.frame(ss))
  expect_identical(nrow(ss), 0L)
})

test_that("work claimed is work handed back", {
  mocked <- mockDB(
    fetchUnanalysedRecordings("db", "unp", "agent1"),
    rows=someRows(source="unp", id="1", file="a.wav", type="audio/wav",
                  Duration=60, task="recordings_calculated", process="agent1"))

  expect_identical(nrow(mocked$value), 1L)
  expect_identical(mocked$value[1, "task"], "recordings_calculated")
})

test_that("a statement that fails is named by what it calls, not printed whole", {
  expect_identical(statementName("CALL `claim-tasks-by-file`(?, ?, ?, ?);"), "`claim-tasks-by-file`")
  expect_identical(statementName("  DELETE FROM `tasks-progress`\n  WHERE `process` = ?;"),
                   "DELETE FROM `tasks-progress` WHERE")
})

test_that("retries are spread about their backoff", {
  set.seed(1)
  spreads <- vapply(rep(10, 20), spread, numeric(1))
  expect_true(all(spreads >= 5 & spreads <= 15))
  expect_gt(length(unique(spreads)), 1)
  expect_identical(spread(0), 0)
})
