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

  expect_identical(mocked$queried, "SELECT * FROM `tasks-data` WHERE `process` = ?;")
})

test_that("the claim says who is asking, for what, how much and from where", {
  mocked <- mockDB(fetchUnanalysedRecordings("db", "unp", "agent1", n=25))

  expect_identical(theClaim(mocked)$params,
                   list("agent1", 25L, "unp", "abaR", "recordings_calculated"))
})

test_that("an agent asks only for the kinds of task it does", {
  #Without this an agent claims a soundscape task, finds doTask() does not do
  #it, and gives it straight back: ten claimed and ten released, round and
  #round. Most of what abaR is registered for is work it no longer runs.
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
  mocked <- mockDB(fetchDownloadableRecordings("db", "bio.acousti.ca", "agent1"))

  expect_identical(theClaim(mocked)$sql, "CALL `claim-tasks-by-file`(?, ?, ?, ?);")
  expect_identical(theClaim(mocked)$params,
                   list("agent1", "bio.acousti.ca", "abaR", "recordings_calculated"))
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
