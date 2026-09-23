#A connection as far as analyse() is concerned, so that the arguments checked
#after it can be reached without a database to check them against
aConnection <- function() {
  return(structure(list(), class="MariaDBConnection"))
}

test_that("analysing needs a database to analyse into", {
  expect_error(analyse("audioblast"), "db is not a MariaDBConnection")
  expect_error(analyse(NULL), "db is not a MariaDBConnection")
})

test_that("an argument that is not what it should be is said to be so", {
  db <- aConnection()
  expect_error(analyse(db, db_legacy="yes"), "db_legacy must be logical")
  expect_error(analyse(db, mode="remote"), "mode must be one of")
  expect_error(analyse(db, source=1), "source must be a character vector")
  expect_error(analyse(db, debug="yes"), "debug must be logical")
  expect_error(analyse(db, verbose="loudly"), "verbose must be logical")
  expect_error(analyse(db, force="yes"), "force must be logical")
  expect_error(analyse(db, base_dir=1), "base_dir must be a character vector")
})

test_that("debugging one recording needs to be told which, and what to do to it", {
  db <- aConnection()
  expect_error(analyse(db, debug=TRUE), "id must be a character vector")
  expect_error(analyse(db, debug=TRUE, id="58428"), "task must be a character vector")
})

#A task claimed for a recording, in the shape `tasks-data` answers in
aClaimedTask <- function(task="soundscapes_minute") {
  return(someRows(source="unp", id="1", file="1.wav", type="audio/x-wav",
                  Duration=60, task=task, process="p"))
}

#How many times an agent asked the database for work
claimsMade <- function(mocked) {
  return(sum(vapply(mocked$executed, function(s) startsWith(s$sql, "CALL `claim-tasks`"),
                    logical(1))))
}

test_that("an agent that claims nothing asks again before it stops", {
  waited <- numeric()
  local_mocked_bindings(pause=function(seconds) waited <<- c(waited, seconds))
  mocked <- mockDB(analyse(aConnection(), mode="local", source="unp"))

  #Five waits, and a claim before and after each of them
  expect_length(waited, 5)
  expect_identical(claimsMade(mocked), 6L)
})

test_that("claiming something starts the count of empty claims again", {
  #Losing a race for the one recording every agent asked for is not the end
  #of the work: the next claim may win
  waited <- numeric()
  local_mocked_bindings(pause=function(seconds) waited <<- c(waited, seconds))
  mocked <- expect_warning(
    mockDB(analyse(aConnection(), mode="local", source="unp"),
           rows=list(noRows(), aClaimedTask(), noRows())),
    "Not a task this agent does")

  #An empty claim and its wait, the claim that won, and six more after it
  expect_identical(claimsMade(mocked), 8L)
  expect_length(waited, 6)
})

test_that("the waits after empty claims grow, are spread, and end", {
  set.seed(1)
  waits <- vapply(1:5, emptyClaimWait, numeric(1))
  expect_true(all(waits >= c(1, 5, 15, 30, 60) * 0.5))
  expect_true(all(waits <= c(1, 5, 15, 30, 60) * 1.5))
  expect_identical(emptyClaimWait(6), NA_real_)
  #Two agents that collided do not wait exactly as long as each other
  expect_false(identical(emptyClaimWait(3), emptyClaimWait(3)))
})

#The statements a mocked database was asked to execute that start as given
statementsLike <- function(mocked, start) {
  return(Filter(function(s) startsWith(s$sql, start), mocked$executed))
}

test_that("a recording that did not download has its tasks given back, not measured", {
  local_mocked_bindings(pause=function(seconds) NULL,
                        webFile=function(...) NA_character_,
                        downloadAttempts=function() 3)
  #One recording is claimed, then nothing more
  mocked <- expect_warning(
    mockDB(analyse(aConnection(), mode="web", source="xeno-canto"),
           rows=list(aClaimedTask("recordings_calculated"), noRows())),
    "Could not download unp 1 and gave its tasks back")

  expect_length(statementsLike(mocked, "DELETE FROM `tasks-progress`"), 1)
  #Nothing was written about a file that was never had
  expect_length(statementsLike(mocked, "INSERT INTO `recordings-calculated`"), 0)
  expect_length(statementsLike(mocked, "CALL `delete-task`"), 0)
})

test_that("a recording that will not download is recorded as missing at its address", {
  local_mocked_bindings(pause=function(seconds) NULL,
                        webFile=function(...) NA_character_,
                        downloadAttempts=function() 3)
  #The same recording is claimed three times, as the first unclaimed one is
  claim <- aClaimedTask("recordings_calculated")
  claim$file <- "https://xeno-canto.org/1000001/download"
  mocked <- suppressWarnings(
    mockDB(analyse(aConnection(), mode="web", source="xeno-canto"),
           rows=list(claim, noRows(), claim, noRows(), claim, noRows())))

  #Given back twice, then measured, written and crossed off
  expect_length(statementsLike(mocked, "DELETE FROM `tasks-progress`"), 2)
  written <- statementsLike(mocked, "INSERT INTO `recordings-calculated`")
  expect_length(written, 1)
  expect_identical(written[[1]]$params[[11]], "missing")
  expect_identical(written[[1]]$params[[12]], "No file at https://xeno-canto.org/1000001/download")
  expect_length(statementsLike(mocked, "CALL `delete-task`"), 1)
})

test_that("an agent has its connection speak UTF-8 before it asks for anything", {
  local_mocked_bindings(pause=function(seconds) NULL)
  mocked <- mockDB(analyse(aConnection(), mode="local", source="unp"))

  expect_identical(onlyStatement(mocked, 1)$sql, "SET NAMES utf8mb4;")
})
