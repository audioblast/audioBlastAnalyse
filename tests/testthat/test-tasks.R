#The statement an agent settled a claim with, and what it bound to it
settled <- function(mocked) {
  return(mocked$executed[[length(mocked$executed)]])
}

test_that("a task that was done is crossed off", {
  path <- aWave()
  on.exit(unlink(path))
  mocked <- mockDB(doTask("db", "recordings_calculated", "bio.acousti.ca", "58428", path, "agent1"))

  expect_identical(mocked$value, "measured")
  #The measurements, and then the task crossed off
  expect_length(mocked$executed, 2)
  expect_identical(settled(mocked)$sql, "CALL `delete-task`(?, ?, ?, ?);")
  expect_identical(settled(mocked)$params,
                   list("agent1", "bio.acousti.ca", "58428", "recordings_calculated"))
})

test_that("a recording that cannot be read is done with, not given back", {
  path <- aFile()
  on.exit(unlink(path))
  mocked <- suppressWarnings(
    mockDB(doTask("db", "recordings_calculated", "unp", "1", path, "agent1")))

  expect_identical(mocked$value, "unmeasurable")
  expect_identical(settled(mocked)$sql, "CALL `delete-task`(?, ?, ?, ?);")
})

test_that("a recording already measured is crossed off without measuring it again", {
  path <- aWave()
  on.exit(unlink(path))
  mocked <- mockDB(doTask("db", "recordings_calculated", "bio.acousti.ca", "58428", path, "agent1"),
                   rows=someRows(status="ok", calculated_by="audioBlastAnalyse 0.0.0.9000"))

  expect_identical(mocked$value, "kept")
  expect_length(mocked$executed, 1)
  expect_identical(settled(mocked)$sql, "CALL `delete-task`(?, ?, ?, ?);")
})

test_that("a task whose work was not kept is given back, not crossed off", {
  path <- aWave()
  on.exit(unlink(path))
  #The measurements fail to write; giving the task back must still work
  written <- 0
  local_mocked_bindings(
    dbGetQuery=function(conn, statement, params=NULL, ...) data.frame(),
    dbExecute=function(conn, statement, params=NULL, ...) {
      written <<- written + 1
      if (grepl("INSERT INTO `recordings-calculated`", statement, fixed=TRUE)) {
        stop("The database has gone away")
      }
      1L
    },
    backoff=function() c(0, 0))

  outcome <- suppressWarnings(
    doTask("db", "recordings_calculated", "bio.acousti.ca", "58428", path, "agent1"))
  expect_identical(outcome, "released")
})

test_that("a task this agent does not do is given back", {
  mocked <- expect_warning(
    mockDB(doTask("db", "soundscapes_minute", "unp", "1", "nowhere.wav", "agent1")),
    "Not a task this agent does, and given back: soundscapes_minute")

  expect_identical(mocked$value, "released")
  #The claim goes, and only the claim: the task itself stays to be done
  expect_length(mocked$executed, 1)
  expect_match(settled(mocked)$sql, "^DELETE FROM `tasks-progress`")
  expect_identical(settled(mocked)$params,
                   list("agent1", "unp", "1", "soundscapes_minute"))
  expect_false(grepl("delete-task", settled(mocked)$sql, fixed=TRUE))
})

test_that("no task this agent does not do is measured", {
  for (task in c("soundscapes_minute", "soundscapes_second", "soundscapes_spec", "")) {
    mocked <- suppressWarnings(mockDB(doTask("db", task, "unp", "1", "nowhere.wav", "agent1")))
    expect_identical(mocked$value, "released")
    #Nothing was asked of recordings-calculated, so nothing was measured
    expect_length(mocked$queried, 0)
  }
})

test_that("a claim is given back by the agent that made it", {
  mocked <- mockDB(releaseToDo("db", "unp", "1", "recordings_calculated", "agent1"))
  statement <- onlyStatement(mocked)

  expect_identical(
    statement$sql,
    paste("DELETE FROM `tasks-progress`",
          "WHERE `process` = ? AND `source` = ? AND `id` = ? AND `task` = ?;"))
  expect_identical(statement$params, list("agent1", "unp", "1", "recordings_calculated"))
})
