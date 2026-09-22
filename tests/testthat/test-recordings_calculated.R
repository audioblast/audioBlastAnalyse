test_that("a recording that has not been measured is measured and written", {
  path <- aWave()
  on.exit(unlink(path))
  mocked <- mockDB(recordings_calculated("db", "bio.acousti.ca", "58428", path))

  expect_identical(mocked$value, "measured")
  expect_length(mocked$executed, 1)
  statement <- onlyStatement(mocked)
  expect_identical(statement$sql, calculatedSQL())
  expect_identical(statement$params[[1]], "bio.acousti.ca")
  expect_identical(statement$params[[2]], "58428")
  expect_match(statement$params[[3]], "^[0-9a-f]{64}$")
  expect_equal(statement$params[[4]], 1)
  expect_identical(statement$params[[11]], "ok")
})

test_that("a recording that has been measured is left as it is", {
  path <- aWave()
  on.exit(unlink(path))
  mocked <- mockDB(recordings_calculated("db", "bio.acousti.ca", "58428", path),
                   rows=someRows(status="ok", calculated_by="audioBlastAnalyse 0.0.0.9000"))

  expect_identical(mocked$value, "kept")
  expect_length(mocked$executed, 0)
})

test_that("force measures a recording again, without asking how it went before", {
  path <- aWave()
  on.exit(unlink(path))
  mocked <- mockDB(recordings_calculated("db", "bio.acousti.ca", "58428", path, force=TRUE),
                   rows=someRows(status="ok", calculated_by="audioBlastAnalyse 0.0.0.9000"))

  expect_identical(mocked$value, "measured")
  expect_length(mocked$executed, 1)
  #Nothing is asked of a table whose answer would be ignored
  expect_length(mocked$queried, 0)
})

test_that("a recording measured as unreadable before is measured again", {
  #A task is only in hand because something asked for it, and a file that could
  #not be read may since have been fetched again
  path <- aWave()
  on.exit(unlink(path))
  mocked <- mockDB(recordings_calculated("db", "unp", "1", path),
                   rows=someRows(status="unreadable", calculated_by="audioBlastAnalyse 0.0.0.9000"))

  expect_identical(mocked$value, "measured")
  expect_length(mocked$executed, 1)
})

test_that("a recording that cannot be read is written as unreadable, not passed over", {
  path <- aFile()
  on.exit(unlink(path))
  mocked <- expect_warning(mockDB(recordings_calculated("db", "unp", "1", path)),
                           "Could not measure unp 1")

  expect_identical(mocked$value, "unmeasurable")
  expect_length(mocked$executed, 1)
  expect_identical(onlyStatement(mocked)$params[[11]], "unreadable")
})

test_that("a recording whose file is gone is written as missing", {
  mocked <- expect_warning(
    mockDB(recordings_calculated("db", "unp", "1", file.path(tempdir(), "gone.wav"))),
    "Could not measure unp 1")

  expect_identical(mocked$value, "unmeasurable")
  expect_identical(onlyStatement(mocked)$params[[11]], "missing")
})

test_that("measurements the database would not keep are worth measuring again", {
  path <- aWave()
  on.exit(unlink(path))
  local_mocked_bindings(
    dbGetQuery=function(conn, statement, params=NULL, ...) data.frame(),
    dbExecute=function(conn, statement, params=NULL, ...) stop("The database has gone away"),
    backoff=function() c(0, 0))

  warned <- character()
  withCallingHandlers(
    outcome <- recordings_calculated("db", "bio.acousti.ca", "58428", path),
    warning=function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  expect_identical(outcome, "retry")
  #The database says it gave up, and the measuring says what was lost with it
  expect_match(warned, "Gave up on a statement", all=FALSE)
  expect_match(warned, "Could not write the measurements of bio.acousti.ca 58428", all=FALSE)
})

test_that("what was measured is said when asked for", {
  path <- aWave()
  on.exit(unlink(path))
  expect_output(mockDB(recordings_calculated("db", "s", "1", path, verbose=TRUE)),
                "44100 Hz, pcm_s16le")
  expect_silent(mockDB(recordings_calculated("db", "s", "1", path)))
})

test_that("a recording left as it is says so when asked for", {
  path <- aWave()
  on.exit(unlink(path))
  expect_output(
    mockDB(recordings_calculated("db", "s", "1", path, verbose=TRUE), rows=someRows(status="ok")),
    "Already measured: s 1")
})
