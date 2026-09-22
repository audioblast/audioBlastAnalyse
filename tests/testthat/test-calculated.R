test_that("measurements are written as one row, and replace a row already there", {
  expect_identical(
    calculatedSQL(),
    paste("INSERT INTO `recordings-calculated`",
          "(`source`, `id`, `hash`, `duration`, `channels`, `sample_rate`, `bit_depth`,",
          "`bit_rate`, `codec`, `size_raw`, `status`, `error`, `calculated_at`, `calculated_by`)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), ?)",
          "ON DUPLICATE KEY UPDATE `hash` = VALUES(`hash`), `duration` = VALUES(`duration`),",
          "`channels` = VALUES(`channels`), `sample_rate` = VALUES(`sample_rate`),",
          "`bit_depth` = VALUES(`bit_depth`), `bit_rate` = VALUES(`bit_rate`),",
          "`codec` = VALUES(`codec`), `size_raw` = VALUES(`size_raw`),",
          "`status` = VALUES(`status`), `error` = VALUES(`error`),",
          "`calculated_at` = VALUES(`calculated_at`), `calculated_by` = VALUES(`calculated_by`)"))
})

test_that("the recording a measurement is of is its key, and is never updated", {
  sql <- calculatedSQL()
  expect_false(grepl("`source` = VALUES", sql, fixed=TRUE))
  expect_false(grepl("`id` = VALUES", sql, fixed=TRUE))
})

test_that("a measurement is bound, not written into the statement", {
  measurements <- measurementsOf(hash="abc", duration=61.5, channels=2L, sample_rate=48000L,
                                bit_depth=16L, bit_rate=1536000L, codec="pcm_s16le",
                                size_raw=5904000, status="ok", error=NA_character_)
  mocked <- mockDB(writeMeasurements("db", "bio.acousti.ca", "58428", measurements))
  statement <- onlyStatement(mocked)

  expect_length(mocked$executed, 1)
  expect_identical(statement$sql, calculatedSQL())
  expect_identical(statement$params[[1]], "bio.acousti.ca")
  expect_identical(statement$params[[2]], "58428")
  expect_identical(statement$params[[3]], "abc")
  expect_identical(statement$params[[4]], 61.5)
  expect_identical(statement$params[[11]], "ok")
  #Nothing that was measured goes unbound, and the version is bound last
  expect_length(statement$params, length(measurementColumns()) + 3)
  expect_match(statement$params[[13]], "^audioBlastAnalyse ")
})

test_that("a recording that could not be read is written as readily as one that could", {
  measurements <- measurementsOf(status="unreadable", error="The file holds no audio",
                                hash="abc", size_raw=402)
  mocked <- mockDB(writeMeasurements("db", "unp", "nhm-unp-1-1588606809", measurements))
  statement <- onlyStatement(mocked)

  expect_identical(statement$params[[11]], "unreadable")
  expect_identical(statement$params[[12]], "The file holds no audio")
  #What was measured of the bytes is kept, and what was not is NULL
  expect_identical(statement$params[[3]], "abc")
  expect_identical(statement$params[[10]], 402)
  expect_identical(statement$params[[4]], NA_real_)
  expect_identical(statement$params[[5]], NA_integer_)
})

test_that("an id is bound as itself, whatever is in it", {
  mocked <- mockDB(writeMeasurements("db", "xc", "a'b`c\\d", measurementsOf()))
  expect_identical(onlyStatement(mocked)$params[[2]], "a'b`c\\d")
})

test_that("a recording nothing has measured has no status", {
  mocked <- mockDB(calculatedStatus("db", "bio.acousti.ca", "58428"))

  expect_null(mocked$value)
  expect_match(mocked$queried, "FROM `recordings-calculated`")
  expect_match(mocked$queried, "WHERE `source` = \\? AND `id` = \\?")
})

test_that("a recording that has been measured says how it went", {
  mocked <- mockDB(calculatedStatus("db", "bio.acousti.ca", "58428"),
                   rows=someRows(status="ok", calculated_by="audioBlastAnalyse 0.0.0.9000"))

  expect_identical(mocked$value$status, "ok")
  expect_identical(mocked$value$calculated_by, "audioBlastAnalyse 0.0.0.9000")
})

test_that("a query that answers nothing is not read as having failed", {
  #An empty answer and a failed query are different things, and only one of
  #them is worth retrying
  mocked <- mockDB(calculatedStatus("db", "s", "1"), rows=noRows())
  expect_null(mocked$value)
  expect_length(mocked$queried, 1)
})

test_that("a statement is retried, and says so when the retries are spent", {
  tried <- 0
  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      tried <<- tried + 1
      stop("The database has gone away")
    },
    backoff=function() c(0, 0, 0))

  expect_warning(expect_false(abdbExecute("db", "SELECT 1")), "Gave up on a statement")
  expect_identical(tried, 3)
})

test_that("a statement that succeeds is not retried", {
  tried <- 0
  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      tried <<- tried + 1
      1L
    },
    backoff=function() c(0, 0, 0))

  expect_true(abdbExecute("db", "SELECT 1"))
  expect_identical(tried, 1)
})

test_that("a query says when its retries are spent, rather than answering nothing", {
  local_mocked_bindings(
    dbGetQuery=function(conn, statement, params=NULL, ...) stop("The database has gone away"),
    backoff=function() c(0, 0))

  expect_warning(expect_null(abdbGetQuery("db", "SELECT 1")), "Gave up on a query")
})
