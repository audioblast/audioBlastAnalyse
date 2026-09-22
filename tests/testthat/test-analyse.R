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
