#A ledger of its own for a test, and limits for the sources it names
withLimits <- function(limits, env=parent.frame()) {
  withr::local_options(list(audioBlastAnalyse.limits=limits,
                            audioBlastAnalyse.ledger_dir=tempfile("ledger")), .local_envir=env)
}

#Writes down downloads as though they had been made the given numbers of seconds
#before now
downloaded <- function(source, ago, bytes, now=Sys.time()) {
  for (i in seq_along(ago)) recordDownload(source, bytes[i], when=now - ago[i])
}

test_that("iNaturalist is kept well under the limits it publishes, and others are not limited", {
  withr::local_options(list(audioBlastAnalyse.limits=NULL))
  expect_identical(downloadLimits("iNaturalist"), c(hour=1e9, day=20e9))
  expect_true(downloadLimits("iNaturalist")[["hour"]] < 5e9)
  expect_true(downloadLimits("iNaturalist")[["day"]] < 24e9)
  expect_null(downloadLimits("xeno-canto"))
  expect_null(downloadLimits(NA_character_))
})

test_that("limits can be set for any source, changed, and lifted", {
  withLimits(list(TSA=c(hour=5, day=50), iNaturalist=NULL))
  expect_identical(downloadLimits("TSA"), c(hour=5, day=50))
  expect_null(downloadLimits("iNaturalist"))
})

test_that("only downloads from a source with limits are written down", {
  withLimits(list(unp=c(hour=100, day=1000)))
  recordDownload("unp", 10)
  recordDownload("xeno-canto", 10)
  recordDownload("unp", 0)
  expect_true(file.exists(ledgerPath("unp")))
  expect_false(file.exists(ledgerPath("xeno-canto")))
  expect_identical(downloadsSince("unp")$bytes, 10)
})

test_that("a source's downloads are written down in a file of its own, named safely", {
  withLimits(list(`a/b`=c(hour=100, day=1000), unp=c(hour=100, day=1000)))
  expect_false(identical(ledgerPath("a/b"), ledgerPath("unp")))
  expect_false(grepl("/", basename(ledgerPath("a/b")), fixed=TRUE))
})

test_that("downloads within the limits need no wait", {
  withLimits(list(unp=c(hour=100, day=1000)))
  expect_identical(budgetWait("unp"), 0)
  now <- Sys.time()
  downloaded("unp", c(30, 20, 10), c(30, 30, 30), now)
  expect_identical(budgetWait("unp", now), 0)
})

test_that("an hour's limit reached is waited out until enough has left the hour", {
  withLimits(list(unp=c(hour=100, day=1000)))
  now <- Sys.time()
  #60 bytes 50 minutes ago, 50 bytes 10 minutes ago: 110 in the hour. The 60
  #leave it in 10 minutes, and 50 is under the limit.
  downloaded("unp", c(3000, 600), c(60, 50), now)
  expect_equal(budgetWait("unp", now), 600, tolerance=0.01)
})

test_that("a day's limit reached is waited out until enough has left the day", {
  withLimits(list(unp=c(hour=1000, day=100)))
  now <- Sys.time()
  #Two hours apart, both within the day and neither within the hour of the other
  downloaded("unp", c(86400 - 3600, 7200), c(70, 40), now)
  expect_equal(budgetWait("unp", now), 3600, tolerance=0.01)
})

test_that("the longer of two waits is the one waited", {
  withLimits(list(unp=c(hour=50, day=100)))
  now <- Sys.time()
  downloaded("unp", c(80000, 1800), c(60, 60), now)
  #The hour frees in 30 minutes, the day only when the first download is a day old
  expect_equal(budgetWait("unp", now), 86400 - 80000, tolerance=0.01)
})

test_that("downloads older than a day are cleared from the file once they outnumber the rest", {
  withLimits(list(unp=c(hour=1e9, day=1e9)))
  now <- Sys.time()
  downloaded("unp", rep(90000, 1200), rep(1, 1200), now)
  downloaded("unp", 10, 5, now)
  expect_identical(downloadsSince("unp", now)$bytes, 5)
  expect_length(readLines(ledgerPath("unp")), 1)
})

test_that("a line that is not a download is passed over", {
  withLimits(list(unp=c(hour=100, day=1000)))
  dir.create(dirname(ledgerPath("unp")), recursive=TRUE, showWarnings=FALSE)
  writeLines(c("not,a,download", "", sprintf("%.3f,7", as.numeric(Sys.time()) - 5), "x,y"),
             ledgerPath("unp"))
  expect_identical(downloadsSince("unp")$bytes, 7)
})

test_that("an agent waits for its limits, an hour at a time at most", {
  waits <- c(5000, 120, 0)
  asked <- 0
  waited <- numeric()
  local_mocked_bindings(budgetWait=function(source, now=Sys.time()) {
                          asked <<- asked + 1
                          waits[asked]
                        },
                        pause=function(seconds) waited <<- c(waited, seconds))
  waitForBudget("unp")
  expect_identical(waited, c(3601, 121))
})

test_that("a download is written down against its source, whether or not it was a recording", {
  withLimits(list(unp=c(hour=1e9, day=1e9)))
  cache <- tempfile("cache")
  dir.create(cache)
  local_mocked_bindings(
    responseHeaders=function(address) list(status=200L, type=NA_character_, length=NA_real_),
    dl_file=function(file, tmp=NULL) {
      writeBin(as.raw(c(0x52, 0x49, 0x46, 0x46, rep(0, 96))), tmp)
      TRUE
    })
  expect_false(is.na(webFile("https://x.org/a.wav", "unp", "1", cache)))
  #Found in the cache the second time, and not downloaded or counted again
  expect_false(is.na(webFile("https://x.org/a.wav", "unp", "1", cache)))
  expect_identical(downloadsSince("unp")$bytes, 100)

  #A page sent in place of a recording counts too: the source sent it
  local_mocked_bindings(
    dl_file=function(file, tmp=NULL) {
      writeBin(charToRaw("<html>not here</html>"), tmp)
      TRUE
    })
  expect_true(is.na(webFile("https://x.org/b.wav", "unp", "2", cache)))
  expect_identical(downloadsSince("unp")$bytes, c(100, 21))
})

test_that("a source is taken to be refusing only where it will not serve what it served", {
  withLimits(list(unp=c(hour=1e9, day=1e9)))
  local_mocked_bindings(responseHeaders=function(address) {
    list(status=if (grepl("good", address)) 200L else 403L, type=NA_character_, length=NA_real_)
  })
  expect_false(refusing("unp", c(unp="https://x.org/good.wav")))
  expect_true(refusing("unp", c(unp="https://x.org/refused.wav")))
  #Nothing downloaded from it yet: nothing to tell by
  expect_false(refusing("unp", character()))
  expect_false(refusing("unp", c(other="https://x.org/refused.wav")))
  #A source with no limits is never asked
  expect_false(refusing("xeno-canto", c(`xeno-canto`="https://x.org/refused.wav")))
})

test_that("waits for a refusing source grow, are spread, and are capped", {
  set.seed(1)
  waits <- vapply(1:8, refusalWait, numeric(1))
  expected <- pmin(7200, 300 * 2^(0:7))
  expect_true(all(waits >= expected * 0.5 & waits <= expected * 1.5))
  expect_true(refusalWait(0) <= 450)
})

#A connection as far as analyse() is concerned (as in test-analyse.R)
aConnection <- function() {
  return(structure(list(), class="MariaDBConnection"))
}

#The statements a mocked database was asked to execute that begin as given
statementsLike <- function(mocked, start) {
  return(Filter(function(s) startsWith(s$sql, start), mocked$executed))
}

#How many times an agent asked the database for a recording's work
claimsByFile <- function(mocked) {
  return(length(statementsLike(mocked, "CALL `claim-tasks-by-file`")))
}

#A recording claimed over the web, in the shape heldBy() answers in
aWebTask <- function(id) {
  return(someRows(source="unp", id=id, file=paste0("https://x.org/", id, ".wav"),
                  type="audio/x-wav", Duration=60, task="recordings_calculated",
                  process="p", rec_key=1L))
}

test_that("a source that stops serving the agent has its recordings given back, not written off", {
  withLimits(list(unp=c(hour=1e12, day=1e12)))
  waited <- numeric()
  done <- character()
  local_mocked_bindings(
    pause=function(seconds) waited <<- c(waited, seconds),
    doTask=function(db, task, source, id, ...) {
      done <<- c(done, id)
      invisible("measured")
    },
    webFile=function(url, ...) if (grepl("/1[.]wav$", url)) tempfile() else NA_character_,
    responseHeaders=function(address) list(status=403L, type=NA_character_, length=NA_real_),
    downloadAttempts=function() 3)
  #Recording 1 comes down; recording 2 then fails three times, and so would the
  #one that came down before it
  mocked <- suppressWarnings(
    mockDB(analyse(aConnection(), mode="web", source="unp"),
           rows=list(aWebTask("1"), noRows(), aWebTask("2"), noRows(), aWebTask("2"),
                     noRows(), aWebTask("2"), noRows())))

  #Given back three times, and never measured at its address as missing
  expect_length(statementsLike(mocked, "DELETE FROM `tasks-progress`"), 3)
  expect_identical(done, "1")
  #A wait of minutes for the source, not seconds for the recording
  expect_true(any(waited >= 150))
})

test_that("a recording a serving source has not got is still written off as missing", {
  withLimits(list(unp=c(hour=1e12, day=1e12)))
  local_mocked_bindings(
    pause=function(seconds) NULL,
    webFile=function(url, ...) if (grepl("/1[.]wav$", url)) tempfile() else NA_character_,
    responseHeaders=function(address) list(status=200L, type=NA_character_, length=NA_real_),
    downloadAttempts=function() 3)
  tasks <- character()
  local_mocked_bindings(doTask=function(db, task, source, id, path, ...) {
    tasks <<- c(tasks, paste(id, path))
    invisible("measured")
  })
  mocked <- suppressWarnings(
    mockDB(analyse(aConnection(), mode="web", source="unp"),
           rows=list(aWebTask("1"), noRows(), aWebTask("2"), noRows(), aWebTask("2"),
                     noRows(), aWebTask("2"), noRows())))

  #Given back twice, then measured at its address, as before
  expect_length(statementsLike(mocked, "DELETE FROM `tasks-progress`"), 2)
  expect_identical(tasks[2], "2 https://x.org/2.wav")
})

test_that("an agent does not ask for work while a source's limits are reached", {
  withLimits(list(unp=c(hour=1e12, day=1e12)))
  order <- character()
  local_mocked_bindings(
    pause=function(seconds) NULL,
    waitForBudget=function(source, verbose=FALSE) order <<- c(order, paste("wait", source)))
  mocked <- mockDB(analyse(aConnection(), mode="web", source="unp"))
  #A wait before every claim, the first of them before anything is claimed
  expect_identical(order[1], "wait unp")
  expect_identical(length(order), claimsByFile(mocked))
})
