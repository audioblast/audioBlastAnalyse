#A cache directory of its own, cleaned up after the test
aCache <- function() {
  path <- tempfile("cache")
  dir.create(path)
  return(path)
}

#Runs code with downloading mocked, returning what was asked for and where it
#was put, and writing the given contents wherever it was asked to download to
mockDownloads <- function(code, contents="audio") {
  asked <- list()
  local_mocked_bindings(
    dl_file=function(file, tmp=NULL) {
      asked[[length(asked) + 1]] <<- list(url=file, path=tmp)
      writeBin(charToRaw(contents), tmp)
      TRUE
    })
  value <- code
  return(list(value=value, asked=asked))
}

test_that("a recording is kept under its source and id", {
  expect_identical(cachePath("/cache", "bio.acousti.ca", "58428", "https://x.org/a.wav"),
                   file.path("/cache", "bio.acousti.ca", "58428.wav"))
  expect_identical(cachePath("/cache", "xc", "12345", "https://xeno-canto.org/12345/download.mp3"),
                   file.path("/cache", "xc", "12345.mp3"))
})

test_that("an id a file system will not take is still named, and named only once", {
  #audioBlast! ids are whatever their source called them, and a slash, a colon
  #or a quote is no part of a file name
  for (id in list("a/b", "a:b", "C:\\x", "a\"b", "..", ".", "", NA_character_,
                  strrep("x", 300), "CON", "nul.wav")) {
    name <- safeName(id)
    expect_match(name, "^[A-Za-z0-9._-]+$")
    expect_lt(nchar(name), 130)
    expect_false(grepl("^[.]+$", name))
    #Naming it twice names it the same
    expect_identical(name, safeName(id))
  }
})

test_that("two ids never come to one name", {
  expect_false(identical(safeName("a/b"), safeName("a-b")))
  expect_false(identical(safeName("a:b"), safeName("a/b")))
  expect_false(identical(safeName(strrep("x", 300)), safeName(strrep("x", 301))))
})

test_that("an id that is already a name is left as it is", {
  expect_identical(safeName("58428"), "58428")
  expect_identical(safeName("bio.acousti.ca"), "bio.acousti.ca")
  expect_identical(safeName("nhm-unp-1-1588606809"), "nhm-unp-1-1588606809")
})

test_that("a file is named by what its address points at, not by what follows", {
  expect_identical(extension("https://x.org/a.WAV"), ".wav")
  expect_identical(extension("https://x.org/a.wav?download=1"), ".wav")
  expect_identical(extension("https://x.org/a.mp3#t=1"), ".mp3")
  expect_identical(extension("https://x.org/download"), "")
  #The name a recording is served under, not one named in the query
  expect_identical(extension("https://x.org/a.php?f=1.wav"), ".php")
  expect_identical(extension("https://x.org/a.somethinglong"), "")
})

test_that("a recording is kept where it is being worked when nowhere is given", {
  expect_identical(cachePath("", "unp", "1", "a.wav"), file.path(".", "unp", "1.wav"))
})

test_that("a recording is fetched once and kept", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))

  first <- mockDownloads(webFile("https://x.org/a.wav", "bio.acousti.ca", "58428", cache))
  expect_length(first$asked, 1)
  expect_identical(first$value, cachePath(cache, "bio.acousti.ca", "58428", "a.wav"))
  expect_true(file.exists(first$value))

  #Meeting the recording again reads what is there rather than fetching it
  second <- mockDownloads(webFile("https://x.org/a.wav", "bio.acousti.ca", "58428", cache))
  expect_length(second$asked, 0)
  expect_identical(second$value, first$value)
})

test_that("a recording is downloaded beside where it is kept, and moved there whole", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #A download that has not finished must not be left where it would be read as
  #the recording
  seen <- character()
  local_mocked_bindings(
    dl_file=function(file, tmp=NULL) {
      seen <<- c(seen, tmp)
      expect_false(file.exists(cachePath(cache, "unp", "1", file)))
      writeBin(charToRaw("audio"), tmp)
      TRUE
    })

  path <- webFile("https://x.org/a.wav", "unp", "1", cache)
  expect_match(seen, "\\.part$")
  expect_true(file.exists(path))
  #Nothing half written is left behind
  expect_length(list.files(dirname(path), pattern="[.]part$"), 0)
})

test_that("recordings of two sources with one id are kept apart", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))

  mockDownloads(webFile("https://x.org/a.wav", "unp", "1", cache), contents="one")
  mockDownloads(webFile("https://y.org/b.wav", "xc", "1", cache), contents="two")

  expect_identical(readLines(cachePath(cache, "unp", "1", "a.wav"), warn=FALSE), "one")
  expect_identical(readLines(cachePath(cache, "xc", "1", "b.wav"), warn=FALSE), "two")
})

test_that("a cache says what it is doing when asked", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))

  expect_output(mockDownloads(webFile("https://x.org/a.wav", "unp", "1", cache, verbose=TRUE)),
                "Downloading: https://x.org/a.wav")
  expect_output(mockDownloads(webFile("https://x.org/a.wav", "unp", "1", cache, verbose=TRUE)),
                "Already downloaded:")
})

test_that("a download that did not finish is neither kept nor read", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #A download stopped part way through leaves what it had fetched so far
  local_mocked_bindings(dl_file=function(file, tmp=NULL) {
    writeBin(charToRaw("half a recor"), tmp)
    FALSE
  })

  expect_identical(webFile("https://x.org/a.wav", "unp", "1", cache), NA_character_)
  #Nothing is left to be taken for the recording next time, whole or in part
  expect_length(list.files(cache, recursive=TRUE), 0)
})

test_that("wget has finished only when it says so, or the server said no", {
  expect_true(downloadFinished(0))
  #An error from the server is all there is to be had, and is measured as such
  expect_true(downloadFinished(8))
  #A network that failed, a file that could not be written, a wget stopped
  for (status in list(1, 3, 4, 5, 130, 2, NA, NULL, "x")) {
    expect_false(downloadFinished(status))
  }
})

test_that("wget is asked for the address it was given, into the file it was given", {
  skip_on_os("windows")
  asked <- character()
  local_mocked_bindings(runCommand=function(command) {
    asked <<- c(asked, command)
    4L
  })
  path <- file.path(tempdir(), "a recording.part")
  expect_false(dl_file("https://x.org/a b.wav", path))
  expect_match(asked, "'https://x.org/a b.wav'", fixed=TRUE)
  expect_match(asked, shQuote(path), fixed=TRUE)
})

test_that("an address with no extension is named by its MIME type", {
  expect_identical(cachePath("/cache", "xeno-canto", "10", "https://xeno-canto.org/10/download",
                             "audio/mpeg"),
                   file.path("/cache", "xeno-canto", "10.mp3"))
  expect_identical(mimeExtension("audio/x-wav"), ".wav")
  expect_identical(mimeExtension("Audio/FLAC; charset=binary"), ".flac")
  expect_identical(mimeExtension("text/html"), "")
  expect_identical(mimeExtension(NA_character_), "")
  expect_identical(mimeExtension(character(0)), "")
  #The address's own extension is kept where it has one
  expect_identical(cachePath("/cache", "unp", "1", "https://x.org/a.flac", "audio/x-wav"),
                   file.path("/cache", "unp", "1.flac"))
  #And a file of no known type is still named, without one
  expect_identical(cachePath("/cache", "xc", "1", "https://x.org/1/download"),
                   file.path("/cache", "xc", "1"))
})

test_that("a download on Windows is the bytes that were sent, whatever its address", {
  #Only Windows downloads with download.file(), and only there does text mode
  #change what is written
  skip_if_not(.Platform$OS.type == "windows", "download.file() is used only on Windows")
  #Bytes an MP3 is full of, with nothing in the address to say it is binary
  sent <- as.raw(c(0xFF, 0xFB, 0x0A, 0x00, 0x0D, 0x0A, 0x0A, 0x41, 0x0A))
  source <- tempfile("s")
  writeBin(sent, source)
  path <- tempfile(fileext=".part")
  on.exit(unlink(c(source, path)))
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(source)))

  expect_true(dl_file(url, path))
  expect_identical(readBin(path, "raw", n=100), sent)
})
