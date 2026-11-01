#A cache directory of its own, cleaned up after the test
aCache <- function() {
  path <- tempfile("cache")
  dir.create(path)
  return(path)
}

#What a source answered about a recording, as responseHeaders() gives it. The
#answer most sources give is 200 and nothing else of any use.
anAnswer <- function(status=200L, type=NA_character_, length=NA_real_) {
  return(list(status=status, type=type, length=length))
}

#Runs code with downloading mocked, returning what was asked for and where it
#was put, and writing the given contents wherever it was asked to download to.
#The source is mocked as well as the download: nothing here reaches the network.
mockDownloads <- function(code, contents="audio", headers=anAnswer(), page=NA_character_) {
  asked <- list()
  local_mocked_bindings(
    fetchText=function(address, bytes=2e6) page,
    responseHeaders=function(address) headers,
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
    responseHeaders=function(address) anAnswer(),
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
  local_mocked_bindings(
    responseHeaders=function(address) anAnswer(),
    dl_file=function(file, tmp=NULL) {
      writeBin(charToRaw("half a recor"), tmp)
      FALSE
    })

  expect_identical(webFile("https://x.org/a.wav", "unp", "1", cache), NA_character_)
  #Nothing is left to be taken for the recording next time, whole or in part
  expect_length(list.files(cache, recursive=TRUE), 0)
})

test_that("wget has finished only when it says so", {
  expect_true(downloadFinished(0))
  #8 is a server that answered with an error. It was once taken for a finished
  #download, and because wget's -O opens its output file before it asks for
  #anything, that left an empty file to be kept under the recording's name: 394
  #of them in the xeno-canto cache, 90 of which the source serves today.
  expect_false(downloadFinished(8))
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

test_that("an id read as Latin-1 is still named, rather than stopping the agent", {
  #TSA:1091_Rotkopfstärling_Lautfolgen as it came over a connection speaking
  #Latin-1: the umlaut is one byte, which is not UTF-8
  latin1 <- rawToChar(as.raw(c(charToRaw("TSA:1091_Rotkopfst"), 0xE4, charToRaw("rling"))))
  Encoding(latin1) <- "UTF-8"
  expect_false(validUTF8(latin1))

  name <- safeName(latin1)
  expect_match(name, "^[A-Za-z0-9._-]+$")
  expect_identical(name, safeName(latin1))
  path <- cachePath("/cache", "TSA", latin1, "https://x.org/a.wav")
  expect_match(basename(path), "^TSA-1091_Rotkopfst-rling-[0-9a-f]{16}[.]wav$")
})

test_that("an id in UTF-8 is named as it always was", {
  #A recording already kept under its name must still be found under it
  utf8 <- "TSA:1091_Rotkopfst\u00e4rling"
  expect_match(safeName(utf8), "^TSA-1091_Rotkopfst-rling-[0-9a-f]{16}$")
})

test_that("a server that answered with an error leaves nothing in the cache", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #What wget did on a 404: -O had already opened the output file, and the body
  #of the error answer was thrown away, so a file with nothing in it was all
  #that was left to be renamed into the cache under the recording's name
  local_mocked_bindings(
    responseHeaders=function(address) anAnswer(status=404L),
    dl_file=function(file, tmp=NULL) {
      writeBin(raw(), tmp)
      downloadFinished(8)
    })

  expect_identical(webFile("https://x.org/1/download", "xc", "1", cache), NA_character_)
  expect_false(file.exists(cachePath(cache, "xc", "1", "https://x.org/1/download")))
  #What is left is written down where nothing will read it as a recording
  expect_identical(list.files(cache, recursive=TRUE), file.path("xc", "1.gone"))
  expect_match(readLines(file.path(cache, "xc", "1.gone")), "^[0-9-]{10}T[0-9:]{8}Z 404 ")
})

test_that("an empty answer is not kept even where the download says it finished", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #Whatever a downloader makes of an answer, nothing any source ever sent is a
  #recording of no length
  local_mocked_bindings(
    responseHeaders=function(address) anAnswer(),
    dl_file=function(file, tmp=NULL) {
      writeBin(raw(), tmp)
      TRUE
    })

  expect_identical(webFile("https://x.org/a.wav", "unp", "1", cache), NA_character_)
  expect_false(file.exists(cachePath(cache, "unp", "1", "https://x.org/a.wav")))
})

test_that("a page the source sent in place of a recording is not kept as one", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #The answers that left the 394 empty files were 15 kB pages of HTML saying
  #the recording was not there, which a downloader that keeps the body of an
  #error answer writes out in full
  page <- paste0("<!DOCTYPE html>\n<html><body>", strrep("not here. ", 1500), "</body></html>")
  got <- mockDownloads(webFile("https://x.org/2/download", "xc", "2", cache),
                       contents=page, headers=anAnswer(type="text/html; charset=UTF-8"))

  expect_identical(got$value, NA_character_)
  expect_identical(list.files(cache, recursive=TRUE), file.path("xc", "2.gone"))
  expect_match(readLines(file.path(cache, "xc", "2.gone")), "text/html", fixed=TRUE)
})

test_that("a recording that came down short is not kept, and not written off", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  got <- mockDownloads(webFile("https://x.org/a.mp3", "unp", "1", cache),
                       headers=anAnswer(length=5000))

  expect_identical(got$value, NA_character_)
  expect_false(file.exists(cachePath(cache, "unp", "1", "https://x.org/a.mp3")))
  #A connection that gave out part way through says nothing about whether the
  #source has the recording, so nothing is written down about it
  expect_length(list.files(cache, recursive=TRUE), 0)
})

test_that("a recording the cache holds nothing of is fetched again", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #The 394: an empty file that file.exists() was perfectly happy with, and so
  #was given back as the recording ever after
  path <- cachePath(cache, "xc", "1", "https://x.org/a.mp3")
  dir.create(dirname(path), recursive=TRUE)
  writeBin(raw(), path)

  got <- mockDownloads(webFile("https://x.org/a.mp3", "xc", "1", cache))
  expect_length(got$asked, 1)
  expect_identical(got$value, path)
  expect_gt(file.size(path), 0)
})

test_that("a recording kept under a bare id is found there and not fetched again", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #324 files in the cache are named by a bare numeric id, for want of a type in
  #the database to name them by. They are ordinary audio, they are what
  #audioBlastBirdNET names them too, and they must go on being found.
  path <- cachePath(cache, "xeno-canto", "1", "https://xeno-canto.org/1/download")
  expect_identical(basename(path), "1")
  dir.create(dirname(path), recursive=TRUE)
  writeBin(charToRaw("audio"), path)

  got <- mockDownloads(webFile("https://xeno-canto.org/1/download", "xeno-canto", "1", cache))
  expect_length(got$asked, 0)
  expect_identical(got$value, path)
})

test_that("a recording the database has no type for is named by what the source says", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #audioBlastIngest records the type from one HEAD request and leaves it empty
  #where that request failed, and a /download address has no extension to fall
  #back on. The source says plainly enough what it is sending.
  got <- mockDownloads(webFile("https://osf.io/abcde/download", "osf", "abcde", cache),
                       headers=anAnswer(type="audio/mpeg"))
  expect_identical(basename(got$value), "abcde.mp3")
  expect_true(file.exists(got$value))

  #Where the database does have a type that is what names it: the source is not
  #asked to overrule what is already known
  also <- mockDownloads(webFile("https://osf.io/fghij/download", "osf", "fghij", cache,
                                type="audio/x-wav"),
                        headers=anAnswer(type="audio/mpeg"))
  expect_identical(basename(also$value), "fghij.wav")
})

test_that("an empty file is cleared away when the recording is kept under another name", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #An empty file left by an error, under the bare id a missing type named it by.
  #Once the source has said what it is sending the recording is kept under a
  #name with an extension, and the empty one must not be left behind where
  #anything downstream would read it as a recording of silence.
  stub <- cachePath(cache, "xc", "1", "https://xeno-canto.org/1/download")
  dir.create(dirname(stub), recursive=TRUE)
  writeBin(raw(), stub)

  got <- mockDownloads(webFile("https://xeno-canto.org/1/download", "xc", "1", cache),
                       headers=anAnswer(type="audio/mpeg"))
  expect_identical(basename(got$value), "1.mp3")
  expect_false(file.exists(stub))
  expect_identical(list.files(cache, recursive=TRUE), file.path("xc", "1.mp3"))
})

test_that("a recording written off as absent is still asked for again", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #Of the 394 recordings whose cached files were empty, 90 are served perfectly
  #today. What is written down is a note and not a verdict.
  recordGone(cache, "xc", "1", "https://x.org/1/download", 404L, "the source has not got it")
  expect_true(file.exists(goneMarker(cache, "xc", "1")))

  got <- mockDownloads(webFile("https://x.org/a.mp3", "xc", "1", cache))
  expect_length(got$asked, 1)
  expect_true(file.exists(got$value))
})

test_that("what is written down about an absent recording cannot be read as audio", {
  #An empty file was what used to say this, and read as audio an empty file is a
  #recording of silence
  marker <- goneMarker("/cache", "xc", "1")
  expect_identical(marker, file.path("/cache", "xc", "1.gone"))
  #So nothing looking for the recording's own file can land on it, named by its
  #address, by its type, or by neither
  expect_false(identical(marker, cachePath("/cache", "xc", "1",
                                           "https://xeno-canto.org/1/download")))
  expect_false(identical(marker, cachePath("/cache", "xc", "1",
                                           "https://xeno-canto.org/1/download", "audio/mpeg")))
  expect_false(identical(marker, cachePath("/cache", "xc", "1", "https://x.org/a.mp3")))
})

test_that("a xeno-canto recording is fetched from the file's own address", {
  #81 of the 394 answer 500 at /download on every attempt, while the file
  #itself answers 200 with correct audio, so the record page is read for the
  #address the recording is really at
  local_mocked_bindings(fetchText=function(address, bytes=2e6)
    '<a href="/sounds/uploaded/ABCDEFGHIJ/XC1-a%20bird.mp3">download</a>')

  expect_identical(audioAddress("https://xeno-canto.org/1/download"),
                   "https://xeno-canto.org/sounds/uploaded/ABCDEFGHIJ/XC1-a%20bird.mp3")
  #Which gives the recording a name, where /download gave it none at all
  expect_identical(cachePath("/cache", "xeno-canto", "1",
                             audioAddress("https://xeno-canto.org/1/download")),
                   file.path("/cache", "xeno-canto", "1.mp3"))
})

test_that("a record page with no audio on it leaves the address alone", {
  #40 of the 394 have a live record page that links no audio file at all, and
  #there is nothing to be fetched from anywhere
  local_mocked_bindings(fetchText=function(address, bytes=2e6)
    '<p>No recording here</p><img src="/static/logo.png">')
  expect_identical(audioAddress("https://xeno-canto.org/500000/download"),
                   "https://xeno-canto.org/500000/download")
})

test_that("a record page that could not be read leaves the address alone", {
  local_mocked_bindings(fetchText=function(address, bytes=2e6) NA_character_)
  expect_identical(audioAddress("https://xeno-canto.org/1/download"),
                   "https://xeno-canto.org/1/download")
})

test_that("only a xeno-canto download address is looked up", {
  #This is one source's broken endpoint, not something to do to every source,
  #and nothing but a recording's id is ever sent to xeno-canto to be looked up
  local_mocked_bindings(fetchText=function(address, bytes=2e6)
    stop("no page should have been read"))
  for (url in c("https://bio.acousti.ca/sites/default/files/a.wav",
                "https://xeno-canto.org/1",
                "https://xeno-canto.org/1/download/extra",
                "https://example.org/1/download")) {
    expect_identical(audioAddress(url), url)
  }

  expect_identical(xenoCantoDownloadId("https://xeno-canto.org/900000/download"), "900000")
  expect_identical(xenoCantoDownloadId("http://www.xeno-canto.org/900000/download/"), "900000")
  expect_true(is.na(xenoCantoDownloadId("https://xeno-canto.org/abc/download")))
  expect_true(is.na(xenoCantoDownloadId("https://notxeno-canto.org/1/download")))
  expect_true(is.na(xenoCantoDownloadId(NA_character_)))
})

test_that("one of several files a record page links is the one that was asked for", {
  #XC900000 links the WAV it was uploaded as and the MP3 made from that. What is
  #wanted is the recording the broken endpoint should have served.
  files <- c("https://xeno-canto.org/sounds/uploaded/T/XC900000.wav",
             "https://xeno-canto.org/sounds/uploaded/T/XC900000.mp3")
  expect_identical(chooseUpload(files, "audio/x-wav"), files[1])
  #Failing a type to go on, the MP3: it is what /download serves, and what 77
  #of the 81 recovered this way turned out to be
  expect_identical(chooseUpload(files, NA_character_), files[2])
  expect_identical(chooseUpload(rev(files), NA_character_), files[2])
  #Failing that, whatever the page gives first
  expect_identical(chooseUpload(files[1], NA_character_), files[1])
})

test_that("a record page's spectrograms are not taken for its recording", {
  page <- paste0('<img src="/sounds/uploaded/T/ffts/XC1-small.png">',
                 '<a href="/sounds/uploaded/T/XC1.mp3">')
  expect_identical(uploadedFiles(page), "https://xeno-canto.org/sounds/uploaded/T/XC1.mp3")
  expect_length(uploadedFiles('<img src="/sounds/uploaded/T/ffts/XC1.png">'), 0)
  expect_length(uploadedFiles("<p>nothing here</p>"), 0)
  expect_length(uploadedFiles(NA_character_), 0)
})

test_that("what the source sent is looked at before it is kept", {
  path <- tempfile()
  on.exit(unlink(path))

  expect_identical(notRecording(file.path(tempdir(), "never-written-at-all")),
                   "nothing was written")
  writeBin(raw(), path)
  expect_identical(notRecording(path), "nothing was sent")
  #An error page, whether the source labels it as one or not
  writeBin(charToRaw("<!DOCTYPE html><html>not here</html>"), path)
  expect_match(notRecording(path), "document")
  expect_match(notRecording(path, "text/html; charset=UTF-8"), "text/html", fixed=TRUE)
  writeBin(charToRaw('{"error":"not found"}'), path)
  expect_match(notRecording(path, "application/json"), "application/json", fixed=TRUE)
  #A recording cut off part way through still begins with everything that says
  #it is a recording, so its length is looked at first
  writeBin(charToRaw("ID3 and not much else"), path)
  expect_match(notRecording(path, "audio/mpeg", 500000), "bytes came of the 500000")
  #And a recording that is the length it was said to be is the recording
  expect_identical(notRecording(path, "audio/mpeg", 21), "")
  expect_identical(notRecording(path, "audio/mpeg"), "")
})

test_that("a recording in a format with no mark this package knows is still kept", {
  #Insisting on ID3, RIFF, fLaC or OggS would throw away m4a, AAC and WavPack,
  #and insisting on audio/* would throw away xeno-canto's WAV files, which the
  #live server sends as application/octet-stream
  path <- tempfile()
  on.exit(unlink(path))

  writeBin(c(as.raw(c(0x00, 0x00, 0x00, 0x18)), charToRaw("ftypM4A ")), path)
  expect_identical(notRecording(path, "application/octet-stream"), "")
  writeBin(charToRaw("RIFFxxxxWAVEfmt "), path)
  expect_identical(notRecording(path, "application/octet-stream"), "")
  #An MP3 carrying no tag begins with a frame sync and nothing else
  writeBin(as.raw(c(0xFF, 0xFB, 0x90, 0x00)), path)
  expect_identical(notRecording(path), "")

  expect_false(documentType("application/octet-stream"))
  expect_false(documentType("audio/mpeg"))
  expect_false(documentType(NA_character_))
  expect_false(documentType(""))
  expect_true(documentType("text/html"))
  expect_true(documentType("TEXT/HTML; charset=UTF-8"))
  expect_true(documentType("application/json"))
})

test_that("downloads that were never finished are cleared away", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  dir.create(file.path(cache, "xc"))
  #An agent stopped between the download and the rename leaves its file behind,
  #and the pid that wrote it is the only one that ever unlinks it
  stale <- file.path(cache, "xc", "1.mp3.1234.part")
  fresh <- file.path(cache, "xc", "2.mp3.5678.part")
  recording <- file.path(cache, "xc", "1.mp3")
  for (path in c(stale, fresh, recording)) writeBin(charToRaw("audio"), path)
  Sys.setFileTime(stale, Sys.time() - 7 * 86400)

  expect_identical(basename(sweepParts(cache)), basename(stale))
  expect_false(file.exists(stale))
  #One written to a moment ago may be an agent at work, and the recording
  #itself is no business of this
  expect_true(file.exists(fresh))
  expect_true(file.exists(recording))
})

test_that("a cache can be repaired, whatever it is holding", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #The reason for repairing a cache is that what is in it cannot be trusted, so
  #what is in it is not consulted
  path <- cachePath(cache, "xc", "1", "https://x.org/a.mp3")
  dir.create(dirname(path), recursive=TRUE)
  writeBin(charToRaw("not the recording"), path)
  stale <- file.path(cache, "xc", "1.mp3.1234.part")
  writeBin(charToRaw("half"), stale)
  Sys.setFileTime(stale, Sys.time() - 7 * 86400)

  recordings <- data.frame(source="xc", id="1", file="https://x.org/a.mp3",
                           type="audio/mpeg", stringsAsFactors=FALSE)
  got <- mockDownloads(repairCache(recordings, cache))

  expect_length(got$asked, 1)
  expect_identical(got$value, c("xc 1"=path))
  expect_identical(readLines(path, warn=FALSE), "audio")
  #And what was left of a download nobody came back for is cleared away with it
  expect_false(file.exists(stale))
})

test_that("what a source says is read from its last answer, not its first", {
  #A redirect is followed and the headers of every answer come back together, of
  #which only the last describes what would be fetched
  headers <- c("HTTP/2 302", "content-type: text/html", "location: /b.mp3", "",
               "HTTP/2 200", "content-type: audio/mpeg", "content-length: 155756", "")
  expect_identical(headerValue(headers, "content-type"), "audio/mpeg")
  expect_identical(headerValue(headers, "content-length"), "155756")
  expect_true(is.na(headerValue(headers, "content-disposition")))
  #Over HTTP/2 the names come down in lower case; over HTTP/1.1 they do not
  expect_identical(headerValue("Content-Type: audio/wav", "content-type"), "audio/wav")
})

test_that("only a source saying it has not got a recording is taken for an answer", {
  for (status in list(400L, 403L, 404L, 410L)) expect_true(sourceHasNot(status))
  #/download answers 500 for 81 recordings whose files are sitting on the
  #server, so a 5xx is a server having trouble and says nothing at all about
  #whether a recording exists
  for (status in list(500L, 502L, 503L, 200L, 302L, NA, NULL, "x")) {
    expect_false(sourceHasNot(status))
  }
})

test_that("a source that cannot be asked is not thereby a source with nothing", {
  #A question that failed buys nothing and settles nothing: whether a recording
  #can be had is settled by fetching it and by nothing else
  answer <- responseHeaders("")
  expect_true(is.na(answer$status))
  expect_true(is.na(answer$type))
  expect_true(is.na(answer$length))
})

test_that("a source that refused the question describes nothing but its refusal", {
  #A HEAD refused with 403 or 405, where the GET is served perfectly well, comes
  #with the type and length of the page it was refused with. Taken for the
  #recording's own, a recording in a format with no mark this package knows
  #would be thrown away for being text/html.
  refused <- structure(c("HTTP/2 403", "content-type: text/html", "content-length: 512", ""),
                       status=403L)
  answer <- answerAbout(refused)
  expect_identical(answer$status, 403L)
  expect_true(is.na(answer$type))
  expect_true(is.na(answer$length))

  #An answer that was about the recording is read in full
  served <- structure(c("HTTP/2 200", "content-type: audio/mpeg", "content-length: 155756", ""),
                      status=200L)
  expect_identical(answerAbout(served), list(status=200L, type="audio/mpeg", length=155756))
})

test_that("a recording is kept although the source refused to say what it is", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #A recording in a format whose first bytes this package does not know, from a
  #source that refused to say anything about it. It is still the recording.
  got <- mockDownloads(webFile("https://x.org/a.m4a", "unp", "1", cache),
                       contents="no mark this package knows",
                       headers=anAnswer(status=403L))

  expect_identical(got$value, cachePath(cache, "unp", "1", "https://x.org/a.m4a"))
  expect_true(file.exists(got$value))
  expect_length(list.files(cache, recursive=TRUE, pattern="[.]gone$"), 0)
})

test_that("a recording kept under a name the database cannot work out is still found", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #A /download address says nothing about the format and neither does a type the
  #database never recorded, so nothing about the recording says whether it was
  #kept as 1, 1.mp3 or 1.wav. It was kept as one of them, and the source is not
  #to be asked again to find out which.
  local_mocked_bindings(
    fetchText=function(address, bytes=2e6) stop("the source should not have been asked"),
    responseHeaders=function(address) stop("the source should not have been asked"),
    dl_file=function(file, tmp=NULL) stop("nothing should have been downloaded"))

  for (name in c("1", "1.mp3", "1.wav", "1.flac", "1.m4a", "1.wv", "1.mov")) {
    dir.create(file.path(cache, "xeno-canto"), recursive=TRUE, showWarnings=FALSE)
    writeBin(charToRaw("audio"), file.path(cache, "xeno-canto", name))
    expect_identical(webFile("https://xeno-canto.org/1/download", "xeno-canto", "1", cache),
                     file.path(cache, "xeno-canto", name))
    unlink(file.path(cache, "xeno-canto", name))
  }
})

test_that("a recording named by its address or its type is looked for in one place only", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #Where the name is known there is nothing to search for, and a file under some
  #other extension is some other recording's business
  dir.create(file.path(cache, "unp"), recursive=TRUE)
  writeBin(charToRaw("audio"), file.path(cache, "unp", "1.mp3"))

  expect_true(is.na(recordingHeld(cache, "unp", "1", "https://x.org/a.wav")))
  expect_true(is.na(recordingHeld(cache, "unp", "1", "https://x.org/1/download", "audio/x-wav")))
  expect_identical(recordingHeld(cache, "unp", "1", "https://x.org/a.mp3"),
                   file.path(cache, "unp", "1.mp3"))
  expect_identical(recordingHeld(cache, "unp", "1", "https://x.org/1/download", "audio/mpeg"),
                   file.path(cache, "unp", "1.mp3"))
  #And an empty one is not a recording under any name
  writeBin(raw(), file.path(cache, "unp", "1.wav"))
  expect_true(is.na(recordingHeld(cache, "unp", "1", "https://x.org/a.wav")))
})

test_that("an id with a dot in it is not mistaken for a name with an extension", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #bio.acousti.ca's ids are numbers, but TSA's and iNaturalist's are whatever
  #their source called them, and a dot in one says nothing about a format
  dir.create(file.path(cache, "TSA"), recursive=TRUE)
  writeBin(charToRaw("audio"), file.path(cache, "TSA", "1091.Lautfolgen.mp3"))

  expect_identical(recordingHeld(cache, "TSA", "1091.Lautfolgen",
                                 "https://x.org/1091/download"),
                   file.path(cache, "TSA", "1091.Lautfolgen.mp3"))
})

test_that("every name a recording can be kept under is one the cache can give it", {
  #recordingHeld() looks for a recording under every extension mimeExtension()
  #can give one, and the two read the same list so that they cannot come to
  #disagree
  extensions <- mimeExtensions()
  for (type in names(extensions)) {
    expect_identical(mimeExtension(type), paste0(".", extensions[[type]]))
  }
  expect_true(all(nzchar(extensions)))
  expect_identical(mimeExtension("audio/mpeg"), ".mp3")
})

test_that("an empty file left under any of a recording's names is cleared away", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #The 394 were left under whatever name the database's type gave them at the
  #time, which need not be the name the recording is kept under now
  dir.create(file.path(cache, "xeno-canto"), recursive=TRUE)
  for (name in c("1", "1.mp3", "1.wav")) {
    writeBin(raw(), file.path(cache, "xeno-canto", name))
  }
  expect_length(emptyHeld(cache, "xeno-canto", "1", "https://xeno-canto.org/1/download"), 3)

  got <- mockDownloads(webFile("https://xeno-canto.org/1/download", "xeno-canto", "1", cache),
                       headers=anAnswer(type="audio/flac"))
  expect_identical(basename(got$value), "1.flac")
  #Nothing is left that anything could read as a recording of silence
  expect_identical(list.files(cache, recursive=TRUE), file.path("xeno-canto", "1.flac"))
})

test_that("a recording is not cleared away for being under another of its names", {
  cache <- aCache()
  on.exit(unlink(cache, recursive=TRUE))
  #Only the empty ones go: a file with something in it is a recording, and is
  #found rather than cleared
  dir.create(file.path(cache, "xeno-canto"), recursive=TRUE)
  writeBin(charToRaw("audio"), file.path(cache, "xeno-canto", "1.wav"))
  writeBin(raw(), file.path(cache, "xeno-canto", "1.mp3"))

  expect_identical(emptyHeld(cache, "xeno-canto", "1", "https://xeno-canto.org/1/download"),
                   file.path(cache, "xeno-canto", "1.mp3"))
  expect_identical(recordingHeld(cache, "xeno-canto", "1", "https://xeno-canto.org/1/download"),
                   file.path(cache, "xeno-canto", "1.wav"))
})

test_that("a recording with a name of its own is looked for under that name only", {
  expect_identical(cacheNames("/cache", "unp", "1", "https://x.org/a.wav"),
                   file.path("/cache", "unp", "1.wav"))
  expect_identical(cacheNames("/cache", "unp", "1", "https://x.org/1/download", "audio/mpeg"),
                   file.path("/cache", "unp", "1.mp3"))
  #And one with no name of its own under every name the cache could give it
  names <- cacheNames("/cache", "unp", "1", "https://x.org/1/download")
  expect_identical(names[1], file.path("/cache", "unp", "1"))
  expect_true(file.path("/cache", "unp", "1.mp3") %in% names)
  expect_true(file.path("/cache", "unp", "1.wav") %in% names)
  expect_length(names, length(unique(mimeExtensions())) + 1)
})

test_that("a document is recognised by what is in front of it and nothing more", {
  #A page with whitespace or a byte order mark before it is still a page
  expect_true(documentMark(charToRaw("  \r\n<!DOCTYPE html>")))
  expect_true(documentMark(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("<html>"))))
  expect_true(documentMark(charToRaw('{"error":1}')))
  #And a recording with a space among its bytes is not a page for having one:
  #skipping over it would carry this as far as a < that began nothing
  expect_false(documentMark(c(charToRaw("RIFF "), as.raw(0x3C))))
  expect_false(documentMark(c(as.raw(c(0x00, 0x00)), charToRaw("<x"))))
  expect_false(documentMark(charToRaw("ID3")))
  expect_false(documentMark(raw()))
  expect_false(documentMark(as.raw(c(0xFF, 0xFB))))
})
