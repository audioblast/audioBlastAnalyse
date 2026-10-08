#Waveform peaks are made by audiowaveform, which most machines the tests run on
#do not have, so it is stood in for here by something that writes what it
#would; a test against audiowaveform itself runs where it is installed.

#Peaks as audiowaveform writes them: one channel, two values for each point
somePeaks <- function(length=3, channels=1) {
  return(paste0('{"version":2,"channels":', channels,
                ',"sample_rate":44100,"samples_per_pixel":512,"bits":8,"length":', length,
                ',"data":[', paste(rep(c(-1, 1), length * channels), collapse=","), ']}'))
}

#audiowaveform, stood in for: it writes the given peaks, and says whether it did
fakeAudiowaveform <- function(peaks=somePeaks(), works=TRUE) {
  calls <- list()
  run <- function(input, format, out) {
    calls[[length(calls) + 1]] <<- list(input=input, format=format, out=out)
    if (works) writeLines(peaks, out)
    return(works)
  }
  return(list(run=run, calls=function() calls))
}

test_that("an agent asks for waveform peaks only where it can make them", {
  local_mocked_bindings(hasAudiowaveform=function() FALSE)
  expect_identical(tasksDone(), "recordings_calculated")
  local_mocked_bindings(hasAudiowaveform=function() TRUE)
  expect_identical(tasksDone(), c("recordings_calculated", "waveform_peaks"))
})

test_that("audiowaveform is looked for where AUDIOWAVEFORM says, or on the path", {
  withr::local_envvar(AUDIOWAVEFORM=NA)
  expect_identical(audiowaveformCommand(), "audiowaveform")
  withr::local_envvar(AUDIOWAVEFORM="/opt/bin/audiowaveform")
  expect_identical(audiowaveformCommand(), "/opt/bin/audiowaveform")
  expect_false(hasAudiowaveform())
})

#The first page of an Ogg stream, holding the given first packet of its codec
#behind a table of the given segment lengths: all a reader looks at to know
#the codec, written by hand as av may have no encoder for it
anOggPage <- function(packet, segments=length(packet)) {
  path <- tempfile(fileext=".ogg")
  header <- c(charToRaw("OggS"), as.raw(0), as.raw(2), rep(as.raw(0), 20),
              as.raw(length(segments)), as.raw(segments))
  writeBin(c(header, packet), path)
  return(path)
}

test_that("a WAV is known by its header, whatever its size", {
  wav <- aWave()
  on.exit(unlink(wav))
  expect_identical(audioFormat(wav), "wav")
  #An RF64 file is laid out as a WAV is
  withBytes(wav, 1, charToRaw("RF64"))
  expect_identical(audioFormat(wav), "wav")
})

test_that("a FLAC file is known by its header", {
  flac <- aFlac()
  on.exit(unlink(flac))
  expect_identical(audioFormat(flac), "flac")
})

test_that("an MP3 is known by its header", {
  mp3 <- aConverted(".mp3")
  on.exit(unlink(mp3))
  expect_identical(audioFormat(mp3), "mp3")
})

test_that("an MP3 is known by its frames when it has no ID3 tag", {
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(as.raw(c(0xFF, 0xFB, 0x90, 0x64, rep(0, 60))), path)
  expect_identical(audioFormat(path), "mp3")
})

test_that("an Ogg file is known by the codec its first packet starts", {
  vorbis <- anOggPage(c(as.raw(1), charToRaw("vorbis"), as.raw(rep(0, 23))))
  opus <- anOggPage(c(charToRaw("OpusHead"), as.raw(rep(0, 11))))
  #FLAC in Ogg is not read by audiowaveform, so it is converted first
  flac <- anOggPage(c(as.raw(0x7F), charToRaw("FLAC"), as.raw(rep(0, 46))))
  #A table of more than one segment puts the packet further on
  long <- anOggPage(c(charToRaw("OpusHead"), as.raw(rep(0, 11))), segments=c(8, 11))
  on.exit(unlink(c(vorbis, opus, flac, long)))

  expect_identical(audioFormat(vorbis), "ogg")
  expect_identical(audioFormat(opus), "opus")
  expect_true(is.na(audioFormat(flac)))
  expect_identical(audioFormat(long), "opus")
})

test_that("a file audiowaveform does not read has no format to read it as", {
  aiff <- aConverted(".aiff")
  on.exit(unlink(aiff))
  expect_true(is.na(audioFormat(aiff)))
})

test_that("what is not audio, or is not there, has no format", {
  notAudio <- aFile()
  on.exit(unlink(notAudio))
  expect_true(is.na(audioFormat(notAudio)))
  expect_true(is.na(audioFormat(tempfile())))
})

test_that("peaks are only peaks when there are as many values as they say", {
  path <- tempfile(fileext=".json")
  on.exit(unlink(path))
  writeLines(somePeaks(length=3), path)
  expect_true(peaksValid(path))
  writeLines(somePeaks(length=3, channels=2), path)
  expect_true(peaksValid(path))
  writeLines(sub('"length":3', '"length":4', somePeaks(length=3)), path)
  expect_false(peaksValid(path))
  writeLines(substr(somePeaks(), 1, 60), path)
  expect_false(peaksValid(path))
  writeLines(sub('"length":3', '"length":0', somePeaks(length=0)), path)
  expect_false(peaksValid(path))
  expect_false(peaksValid(tempfile()))
})

test_that("audiowaveform is asked for 86 points a second at 8 bits, mixed to one channel", {
  #Read from the arguments it is given, as running it is stood in for
  expect_identical(peaksPerSecond(), 86)
  expect_identical(peaksBits(), 8)
})

test_that("a file audiowaveform reads is given it as it is", {
  fake <- fakeAudiowaveform()
  local_mocked_bindings(runAudiowaveform=fake$run)
  wav <- aWave()
  out <- tempfile(fileext=".json")
  on.exit(unlink(c(wav, out)))

  expect_true(peaksFile(wav, out))
  expect_identical(fake$calls()[[1]]$input, wav)
  expect_identical(fake$calls()[[1]]$format, "wav")
})

test_that("a file audiowaveform does not read is converted to WAV first", {
  fake <- fakeAudiowaveform()
  local_mocked_bindings(runAudiowaveform=fake$run)
  aiff <- aConverted(".aiff")
  out <- tempfile(fileext=".json")
  on.exit(unlink(c(aiff, out)))

  expect_true(peaksFile(aiff, out))
  call <- fake$calls()[[1]]
  expect_identical(call$format, "wav")
  expect_false(identical(call$input, aiff))
  #The conversion is not left lying about
  expect_false(file.exists(call$input))
})

test_that("no peaks are made of what is not audio", {
  fake <- fakeAudiowaveform()
  local_mocked_bindings(runAudiowaveform=fake$run)
  notAudio <- aFile()
  out <- tempfile(fileext=".json")
  on.exit(unlink(c(notAudio, out)))

  expect_false(peaksFile(notAudio, out))
  expect_length(fake$calls(), 0)
})

test_that("peaks that audiowaveform did not finish are not peaks", {
  local_mocked_bindings(runAudiowaveform=fakeAudiowaveform(works=FALSE)$run)
  wav <- aWave()
  out <- tempfile(fileext=".json")
  on.exit(unlink(c(wav, out)))
  expect_false(peaksFile(wav, out))

  local_mocked_bindings(runAudiowaveform=fakeAudiowaveform(peaks="{\"version\":2")$run)
  expect_false(peaksFile(wav, out))
})

test_that("peaks are put under their source and id, and their address given", {
  dir <- tempfile("served")
  peaks <- tempfile(fileext=".json")
  writeLines(somePeaks(), peaks)
  on.exit(unlink(c(dir, peaks), recursive=TRUE))

  url <- publishPeaks(peaks, "bio.acousti.ca", "10753", dir=dir, rsync="",
                      base="https://files.audioblast.org/")
  expect_identical(url, "https://files.audioblast.org/peaks/bio.acousti.ca/10753.json")
  expect_identical(readLines(file.path(dir, "peaks", "bio.acousti.ca", "10753.json")), somePeaks())
  #Nothing half written is left beside it
  expect_identical(list.files(file.path(dir, "peaks", "bio.acousti.ca")), "10753.json")

  #A base without its slash is given one, and peaks made again replace the old
  writeLines(somePeaks(length=4), peaks)
  url <- publishPeaks(peaks, "bio.acousti.ca", "10753", dir=dir, rsync="", base="https://example.org/files")
  expect_identical(url, "https://example.org/files/peaks/bio.acousti.ca/10753.json")
  expect_identical(readLines(file.path(dir, "peaks", "bio.acousti.ca", "10753.json")), somePeaks(length=4))
})

test_that("a source or id that is no name for a file is named as the download cache names it", {
  dir <- tempfile("served")
  peaks <- tempfile(fileext=".json")
  writeLines(somePeaks(), peaks)
  on.exit(unlink(c(dir, peaks), recursive=TRUE))

  url <- publishPeaks(peaks, "xc", "a/b c", dir=dir, rsync="", base="https://files.audioblast.org/")
  expect_identical(url, paste0("https://files.audioblast.org/peaks/xc/", safeName("a/b c"), ".json"))
  #Which is safe in an address as it is
  expect_match(url, "^https://[A-Za-z0-9./_-]+$")
  expect_true(file.exists(file.path(dir, "peaks", "xc", paste0(safeName("a/b c"), ".json"))))
})

test_that("peaks are sent by rsync where there is no directory to put them in", {
  sent <- NULL
  local_mocked_bindings(runRsync=function(from, to) {
    sent <<- list(from=from, to=to, there=file.exists(from), peaks=readLines(from))
    TRUE
  })
  peaks <- tempfile(fileext=".json")
  writeLines(somePeaks(), peaks)
  on.exit(unlink(peaks))

  url <- publishPeaks(peaks, "bio.acousti.ca", "10753", dir="", rsync="peaks@files:/srv/files/",
                      base="https://files.audioblast.org/")
  expect_identical(url, "https://files.audioblast.org/peaks/bio.acousti.ca/10753.json")
  expect_identical(sent$to, "peaks@files:/srv/files/")
  #Named as it is to be named there from the /./ on
  expect_match(sent$from, "/\\./peaks/bio\\.acousti\\.ca/10753\\.json$")
  expect_true(sent$there)
  expect_identical(sent$peaks, somePeaks())
  #The staged copy is cleared away
  expect_false(file.exists(sent$from))
})

test_that("peaks that could not be sent have no address", {
  local_mocked_bindings(runRsync=function(from, to) FALSE)
  peaks <- tempfile(fileext=".json")
  writeLines(somePeaks(), peaks)
  on.exit(unlink(peaks))

  expect_true(is.na(publishPeaks(peaks, "unp", "1", dir="", rsync="peaks@files:/srv/files/")))
  expect_warning(expect_true(is.na(publishPeaks(peaks, "unp", "1", dir="", rsync=""))),
                 "nowhere to go")
})

test_that("where peaks are is written over where they were, bound not written in", {
  mocked <- mockDB(writePeaks("db", "o'brien", "1", "https://files.audioblast.org/peaks/o-brien/1.json"))
  statement <- onlyStatement(mocked)

  #Beside the recording's measurements, in a row of its own where there are none
  expect_match(statement$sql, "^INSERT INTO `recordings-calculated` \\(`source`, `id`, `peaks_url`\\)")
  expect_match(statement$sql, "ON DUPLICATE KEY UPDATE `peaks_url` = VALUES\\(`peaks_url`\\);$")
  expect_false(grepl("o'brien", statement$sql, fixed=TRUE))
  expect_identical(statement$params,
                   list("o'brien", "1", "https://files.audioblast.org/peaks/o-brien/1.json"))
})

test_that("a recording's peaks are looked up by its source and id", {
  mocked <- mockDB(peaksURL("db", "unp", "1"), rows=someRows(peaks_url="https://example.org/1.json"))
  expect_identical(mocked$value, "https://example.org/1.json")
  expect_match(mocked$queried, "SELECT `peaks_url` FROM `recordings-calculated` WHERE `source` = \\? AND `id` = \\?;")

  expect_true(is.na(mockDB(peaksURL("db", "unp", "1"))$value))
  expect_true(is.na(mockDB(peaksURL("db", "unp", "1"), rows=someRows(peaks_url=""))$value))
  expect_true(is.na(mockDB(peaksURL("db", "unp", "1"), rows=someRows(peaks_url=NA_character_))$value))
})

test_that("deleting all of a recording's analyses forgets where its peaks are", {
  mocked <- mockDB(deleteAllAnalyses("db", "unp", "1", justR=FALSE))
  cleared <- Filter(function(s) startsWith(s$sql, "UPDATE `recordings-calculated`"), mocked$executed)
  expect_length(cleared, 1)
  expect_match(cleared[[1]]$sql, "SET `peaks_url` = NULL WHERE `source` = \\? AND `id` = \\?;$")
  expect_identical(cleared[[1]]$params, list("unp", "1"))

  #Only analyses made by this package, which peaks are not, by default
  mocked <- mockDB(deleteAllAnalyses("db", "unp", "1"))
  expect_false(any(vapply(mocked$executed, function(s) grepl("peaks_url", s$sql, fixed=TRUE), TRUE)))
})

#waveform_peaks() as an agent runs it: peaks served from a directory, with
#audiowaveform stood in for
peaksRun <- function(path, rows=list(), works=TRUE, force=FALSE) {
  dir <- tempfile("served")
  withr::defer(unlink(dir, recursive=TRUE), envir=parent.frame())
  withr::local_envvar(AUDIOBLAST_PEAKS_DIR=dir, AUDIOBLAST_PEAKS_RSYNC=NA,
                      AUDIOBLAST_PEAKS_URL="https://files.audioblast.org/")
  local_mocked_bindings(hasAudiowaveform=function() TRUE,
                        runAudiowaveform=fakeAudiowaveform(works=works)$run)
  mocked <- mockDB(waveform_peaks("db", "bio.acousti.ca", "10753", path, force=force), rows=rows)
  mocked$dir <- dir
  return(mocked)
}

test_that("peaks are made, put where they are served from, and their address written", {
  wav <- aWave()
  on.exit(unlink(wav))
  mocked <- peaksRun(wav)

  expect_identical(mocked$value, "measured")
  expect_true(file.exists(file.path(mocked$dir, "peaks", "bio.acousti.ca", "10753.json")))
  expect_identical(onlyStatement(mocked)$params[[3]],
                   "https://files.audioblast.org/peaks/bio.acousti.ca/10753.json")
})

test_that("a recording that has peaks keeps them, unless they are to be made again", {
  wav <- aWave()
  on.exit(unlink(wav))
  had <- someRows(peaks_url="https://files.audioblast.org/peaks/bio.acousti.ca/10753.json")

  mocked <- peaksRun(wav, rows=had)
  expect_identical(mocked$value, "kept")
  expect_length(mocked$executed, 0)

  mocked <- peaksRun(wav, rows=had, force=TRUE)
  expect_identical(mocked$value, "measured")
  expect_length(mocked$executed, 1)
})

test_that("a recording no peaks can be made of is done with, and nothing written", {
  notAudio <- aFile()
  on.exit(unlink(notAudio))
  mocked <- peaksRun(notAudio)
  expect_identical(mocked$value, "unmeasurable")
  expect_length(mocked$executed, 0)

  wav <- aWave()
  on.exit(unlink(wav), add=TRUE)
  expect_identical(peaksRun(wav, works=FALSE)$value, "unmeasurable")
})

test_that("a recording that could not be downloaded is not fetched again for its peaks", {
  #analyse() hands over the address of a recording it gave up downloading
  local_mocked_bindings(av_audio_convert=function(...) stop("Fetched it"))
  mocked <- peaksRun("https://example.org/gone.wav")
  expect_identical(mocked$value, "unmeasurable")
})

test_that("peaks with nowhere to go, or whose address is not kept, are given back", {
  wav <- aWave()
  on.exit(unlink(wav))
  local_mocked_bindings(hasAudiowaveform=function() TRUE,
                        runAudiowaveform=fakeAudiowaveform()$run)
  withr::local_envvar(AUDIOBLAST_PEAKS_DIR=NA, AUDIOBLAST_PEAKS_RSYNC=NA)
  mocked <- suppressWarnings(mockDB(waveform_peaks("db", "unp", "1", wav)))
  expect_identical(mocked$value, "retry")
  expect_length(mocked$executed, 0)

  dir <- tempfile("served")
  on.exit(unlink(dir, recursive=TRUE), add=TRUE)
  withr::local_envvar(AUDIOBLAST_PEAKS_DIR=dir)
  local_mocked_bindings(dbExecute=function(conn, statement, params=NULL, ...) stop("The database has gone away"),
                        dbGetQuery=function(conn, statement, params=NULL, ...) data.frame(),
                        backoff=function() c(0, 0))
  expect_identical(suppressWarnings(waveform_peaks("db", "unp", "1", wav)), "retry")
})

test_that("an agent without audiowaveform gives peaks back rather than crossing them off", {
  wav <- aWave()
  on.exit(unlink(wav))
  local_mocked_bindings(hasAudiowaveform=function() FALSE)
  mocked <- expect_warning(mockDB(doTask("db", "waveform_peaks", "unp", "1", wav, "agent1")),
                           "audiowaveform is not to be found")

  expect_identical(mocked$value, "released")
  expect_length(mocked$executed, 1)
  expect_match(onlyStatement(mocked)$sql, "^DELETE FROM `tasks-progress`")
})

test_that("a waveform peaks task that was done is crossed off", {
  wav <- aWave()
  dir <- tempfile("served")
  on.exit(unlink(c(wav, dir), recursive=TRUE))
  withr::local_envvar(AUDIOBLAST_PEAKS_DIR=dir)
  local_mocked_bindings(hasAudiowaveform=function() TRUE,
                        runAudiowaveform=fakeAudiowaveform()$run)
  mocked <- mockDB(doTask("db", "waveform_peaks", "bio.acousti.ca", "10753", wav, "agent1"))

  expect_identical(mocked$value, "measured")
  expect_identical(onlyStatement(mocked, 2)$sql, "CALL `delete-task`(?, ?, ?, ?);")
  expect_identical(onlyStatement(mocked, 2)$params, list("agent1", "bio.acousti.ca", "10753", "waveform_peaks"))
})

test_that("audiowaveform itself makes peaks of a recording", {
  skip_if_not(hasAudiowaveform(), "audiowaveform is not installed")
  wav <- aWave(seconds=2)
  out <- tempfile(fileext=".json")
  on.exit(unlink(c(wav, out)))

  expect_true(peaksFile(wav, out))
  peaks <- rjson::fromJSON(file=out)
  expect_identical(as.integer(peaks$channels), 1L)
  expect_identical(as.integer(peaks$bits), 8L)
  #86 points a second, give or take the last
  expect_equal(peaks$length, 2 * 86, tolerance=2)
})
