#Spectrogram tiles are made by ffmpeg. Most of what is checked here stands in
#for it; the tests against ffmpeg itself run where it is installed, and check
#what matters about the tiles: that every column is where it should be in time.

#Tiles as spectrogramTiles() would leave them: the given number of tile files
#and a manifest, in out
fakeTiles <- function(count=2) {
  calls <- 0
  make <- function(path, out, settings=spectrogramSettings()) {
    calls <<- calls + 1
    dir.create(out, recursive=TRUE, showWarnings=FALSE)
    tiles <- paste0(seq_len(count) - 1, ".jpg")
    for (tile in tiles) writeBin(as.raw(c(0xFF, 0xD8, 0xFF, 0xD9)), file.path(out, tile))
    writeLines('{"type": "tiled-spectrogram", "version": 1}', file.path(out, "index.json"))
    return(tiles)
  }
  return(list(make=make, calls=function() calls))
}

test_that("an agent asks for spectrogram tiles only where it can make them", {
  local_mocked_bindings(hasAudiowaveform=function() FALSE, hasFfmpeg=function() FALSE)
  expect_identical(tasksDone(), "recordings_calculated")
  local_mocked_bindings(hasFfmpeg=function() TRUE)
  expect_identical(tasksDone(), c("recordings_calculated", "spectrogram_tiles"))
  local_mocked_bindings(hasAudiowaveform=function() TRUE)
  expect_identical(tasksDone(), c("recordings_calculated", "waveform_peaks", "spectrogram_tiles"))
})

test_that("ffmpeg and ffprobe are looked for where FFMPEG and FFPROBE say, or on the path", {
  withr::local_envvar(FFMPEG=NA, FFPROBE=NA)
  expect_identical(ffmpegCommand(), "ffmpeg")
  expect_identical(ffprobeCommand(), "ffprobe")
  withr::local_envvar(FFMPEG="/opt/ffmpeg/bin/ffmpeg", FFPROBE="/opt/ffmpeg/bin/ffprobe")
  expect_identical(ffmpegCommand(), "/opt/ffmpeg/bin/ffmpeg")
  expect_false(hasFfmpeg())
})

test_that("a column is a whole number of samples that ffmpeg divides into whole hops", {
  #As tools/make-tiles.sh chooses them, and as checked against ffmpeg with
  #click trains at each rate
  expect_equal(samplesPerColumn(44100, 86, 512), 512)
  expect_equal(samplesPerColumn(48000, 86, 512), 558)
  expect_equal(samplesPerColumn(96000, 86, 512), 1116)
  expect_equal(samplesPerColumn(220500, 86, 512), 2562)
  for (rate in c(8000, 22050, 32000, 44100, 48000, 96000, 192000, 220500, 250000, 300000, 384000)) {
    spc <- samplesPerColumn(rate, 86, 512)
    expect_equal(spc %% ceiling(spc / 512), 0, info=paste(rate, "Hz"))
    expect_lt(abs(spc - rate / 86), 10)
  }
})

test_that("tiles are put under their source, id and how they were made", {
  expect_identical(spectrogramPath("bio.acousti.ca", "10753"),
                   "spectrograms/bio.acousti.ca/10753/jpg60s86pps256h-2026-10a/")
  expect_identical(spectrogramPath("xc", "a/b c"),
                   paste0("spectrograms/xc/", safeName("a/b c"), "/jpg60s86pps256h-2026-10a/"))
  #The type is kept in a varchar(45)
  expect_lte(nchar(spectrogramType()), 45)
})

test_that("files are put in order, the last only once the rest are, and its address given", {
  src <- tempfile("made")
  dir <- tempfile("served")
  dir.create(src)
  on.exit(unlink(c(src, dir), recursive=TRUE), add=TRUE)
  names <- c("0.jpg", "1.jpg", "index.json")
  for (name in names) writeLines(name, file.path(src, name))

  url <- publishFiles(file.path(src, names), paste0("spectrograms/x/1/t/", names), dir=dir, rsync="",
                      base="https://files.audioblast.org")
  expect_identical(url, "https://files.audioblast.org/spectrograms/x/1/t/index.json")
  expect_setequal(list.files(file.path(dir, "spectrograms/x/1/t")), names)

  #A file that cannot be put stops the rest, and the manifest is never put
  unlink(dir, recursive=TRUE)
  unlink(file.path(src, "1.jpg"))
  expect_true(is.na(suppressWarnings(
    publishFiles(file.path(src, names), paste0("s/", names), dir=dir, rsync=""))))
  expect_false(file.exists(file.path(dir, "s/index.json")))
})

test_that("by rsync, the last file goes in a pass of its own after the rest", {
  sent <- list()
  local_mocked_bindings(runRsync=function(from, to) {
    sent[[length(sent) + 1]] <<- list(from=from, to=to, there=all(file.exists(from)))
    TRUE
  })
  src <- tempfile("made")
  dir.create(src)
  on.exit(unlink(src, recursive=TRUE), add=TRUE)
  names <- c("0.jpg", "1.jpg", "index.json")
  for (name in names) writeLines(name, file.path(src, name))

  url <- publishFiles(file.path(src, names), paste0("spectrograms/x/1/t/", names), dir="",
                      rsync="tiles@files:/srv/files/", base="https://files.audioblast.org/")
  expect_identical(url, "https://files.audioblast.org/spectrograms/x/1/t/index.json")
  expect_length(sent, 2)
  expect_length(sent[[1]]$from, 2)
  expect_match(sent[[1]]$from, "/\\./spectrograms/x/1/t/[01]\\.jpg$")
  expect_match(sent[[2]]$from, "/\\./spectrograms/x/1/t/index\\.json$")
  expect_true(sent[[1]]$there && sent[[2]]$there)
})

test_that("files whose first pass fails are not followed by the manifest", {
  passes <- 0
  local_mocked_bindings(runRsync=function(from, to) { passes <<- passes + 1; FALSE })
  src <- tempfile("made")
  dir.create(src)
  on.exit(unlink(src, recursive=TRUE), add=TRUE)
  for (name in c("0.jpg", "index.json")) writeLines(name, file.path(src, name))
  expect_true(is.na(publishFiles(file.path(src, c("0.jpg", "index.json")), c("a/0.jpg", "a/index.json"),
                                 dir="", rsync="tiles@files:/srv/files/")))
  expect_identical(passes, 1)
})

test_that("files are put where AUDIOBLAST_FILES_ says, or where AUDIOBLAST_PEAKS_ did", {
  withr::local_envvar(AUDIOBLAST_FILES_DIR=NA, AUDIOBLAST_PEAKS_DIR="/srv/peaks")
  expect_identical(filesSetting("DIR"), "/srv/peaks")
  withr::local_envvar(AUDIOBLAST_FILES_DIR="/srv/files")
  expect_identical(filesSetting("DIR"), "/srv/files")
  withr::local_envvar(AUDIOBLAST_FILES_URL=NA, AUDIOBLAST_PEAKS_URL=NA)
  expect_identical(filesSetting("URL", "https://files.audioblast.org/"), "https://files.audioblast.org/")
})

test_that("where a recording's tiles are is written over where they were, bound not written in", {
  mocked <- mockDB(writeSpectrogram("db", "o'brien", "1", "https://files.audioblast.org/spectrograms/x/index.json"))
  statement <- onlyStatement(mocked)
  expect_match(statement$sql, "^INSERT INTO `analysis-spectrogram` \\(`source`, `id`, `type`, `value`\\)")
  expect_match(statement$sql, "ON DUPLICATE KEY UPDATE `value` = VALUES\\(`value`\\);$")
  expect_false(grepl("o'brien", statement$sql, fixed=TRUE))
  expect_identical(statement$params,
                   list("o'brien", "1", spectrogramType(), "https://files.audioblast.org/spectrograms/x/index.json"))

  found <- mockDB(spectrogramURL("db", "unp", "1"), rows=someRows(value="https://example.org/index.json"))
  expect_identical(found$value, "https://example.org/index.json")
  expect_true(is.na(mockDB(spectrogramURL("db", "unp", "1"))$value))
})

#spectrogram_tiles() as an agent runs it: tiles served from a directory, with
#ffmpeg stood in for
tilesRun <- function(path, rows=list(), count=2, force=FALSE) {
  dir <- tempfile("served")
  withr::defer(unlink(dir, recursive=TRUE), envir=parent.frame())
  withr::local_envvar(AUDIOBLAST_FILES_DIR=dir, AUDIOBLAST_FILES_RSYNC=NA,
                      AUDIOBLAST_FILES_URL="https://files.audioblast.org/")
  fake <- fakeTiles(count)
  local_mocked_bindings(hasFfmpeg=function() TRUE, spectrogramTiles=fake$make)
  mocked <- mockDB(spectrogram_tiles("db", "bio.acousti.ca", "10753", path, force=force), rows=rows)
  mocked$dir <- dir
  mocked$made <- fake$calls()
  return(mocked)
}

test_that("tiles are made, put where they are served from, and their manifest's address written", {
  wav <- aWave()
  on.exit(unlink(wav), add=TRUE)
  mocked <- tilesRun(wav, count=3)

  expect_identical(mocked$value, "measured")
  served <- file.path(mocked$dir, spectrogramPath("bio.acousti.ca", "10753"))
  expect_setequal(list.files(served), c("0.jpg", "1.jpg", "2.jpg", "index.json"))
  expect_identical(onlyStatement(mocked)$params[[4]],
                   paste0("https://files.audioblast.org/", spectrogramPath("bio.acousti.ca", "10753"), "index.json"))
})

test_that("a recording that has tiles keeps them, unless they are to be made again", {
  wav <- aWave()
  on.exit(unlink(wav), add=TRUE)
  had <- someRows(value="https://files.audioblast.org/spectrograms/bio.acousti.ca/10753/t/index.json")
  mocked <- tilesRun(wav, rows=had)
  expect_identical(mocked$value, "kept")
  expect_identical(mocked$made, 0)
  expect_identical(tilesRun(wav, rows=had, force=TRUE)$value, "measured")
})

test_that("a recording no tiles can be made of, or that was never downloaded, is done with", {
  local_mocked_bindings(hasFfmpeg=function() TRUE, spectrogramTiles=function(...) NULL)
  wav <- aWave()
  on.exit(unlink(wav), add=TRUE)
  mocked <- mockDB(spectrogram_tiles("db", "unp", "1", wav))
  expect_identical(mocked$value, "unmeasurable")
  expect_length(mocked$executed, 0)
  expect_identical(mockDB(spectrogram_tiles("db", "unp", "1", "https://example.org/gone.wav"))$value, "unmeasurable")
})

test_that("an agent without ffmpeg gives spectrogram tiles back rather than crossing them off", {
  wav <- aWave()
  on.exit(unlink(wav), add=TRUE)
  local_mocked_bindings(hasFfmpeg=function() FALSE)
  mocked <- expect_warning(mockDB(doTask("db", "spectrogram_tiles", "unp", "1", wav, "agent1")),
                           "ffmpeg is not to be found")
  expect_identical(mocked$value, "released")
  expect_match(onlyStatement(mocked)$sql, "^DELETE FROM `tasks-progress`")
})

test_that("a spectrogram tiles task that was done is crossed off", {
  wav <- aWave()
  dir <- tempfile("served")
  on.exit(unlink(c(wav, dir), recursive=TRUE), add=TRUE)
  withr::local_envvar(AUDIOBLAST_FILES_DIR=dir)
  local_mocked_bindings(hasFfmpeg=function() TRUE, spectrogramTiles=fakeTiles()$make)
  mocked <- mockDB(doTask("db", "spectrogram_tiles", "bio.acousti.ca", "10753", wav, "agent1"))
  expect_identical(mocked$value, "measured")
  expect_identical(onlyStatement(mocked, 2)$sql, "CALL `delete-task`(?, ?, ?, ?);")
  expect_identical(onlyStatement(mocked, 2)$params, list("agent1", "bio.acousti.ca", "10753", "spectrogram_tiles"))
})

#A recording of a click every ten seconds, mono, at the given rate
aClickTrain <- function(rate, seconds=70) {
  samples <- numeric(rate * seconds)
  for (s in seq(0, seconds - 1, by=10)) samples[s * rate + 1:4] <- 30000
  path <- tempfile(fileext=".wav")
  tuneR::writeWave(tuneR::Wave(left=samples, samp.rate=rate, bit=16, pcm=TRUE), path)
  return(path)
}

#A tile's pixels, as a matrix of grey levels (rows by columns), read by ffmpeg
tilePixels <- function(path) {
  size <- system2(ffprobeCommand(), c("-v", "error", "-show_entries", "stream=width,height", "-of", "csv=p=0",
                                       shQuote(path)), stdout=TRUE)
  size <- as.integer(strsplit(trimws(size[1]), ",")[[1]])
  raw <- tempfile()
  on.exit(unlink(raw), add=TRUE)
  system2(ffmpegCommand(), c("-v", "error", "-y", "-i", shQuote(path), "-f", "rawvideo", "-pix_fmt", "gray",
                             shQuote(raw)))
  return(matrix(as.integer(readBin(raw, "raw", n=size[1] * size[2])), nrow=size[2], byrow=TRUE))
}

test_that("ffmpeg itself makes tiles whose every column is where it should be in time", {
  skip_if_not(hasFfmpeg(), "ffmpeg is not installed")
  for (rate in c(44100, 48000)) {
    wav <- aClickTrain(rate)
    out <- tempfile("tiles")
    tiles <- spectrogramTiles(wav, out)
    expect_identical(tiles, c("0.jpg", "1.jpg"), info=paste(rate, "Hz"))

    manifest <- rjson::fromJSON(file=file.path(out, "index.json"))
    spc <- samplesPerColumn(rate, 86, 512)
    expect_identical(manifest$type, "tiled-spectrogram")
    expect_equal(manifest$samplesPerColumn, spc)
    expect_equal(manifest$tileCount, 2)
    expect_equal(manifest$duration, 70)
    expect_equal(manifest$frequencyMax, rate / 2)
    expect_equal(manifest$tileDuration, manifest$width * spc / rate, tolerance=1e-8)

    pixels <- list(tilePixels(file.path(out, "0.jpg")), tilePixels(file.path(out, "1.jpg")))
    expect_identical(dim(pixels[[1]]), c(256L, as.integer(manifest$width)))
    #The last tile is as wide as the audio left for it, to the column
    left <- 70 * rate - manifest$width * spc
    expect_identical(ncol(pixels[[2]]), as.integer(ceiling(left / spc)))

    #Every click darkens the column that holds it, in whichever tile that is
    for (k in 0:6) {
      column <- (k * 10 * rate) %/% spc
      tile <- column %/% manifest$width + 1
      at <- column %% manifest$width + 1
      darkness <- 255 - colMeans(pixels[[tile]])
      near <- max(1, at - 6):min(ncol(pixels[[tile]]), at + 6)
      expect_lte(abs(near[which.max(darkness[near])] - at), 1, label=paste(rate, "Hz click", k))
    }
    unlink(c(wav, out), recursive=TRUE)
  }
})

test_that("ffmpeg itself makes no tiles of what is not audio", {
  skip_if_not(hasFfmpeg(), "ffmpeg is not installed")
  notAudio <- aFile()
  out <- tempfile("tiles")
  on.exit(unlink(c(notAudio, out), recursive=TRUE), add=TRUE)
  expect_null(spectrogramTiles(notAudio, out))
})
