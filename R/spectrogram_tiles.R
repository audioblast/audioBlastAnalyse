#' Spectrogram tiles of a recording
#'
#' Makes a spectrogram of the file a recording is held in, cut into tiles a
#' minute long and at several resolutions, puts them where they are served from
#' with the recording's waveform peaks at the same resolutions, and writes the
#' address of the manifest that describes them as the recording's
#' `spectrogram_url` in `recordings-calculated`, from which the API gives it. A
#' player shows the spectrogram from them at once, before the audio has
#' arrived, and instead of one it would make itself where the browser cannot
#' decode the whole recording, streaming the audio with the peaks (see
#' wavesurfer-tiled-spectrogram, whose `tools/make-tiles.sh` makes the same
#' tiles, and whose SPEC.md describes them).
#'
#' The tiles are made by ffmpeg's showspectrumpic from the first channel, at the
#' file's own sample rate:
#'
#' * 256 rows, from a 512-point FFT with a Hann window;
#' * about 86 columns a second in the finest level, each a whole number of
#'   samples, so that no column drifts against the audio;
#' * coarser levels each with four times fewer columns, until one tile covers
#'   the recording, each pixel keeping the loudest of the four it covers, so
#'   that short sounds still show zoomed out;
#' * loudness mapped as wavesurfer.js's Spectrogram plugin maps it with
#'   `gainDB: 50, rangeDB: 80, colorMap: 'gray'`, so that one can stand in for
#'   the other;
#' * greyscale JPEG at ffmpeg quality 12.
#'
#' The peaks are the lowest and highest sample of each column of each level, in
#' the BBC audiowaveform JSON format at 16 bits. They are made as well as those
#' waveform_peaks() makes, not instead of them.
#'
#' They are put at `spectrograms/<source>/<id>/<type>/`, the source and id named
#' as the download cache names them (see safeName()) and the type saying how
#' they were made (spectrogramType()): each level's tiles in a directory named
#' by its samples a column, the peaks beside them, and the manifest,
#' `index.json`, put last. Where they are put is as for waveform_peaks().
#'
#' @param db database connector
#' @param source Source
#' @param id id (unique within source)
#' @param path Path to the file holding the recording
#' @param force If TRUE makes the tiles again, though there are some already
#' @param verbose If TRUE says what is being done
#' @return Invisibly, what came of it, as recordings_calculated() says it:
#'
#'   * `measured`: the tiles were made, put where they are served from, and
#'     the address of their manifest written.
#'   * `kept`: the recording had tiles already, and they were left as they were.
#'   * `unmeasurable`: no tiles could be made from the file, or there was no
#'     file. Nothing is written.
#'   * `retry`: the tiles could not be put where they are served from, the
#'     database would not keep their address, or ffmpeg is not to be found here.
#' @export
spectrogram_tiles <- function(db, source, id, path, force=FALSE, verbose=FALSE) {
  if (!hasFfmpeg()) {
    warning("ffmpeg is not to be found, so spectrogram tiles are given back")
    return(invisible("retry"))
  }
  if (!force && !is.na(spectrogramURL(db, source, id))) {
    if (verbose) print(paste("Already has spectrogram tiles:", source, id))
    return(invisible("kept"))
  }
  #A recording that could not be downloaded comes as its address. ffmpeg would
  #try to fetch it again, so it is not given the chance.
  if (!file.exists(path)) {
    if (verbose) print(paste("No file to make spectrogram tiles from:", source, id))
    return(invisible("unmeasurable"))
  }

  out <- tempfile("tiles")
  on.exit(unlink(out, recursive=TRUE), add=TRUE)
  files <- spectrogramTiles(path, out)
  if (is.null(files)) {
    if (verbose) print(paste("No spectrogram tiles could be made:", source, id))
    return(invisible("unmeasurable"))
  }

  names <- c(files, "index.json")
  url <- publishFiles(file.path(out, names), paste0(spectrogramPath(source, id), names))
  if (is.na(url)) {
    warning(paste("Could not put the spectrogram tiles of", source, id, "where they are served from"))
    return(invisible("retry"))
  }
  if (!writeSpectrogram(db, source, id, url)) {
    warning(paste("Could not write where the spectrogram tiles of", source, id, "are"))
    return(invisible("retry"))
  }
  if (verbose) print(paste("Spectrogram tiles:", source, id, "-", url))
  return(invisible("measured"))
}

#How the tiles are made, as the directory they are put in names them: JPEG, a
#minute a tile, about 86 columns a second in the finest level, 256 rows, and
#the calibration of their loudness and levels. Tiles made another way go in
#another directory, and replace these as the recording's spectrogram_url.
spectrogramType <- function() {
  return("jpg60s86pps256h-2026-10b")
}

#How the tiles are made, as spectrogramTiles() is told
spectrogramSettings <- function() {
  return(list(tileSeconds=60, pps=86, height=256, channel=0, drange=80, limit=-48,
              quality=12, calibration="2026-10b"))
}

#The ffmpeg and ffprobe programs: FFMPEG and FFPROBE where they are not on the
#path
ffmpegCommand <- function() {
  return(Sys.getenv("FFMPEG", "ffmpeg"))
}
ffprobeCommand <- function() {
  return(Sys.getenv("FFPROBE", "ffprobe"))
}

hasFfmpeg <- function() {
  return(nzchar(Sys.which(ffmpegCommand())) && nzchar(Sys.which(ffprobeCommand())))
}

#Runs ffmpeg, giving whether it said it had done what it was asked
runFfmpeg <- function(args) {
  status <- tryCatch(system2(ffmpegCommand(), args, stdout=FALSE, stderr=FALSE),
                     error=function(e) -1L)
  return(identical(as.integer(status), 0L))
}

#What ffprobe says of a file's first audio stream: the entries asked for, in
#the order asked for, or NULL where it says nothing
probeAudio <- function(path, entries) {
  said <- tryCatch(suppressWarnings(system2(ffprobeCommand(),
    c("-v", "error", "-select_streams", "a:0", "-show_entries",
      paste0("stream=", paste(entries, collapse=",")), "-of", "csv=p=0", shQuote(path)),
    stdout=TRUE, stderr=FALSE)), error=function(e) character(0))
  said <- trimws(said[nzchar(trimws(said))])
  if (length(said) == 0) return(NULL)
  values <- strsplit(said[1], ",", fixed=TRUE)[[1]]
  if (length(values) < length(entries)) return(NULL)
  return(stats::setNames(as.list(values[seq_along(entries)]), entries))
}

#The samples a column is made of: the whole number nearest rate/pps that
#showspectrumpic divides into hops of at most fftSize samples with none left
#over, so that its columns never drift against the audio
samplesPerColumn <- function(rate, pps, fftSize) {
  target <- rate / pps
  candidates <- seq(max(1, floor(target) - 128), floor(target) + 128)
  hops <- ceiling(candidates / fftSize)
  fit <- candidates[candidates %% hops == 0]
  return(fit[which.min(abs(fit - target))])
}

#The levels tiles are made at, by their samples a column: the finest, then
#each four times coarser until one tile covers the recording. Every tile of
#every level is width columns across, so a coarser tile covers four tiles of
#the level below.
tileLevels <- function(samples, width, spc) {
  levels <- spc
  while (ceiling(samples / (width * levels[length(levels)])) > 1) {
    levels <- c(levels, 4 * levels[length(levels)])
  }
  return(levels)
}

#Makes the tiles of the file at path at every level, their peaks and their
#manifest, into the directory out, giving the names of the files made other
#than the manifest, "/"-separated under out, in order, or NULL where they
#could not all be made. The file is first decoded once to a mono WAV of the
#channel shown, so that each tile can be cut from it exactly, whatever its
#format.
spectrogramTiles <- function(path, out, settings=spectrogramSettings()) {
  about <- probeAudio(path, c("sample_rate", "channels"))
  if (is.null(about)) return(NULL)
  rate <- as.integer(about$sample_rate)
  channels <- as.integer(about$channels)
  if (is.na(rate) || rate < 1) return(NULL)
  channel <- if (!is.na(channels) && settings$channel < channels) settings$channel else 0

  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  mono <- tempfile(fileext=".wav")
  lossless <- tempfile("levels")
  on.exit(unlink(c(mono, lossless), recursive=TRUE), add=TRUE)
  if (!runFfmpeg(c("-v", "error", "-nostdin", "-y", "-i", shQuote(path), "-map", "0:a:0",
                   "-af", shQuote(paste0("pan=mono|c0=c", channel)),
                   "-c:a", "pcm_f32le", "-rf64", "auto", shQuote(mono)))) {
    return(NULL)
  }
  samples <- suppressWarnings(as.numeric(probeAudio(mono, "duration_ts")$duration_ts))
  if (length(samples) != 1 || is.na(samples) || samples < 1) return(NULL)

  spc <- samplesPerColumn(rate, settings$pps, 2 * settings$height)
  width <- max(1, round(settings$tileSeconds * rate / spc))
  levels <- tileLevels(samples, width, spc)

  files <- finestTiles(mono, rate, samples, spc, width, length(levels) > 1, out, lossless, settings)
  if (is.null(files)) return(NULL)
  for (k in seq_along(levels)[-1]) {
    made <- coarserTiles(samples, width, levels, k, out, lossless, settings)
    if (is.null(made)) return(NULL)
    files <- c(files, made)
  }
  peaks <- columnPeaks(mono, rate, levels, out)
  if (is.null(peaks)) return(NULL)

  writeLF(spectrogramManifest(rate, samples, width, settings$height, levels, channel, settings),
          file.path(out, "index.json"))
  return(c(files, peaks))
}

#The finest level's tiles, analysed from the audio, into out/<spc>/, giving
#their names, or NULL where ffmpeg could not make one. Where there are coarser
#levels to make from them (keep), each is kept lossless too, as PNG, in
#lossless/1/.
finestTiles <- function(mono, rate, samples, spc, width, keep, out, lossless, settings) {
  height <- settings$height
  tileSamples <- width * spc
  count <- ceiling(samples / tileSamples)
  names <- paste0(sprintf("%.0f", spc), "/", seq_len(count) - 1, ".jpg")
  dir.create(file.path(out, sprintf("%.0f", spc)), recursive=TRUE, showWarnings=FALSE)
  if (keep) dir.create(file.path(lossless, 1), recursive=TRUE, showWarnings=FALSE)

  for (i in seq_len(count)) {
    first <- (i - 1) * tileSamples
    left <- samples - first
    crop <- ""
    if (left < tileSamples) crop <- sprintf(",crop=%d:%d:0:0", as.integer(ceiling(left / spc)), height)
    graph <- paste0(
      "atrim=end_sample=", sprintf("%.0f", tileSamples), ",apad=whole_len=", sprintf("%.0f", tileSamples),
      ",showspectrumpic=s=", width, "x", height,
      ":legend=0:mode=combined:color=channel:scale=log:fscale=lin:win_func=hann",
      ":drange=", settings$drange, ":limit=", settings$limit, ",format=gray,negate", crop)
    jpg <- file.path(out, names[i])
    png <- if (keep) file.path(lossless, 1, paste0(i - 1, ".png"))
    made <- runFfmpeg(c("-v", "error", "-nostdin", "-y", "-ss", sprintf("%.9f", first / rate),
                        "-i", shQuote(mono), tileOutputs(graph, jpg, png, settings$quality)))
    if (!made || !file.exists(jpg)) return(NULL)
  }
  return(names)
}

#The tiles of level k (its place in levels, from 2), into out/<its samples a
#column>/, giving their names, or NULL where ffmpeg could not make one. Each is
#made from four of the level below's lossless tiles side by side, each run of
#four columns kept as the darkest pixel of its row, the loudest, so that a
#sound shorter than a column still shows. But for the last level, each is kept
#lossless too, for the level above.
coarserTiles <- function(samples, width, levels, k, out, lossless, settings) {
  level <- levels[k]
  levelSamples <- width * level
  count <- ceiling(samples / levelSamples)
  names <- paste0(sprintf("%.0f", level), "/", seq_len(count) - 1, ".jpg")
  below <- file.path(lossless, k - 1)
  keep <- k < length(levels)
  dir.create(file.path(out, sprintf("%.0f", level)), recursive=TRUE, showWarnings=FALSE)
  if (keep) dir.create(file.path(lossless, k), recursive=TRUE, showWarnings=FALSE)

  for (j in seq_len(count)) {
    sources <- file.path(below, paste0(4 * (j - 1) + 0:3, ".png"))
    sources <- sources[cumprod(file.exists(sources)) == 1]
    if (length(sources) == 0) return(NULL)
    left <- samples - (j - 1) * levelSamples
    columns <- if (left >= levelSamples) width else ceiling(left / level)
    graph <- paste0(paste0("[", seq_along(sources) - 1, "]", collapse=""),
                    if (length(sources) > 1) paste0("hstack=inputs=", length(sources), ","),
                    "format=gray,", fourColumnsAsOne(), sprintf(",crop=%d:ih:0:0", as.integer(columns)))
    jpg <- file.path(out, names[j])
    png <- if (keep) file.path(lossless, k, paste0(j - 1, ".png"))
    made <- runFfmpeg(c("-v", "error", "-nostdin", "-y", as.vector(rbind("-i", shQuote(sources))),
                        tileOutputs(graph, jpg, png, settings$quality)))
    if (!made || !file.exists(jpg)) return(NULL)
  }
  unlink(below, recursive=TRUE)
  return(names)
}

#The filters that keep each run of four columns of a grey image as its darkest
#pixel in each row. White (silence) pads the width to a multiple of four;
#eroding towards the right three times leaves each pixel the darkest of itself
#and the three after it; and every fourth column, those starting a run, is kept
#by taking alternate lines of the image turned on its side, twice.
fourColumnsAsOne <- function() {
  return(paste0("pad=ceil(iw/4)*4:ih:0:0:white,",
                paste(rep("erosion=coordinates=16", 3), collapse=","),
                ",transpose=clock,il=l=d:c=d,crop=iw:ih/2:0:0,il=l=d:c=d,crop=iw:ih/2:0:0,transpose=cclock"))
}

#ffmpeg's arguments to make a tile from a filter graph: a JPEG at jpg, and the
#same tile lossless at png where it is given
tileOutputs <- function(graph, jpg, png, quality) {
  if (is.null(png)) {
    return(c("-filter_complex", shQuote(graph), "-frames:v", "1", "-q:v", quality, shQuote(jpg)))
  }
  return(c("-filter_complex", shQuote(paste0(graph, ",split=2[jpg][png]")),
           "-map", shQuote("[jpg]"), "-frames:v", "1", "-q:v", quality, shQuote(jpg),
           "-map", shQuote("[png]"), "-frames:v", "1", shQuote(png)))
}

#The waveform peaks of every level, as peaks-<samples a column>.json in out,
#giving their names, or NULL where ffmpeg could not measure them: the lowest
#and highest sample of each column of the channel shown, in the BBC
#audiowaveform JSON format (version 2, 16-bit). Those of the finest level come
#from the audio, and each point of a coarser level is the lowest and highest
#of the four points below it.
columnPeaks <- function(mono, rate, levels, out) {
  measured <- measurePeaks(mono, levels[1])
  if (is.null(measured)) return(NULL)
  low <- measured$low
  high <- measured$high
  names <- paste0("peaks-", sprintf("%.0f", levels), ".json")
  for (k in seq_along(levels)) {
    if (k > 1) {
      low <- fourAsOne(low, pmin, Inf)
      high <- fourAsOne(high, pmax, -Inf)
    }
    writeLF(paste0('{"version":2,"channels":1,"sample_rate":', rate,
                   ',"samples_per_pixel":', sprintf("%.0f", levels[k]), ',"bits":16,"length":', length(low),
                   ',"data":[', paste(as.integer(rbind(low, high)), collapse=","), "]}"),
            file.path(out, names[k]))
  }
  return(names)
}

#The lowest and highest sample of each block of size samples of the mono WAV
#at mono, as 16-bit values, measured by ffmpeg's astats, or NULL where it could
#not measure them. ffmpeg runs in a directory of its own, so that no path in a
#filter needs escaping. astats' measure options (ffmpeg 4.4 on) make it five
#times quicker, an older ffmpeg being asked without them.
measurePeaks <- function(mono, size) {
  work <- tempfile("peaks")
  dir.create(work)
  home <- setwd(work)
  on.exit({
    setwd(home)
    unlink(work, recursive=TRUE)
  }, add=TRUE)
  blocks <- paste0("asetnsamples=n=", sprintf("%.0f", size), ":p=0,astats=metadata=1:reset=1")
  quick <- runFfmpeg(c("-v", "quiet", "-nostdin", "-i", shQuote(mono), "-af",
                       shQuote(paste0(blocks, ":measure_perchannel=Min_level+Max_level:measure_overall=none",
                                      ",ametadata=mode=print:file=levels.txt")),
                       "-f", "null", "-"))
  if (!quick) {
    unlink("levels.txt")
    slow <- runFfmpeg(c("-v", "error", "-nostdin", "-i", shQuote(mono), "-af",
                        shQuote(paste0(blocks, ",ametadata=mode=print:key=lavfi.astats.1.Min_level:file=levels.txt",
                                       ",ametadata=mode=print:key=lavfi.astats.1.Max_level:file=levels-max.txt")),
                        "-f", "null", "-"))
    if (!slow) return(NULL)
  }
  said <- unlist(lapply(intersect(c("levels.txt", "levels-max.txt"), list.files()), readLines))
  low <- int16(grep(".Min_level=", said, value=TRUE, fixed=TRUE))
  high <- int16(grep(".Max_level=", said, value=TRUE, fixed=TRUE))
  if (length(low) == 0 || length(low) != length(high) || anyNA(low) || anyNA(high)) return(NULL)
  return(list(low=low, high=high))
}

#Sample values as ffmpeg's metadata says them ("lavfi.astats.1.Min_level=-0.5"),
#as 16-bit ones, rounded half away from zero as make-tiles.sh rounds them
int16 <- function(said) {
  x <- suppressWarnings(as.numeric(sub(".*=", "", said))) * 32768
  x <- ifelse(x < 0, trunc(x - 0.5), trunc(x + 0.5))
  return(pmin(pmax(x, -32768), 32767))
}

#Each run of four values as one, by keep (pmin or pmax), the last run made up
#to four with fill
fourAsOne <- function(values, keep, fill) {
  runs <- matrix(c(values, rep(fill, (-length(values)) %% 4)), nrow=4)
  return(keep(runs[1, ], runs[2, ], runs[3, ], runs[4, ]))
}

#Writes lines to a file, each ended by a line feed alone on every system, so
#that what is served is the same wherever it was made
writeLF <- function(lines, path) {
  con <- file(path, "wb")
  on.exit(close(con), add=TRUE)
  writeLines(lines, con)
}

#The manifest of a set of tiles and their peaks (see wavesurfer-tiled-spectrogram's
#SPEC.md), as make-tiles.sh writes it
spectrogramManifest <- function(rate, samples, width, height, levels, channel, settings) {
  more <- ifelse(seq_along(levels) < length(levels), ",", "")
  return(c(
    "{",
    '  "type": "tiled-spectrogram",',
    '  "version": 1,',
    sprintf('  "duration": %.6f,', samples / rate),
    sprintf('  "sampleRate": %d,', as.integer(rate)),
    sprintf('  "channel": %d,', as.integer(channel)),
    '  "frequencyMin": 0,',
    sprintf('  "frequencyMax": %s,', format(rate / 2, scientific=FALSE)),
    '  "frequencyScale": "linear",',
    '  "window": "hann",',
    '  "colorMap": "gray",',
    #In wavesurfer.js's terms: ffmpeg measures a sine 2 dB lower than it does
    sprintf('  "dbRange": [%s, %s],', format(settings$limit - 2 - settings$drange), format(settings$limit - 2)),
    '  "levels": [',
    sprintf(paste0('    {"width": %d, "height": %d, "tileDuration": %.9f, "tileCount": %.0f, ',
                   '"tiles": "%.0f/{index}.jpg", "mimeType": "image/jpeg", "samplesPerColumn": %.0f, ',
                   '"pixelsPerSecond": %.6f, "fftSize": %d}%s'),
            as.integer(width), as.integer(height), width * levels / rate, ceiling(samples / (width * levels)),
            levels, levels, rate / levels, as.integer(2 * height), more),
    '  ],',
    '  "peaks": [',
    sprintf('    {"pointsPerSecond": %.6f, "samplesPerPixel": %.0f, "url": "peaks-%.0f.json"}%s',
            rate / levels, levels, levels, more),
    '  ],',
    sprintf('  "renderer": {"name": "ffmpeg showspectrumpic", "scale": "log", "drange": %s, "limit": %s},',
            format(settings$drange), format(settings$limit)),
    sprintf('  "calibration": "%s"', settings$calibration),
    "}"))
}

#Where a recording's tiles are put, under the directory or destination they
#are served from and under the address they are served at, ending in "/"
spectrogramPath <- function(source, id) {
  return(paste0("spectrograms/", safeName(source), "/", safeName(id), "/", spectrogramType(), "/"))
}

#The address of the manifest of a recording's tiles, or NA where it has none,
#where the tiles it has were made another way (their address names how), or
#where the database could not be asked: then they are made, as making them
#twice costs less than never making them
spectrogramURL <- function(db, source, id) {
  found <- abdbGetQuery(db,
    "SELECT `spectrogram_url` FROM `recordings-calculated` WHERE `source` = ? AND `id` = ?;",
    params=list(source, id))
  if (is.null(found) || nrow(found) == 0 || is.na(found[1, 1]) || !nzchar(found[1, 1])) {
    return(NA_character_)
  }
  url <- as.character(found[1, 1])
  if (!grepl(paste0("/", spectrogramType(), "/"), url, fixed=TRUE)) return(NA_character_)
  return(url)
}

#Writes where the manifest of a recording's tiles is, over where it was. As
#with writePeaks(), a recording not yet measured is given a row holding only
#this, which measuring it fills in.
writeSpectrogram <- function(db, source, id, url) {
  return(abdbExecute(db, paste(
    "INSERT INTO `recordings-calculated` (`source`, `id`, `spectrogram_url`) VALUES (?, ?, ?)",
    "ON DUPLICATE KEY UPDATE `spectrogram_url` = VALUES(`spectrogram_url`);"),
    params=list(source, id, url)))
}
