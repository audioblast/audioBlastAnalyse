#' Spectrogram tiles of a recording
#'
#' Makes a spectrogram of the file a recording is held in, cut into tiles a
#' minute long, puts them where they are served from, and writes the address of
#' the manifest that describes them to the `analysis-spectrogram` table, from
#' which the API gives it as the recording's `spectrogram_url`. A player shows
#' the spectrogram from them at once, before the audio has arrived, and instead
#' of one it would make itself where the browser cannot decode the whole
#' recording (see wavesurfer-tiled-spectrogram, whose `tools/make-tiles.sh` makes
#' the same tiles, and whose SPEC.md describes them).
#'
#' The tiles are made by ffmpeg's showspectrumpic from the first channel, at the
#' file's own sample rate:
#'
#' * 256 rows, from a 512-point FFT with a Hann window;
#' * about 86 columns a second, each a whole number of samples, so that no
#'   column drifts against the audio;
#' * levels mapped as wavesurfer.js's Spectrogram plugin maps them with
#'   `gainDB: 50, rangeDB: 80, colorMap: 'gray'`, so that one can stand in for
#'   the other;
#' * greyscale JPEG at ffmpeg quality 12.
#'
#' They are put at `spectrograms/<source>/<id>/<type>/`, the source and id named
#' as the download cache names them (see safeName()) and the type saying how
#' they were made (spectrogramType()), with the manifest, `index.json`, put
#' last. Where they are put is as for waveform_peaks().
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
  tiles <- spectrogramTiles(path, out)
  if (is.null(tiles)) {
    if (verbose) print(paste("No spectrogram tiles could be made:", source, id))
    return(invisible("unmeasurable"))
  }

  names <- c(tiles, "index.json")
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

#How the tiles are made, as `analysis-spectrogram` names them: JPEG, a minute
#a tile, about 86 columns a second, 256 rows, and the calibration of their
#levels. Tiles made another way would be another type, beside these.
spectrogramType <- function() {
  return("jpg60s86pps256h-2026-10a")
}

#How the tiles are made, as spectrogramTiles() is told
spectrogramSettings <- function() {
  return(list(tileSeconds=60, pps=86, height=256, channel=0, drange=80, limit=-48,
              quality=12, calibration="2026-10a"))
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

#Makes the tiles of the file at path, and their manifest, into the directory
#out, giving the tiles' file names in order, or NULL where none could be made.
#The file is first decoded once to a mono WAV of the channel shown, so that
#each tile can be cut from it exactly, whatever its format.
spectrogramTiles <- function(path, out, settings=spectrogramSettings()) {
  about <- probeAudio(path, c("sample_rate", "channels"))
  if (is.null(about)) return(NULL)
  rate <- as.integer(about$sample_rate)
  channels <- as.integer(about$channels)
  if (is.na(rate) || rate < 1) return(NULL)
  channel <- if (!is.na(channels) && settings$channel < channels) settings$channel else 0

  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  mono <- tempfile(fileext=".wav")
  on.exit(unlink(mono), add=TRUE)
  if (!runFfmpeg(c("-v", "error", "-nostdin", "-y", "-i", shQuote(path), "-map", "0:a:0",
                   "-af", shQuote(paste0("pan=mono|c0=c", channel)),
                   "-c:a", "pcm_f32le", "-rf64", "auto", shQuote(mono)))) {
    return(NULL)
  }
  samples <- suppressWarnings(as.numeric(probeAudio(mono, "duration_ts")$duration_ts))
  if (length(samples) != 1 || is.na(samples) || samples < 1) return(NULL)

  height <- settings$height
  spc <- samplesPerColumn(rate, settings$pps, 2 * height)
  width <- max(1, round(settings$tileSeconds * rate / spc))
  tileSamples <- width * spc
  count <- ceiling(samples / tileSamples)
  tiles <- paste0(seq_len(count) - 1, ".jpg")

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
    made <- runFfmpeg(c("-v", "error", "-nostdin", "-y", "-ss", sprintf("%.9f", first / rate),
                        "-i", shQuote(mono), "-lavfi", shQuote(graph),
                        "-frames:v", "1", "-q:v", settings$quality, shQuote(file.path(out, tiles[i]))))
    if (!made || !file.exists(file.path(out, tiles[i]))) return(NULL)
  }

  writeLines(spectrogramManifest(rate, samples, spc, width, height, count, channel, settings),
             file.path(out, "index.json"))
  return(tiles)
}

#The manifest of a set of tiles (see wavesurfer-tiled-spectrogram's SPEC.md),
#as make-tiles.sh writes it
spectrogramManifest <- function(rate, samples, spc, width, height, count, channel, settings) {
  return(c(
    "{",
    '  "type": "tiled-spectrogram",',
    '  "version": 1,',
    sprintf('  "duration": %.6f,', samples / rate),
    sprintf('  "tileDuration": %.9f,', width * spc / rate),
    sprintf('  "tileCount": %d,', as.integer(count)),
    '  "tiles": "{index}.jpg",',
    '  "mimeType": "image/jpeg",',
    sprintf('  "width": %d,', as.integer(width)),
    sprintf('  "height": %d,', as.integer(height)),
    sprintf('  "pixelsPerSecond": %.6f,', rate / spc),
    '  "frequencyMin": 0,',
    sprintf('  "frequencyMax": %s,', format(rate / 2, scientific=FALSE)),
    '  "frequencyScale": "linear",',
    sprintf('  "sampleRate": %d,', as.integer(rate)),
    sprintf('  "samplesPerColumn": %d,', as.integer(spc)),
    sprintf('  "channel": %d,', as.integer(channel)),
    sprintf('  "fftSize": %d,', as.integer(2 * height)),
    '  "window": "hann",',
    '  "colorMap": "gray",',
    #In wavesurfer.js's terms: ffmpeg measures a sine 2 dB lower than it does
    sprintf('  "dbRange": [%s, %s],', format(settings$limit - 2 - settings$drange), format(settings$limit - 2)),
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
#or where the database could not be asked: then they are made, as making them
#twice costs less than never making them
spectrogramURL <- function(db, source, id) {
  found <- abdbGetQuery(db,
    "SELECT `value` FROM `analysis-spectrogram` WHERE `source` = ? AND `id` = ? AND `type` = ?;",
    params=list(source, id, spectrogramType()))
  if (is.null(found) || nrow(found) == 0 || is.na(found[1, 1]) || !nzchar(found[1, 1])) {
    return(NA_character_)
  }
  return(as.character(found[1, 1]))
}

#Writes where the manifest of a recording's tiles is, over where it was
writeSpectrogram <- function(db, source, id, url) {
  return(abdbExecute(db, paste(
    "INSERT INTO `analysis-spectrogram` (`source`, `id`, `type`, `value`) VALUES (?, ?, ?, ?)",
    "ON DUPLICATE KEY UPDATE `value` = VALUES(`value`);"),
    params=list(source, id, spectrogramType(), url)))
}
