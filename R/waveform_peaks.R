#' Waveform peaks of a recording
#'
#' Makes the waveform peaks of the file a recording is held in, puts them where
#' they are served from, and writes where that is as the recording's
#' `peaks_url` in `recordings-calculated`, from which the API gives it. A
#' player draws the recording's waveform from them while the audio itself is
#' still on its way.
#'
#' The peaks are made by BBC audiowaveform: the minimum and maximum of each
#' 1/86 s of the recording, mixed to one channel, at 8 bits, as audiowaveform's
#' JSON (version 2). audiowaveform reads its input as a stream, so a recording
#' of hours costs no more memory than one of seconds. A file in a format it does
#' not read is converted to WAV with ffmpeg first.
#'
#' Peaks are put at `peaks/<source>/<id>.json`, the source and id named as the
#' download cache names them (see safeName()), either in the directory
#' `AUDIOBLAST_PEAKS_DIR` names, where an agent runs beside the files it
#' serves, or by rsync to the destination `AUDIOBLAST_PEAKS_RSYNC` names. Their
#' address is that path under `AUDIOBLAST_PEAKS_URL`, by default
#' `https://files.audioblast.org/`.
#'
#' @param db database connector
#' @param source Source
#' @param id id (unique within source)
#' @param path Path to the file holding the recording
#' @param force If TRUE makes the peaks again, though there are some already
#' @param verbose If TRUE says what is being done
#' @return Invisibly, what came of it, as recordings_calculated() says it:
#'
#'   * `measured`: the peaks were made, put where they are served from, and
#'     their address written.
#'   * `kept`: the recording had peaks already, and they were left as they were.
#'   * `unmeasurable`: no peaks could be made from the file, or there was no
#'     file. Nothing is written.
#'   * `retry`: the peaks could not be put where they are served from, the
#'     database would not keep their address, or audiowaveform is not to be
#'     found here. The task is worth doing again, perhaps by another agent.
#' @export
waveform_peaks <- function(db, source, id, path, force=FALSE, verbose=FALSE) {
  if (!hasAudiowaveform()) {
    #Not something wrong with the recording, so not something to cross it off
    #for: an agent elsewhere can make them
    warning("audiowaveform is not to be found, so waveform peaks are given back")
    return(invisible("retry"))
  }
  if (!force && !is.na(peaksURL(db, source, id))) {
    if (verbose) print(paste("Already has peaks:", source, id))
    return(invisible("kept"))
  }
  #A recording that could not be downloaded comes as its address. ffmpeg would
  #try to fetch it again, so it is not given the chance.
  if (!file.exists(path)) {
    if (verbose) print(paste("No file to make peaks from:", source, id))
    return(invisible("unmeasurable"))
  }

  out <- tempfile(fileext=".json")
  on.exit(unlink(out), add=TRUE)
  if (!peaksFile(path, out)) {
    if (verbose) print(paste("No peaks could be made:", source, id))
    return(invisible("unmeasurable"))
  }

  url <- publishPeaks(out, source, id)
  if (is.na(url)) {
    warning(paste("Could not put the peaks of", source, id, "where they are served from"))
    return(invisible("retry"))
  }
  if (!writePeaks(db, source, id, url)) {
    warning(paste("Could not write where the peaks of", source, id, "are"))
    return(invisible("retry"))
  }
  if (verbose) print(paste("Peaks:", source, id, "-", url))
  return(invisible("measured"))
}

#Points a second. A player drawing 344 pixels a second (BioAcoustica's) gives
#each point four of them.
peaksPerSecond <- function() {
  return(86)
}

peaksBits <- function() {
  return(8)
}

#The audiowaveform program: AUDIOWAVEFORM where it is not on the path
audiowaveformCommand <- function() {
  return(Sys.getenv("AUDIOWAVEFORM", "audiowaveform"))
}

hasAudiowaveform <- function() {
  return(nzchar(Sys.which(audiowaveformCommand())))
}

#The address of the peaks a recording has, or NA where it has none, or where
#the database could not be asked: then they are made, as making them twice
#costs less than never making them
peaksURL <- function(db, source, id) {
  found <- abdbGetQuery(db,
    "SELECT `peaks_url` FROM `recordings-calculated` WHERE `source` = ? AND `id` = ?;",
    params=list(source, id))
  if (is.null(found) || nrow(found) == 0 || is.na(found[1, 1]) || !nzchar(found[1, 1])) {
    return(NA_character_)
  }
  return(as.character(found[1, 1]))
}

#Writes where a recording's peaks are, over where they were. A recording not
#yet measured has no row in recordings-calculated, so it is given one holding
#only this, which measuring it fills in.
writePeaks <- function(db, source, id, url) {
  return(abdbExecute(db, paste(
    "INSERT INTO `recordings-calculated` (`source`, `id`, `peaks_url`) VALUES (?, ?, ?)",
    "ON DUPLICATE KEY UPDATE `peaks_url` = VALUES(`peaks_url`);"),
    params=list(source, id, url)))
}

#Makes the peaks of the file at path into out, giving whether it could. A file
#audiowaveform does not read is given it as WAV, which ffmpeg makes of it.
#' @importFrom av av_audio_convert
peaksFile <- function(path, out) {
  format <- audioFormat(path)
  input <- path
  if (is.na(format)) {
    input <- tempfile(fileext=".wav")
    on.exit(unlink(input), add=TRUE)
    converted <- tryCatch({
      av_audio_convert(path, input, verbose=FALSE)
      file.exists(input)
    }, error=function(e) FALSE)
    if (!converted) return(FALSE)
    format <- "wav"
  }
  return(runAudiowaveform(input, format, out) && peaksValid(out))
}

#Runs audiowaveform on a file of the given input format, giving whether it
#said it had made the peaks
runAudiowaveform <- function(input, format, out) {
  status <- tryCatch(
    system2(audiowaveformCommand(),
            c("-q", "-i", shQuote(input), "--input-format", format, "-o", shQuote(out),
              "--pixels-per-second", peaksPerSecond(), "-b", peaksBits()),
            stdout=FALSE, stderr=FALSE),
    error=function(e) -1L)
  return(identical(as.integer(status), 0L))
}

#Copies a file to an rsync destination, giving whether it was copied. The file
#is named as it is to be named there from the /./ in its path on, which rsync
#keeps (--relative), making the directories it needs.
runRsync <- function(from, to) {
  status <- tryCatch(
    system2("rsync", c("--relative", "--chmod=D755,F644", shQuote(from), shQuote(to)),
            stdout=FALSE, stderr=FALSE),
    error=function(e) -1L)
  return(identical(as.integer(status), 0L))
}

#The input format audiowaveform is to read a file as, from what the file says
#of itself rather than its name or MIME type (BioAcoustica has files called
#application/octet-stream), or NA for anything it does not read, such as AIFF
#or FLAC in Ogg, which is converted first
audioFormat <- function(path) {
  header <- fileBytes(path, 0, 64)
  if ((hasBytes(header, 1, "RIFF") || hasBytes(header, 1, "RF64") || hasBytes(header, 1, "BW64")) &&
      hasBytes(header, 9, "WAVE")) {
    return("wav")
  }
  if (hasBytes(header, 1, "fLaC")) return("flac")
  if (hasBytes(header, 1, "OggS") && length(header) >= 28) {
    #The first page holds the codec's first packet, after the 27 bytes of the
    #page header and its table of segment lengths, which is as long as its last
    #byte says
    start <- 28 + as.integer(header[27])
    if (length(header) < start + 7) return(NA_character_)
    packet <- header[start:length(header)]
    if (hasBytes(packet, 1, "OpusHead")) return("opus")
    if (identical(packet[1], as.raw(1)) && hasBytes(packet, 2, "vorbis")) return("ogg")
    return(NA_character_)
  }
  if (hasBytes(header, 1, "ID3")) return("mp3")
  if (length(header) >= 2 && identical(header[1], as.raw(0xFF)) &&
      bitwAnd(as.integer(header[2]), 0xE0) == 0xE0) {
    return("mp3")
  }
  return(NA_character_)
}

#Whether a file holds peaks a player can draw: audiowaveform's JSON with as
#many values as it says it has
#' @importFrom rjson fromJSON
peaksValid <- function(path) {
  if (!file.exists(path)) return(FALSE)
  peaks <- tryCatch(fromJSON(file=path), error=function(e) NULL)
  if (!is.list(peaks) || !is.numeric(peaks$length) || peaks$length < 1) return(FALSE)
  channels <- if (is.numeric(peaks$channels)) peaks$channels else 1
  return(length(peaks$data) == 2 * channels * peaks$length)
}

#Where a recording's peaks are put, under the directory or destination they
#are served from and under the address they are served at
peaksPath <- function(source, id) {
  return(paste0("peaks/", safeName(source), "/", safeName(id), ".json"))
}

#Puts a recording's peaks where they are served from, giving their address,
#or NA where they could not be put there. A file is never served half written:
#in a directory it is written beside where it goes and moved there, and rsync
#does the same at the other end.
publishPeaks <- function(file, source, id,
                         dir=Sys.getenv("AUDIOBLAST_PEAKS_DIR"),
                         rsync=Sys.getenv("AUDIOBLAST_PEAKS_RSYNC"),
                         base=Sys.getenv("AUDIOBLAST_PEAKS_URL", "https://files.audioblast.org/")) {
  path <- peaksPath(source, id)
  if (nzchar(dir)) {
    dest <- file.path(dir, path)
    dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
    part <- paste0(dest, ".", Sys.getpid(), ".part")
    put <- file.copy(file, part, overwrite=TRUE) && file.rename(part, dest)
    unlink(part)
    if (!put) return(NA_character_)
  } else if (nzchar(rsync)) {
    #Staged under the path it is to have, so that rsync makes the source's
    #directory where there is none yet
    stage <- tempfile("peaks")
    on.exit(unlink(stage, recursive=TRUE), add=TRUE)
    staged <- file.path(stage, path)
    dir.create(dirname(staged), recursive=TRUE, showWarnings=FALSE)
    if (!file.copy(file, staged)) return(NA_character_)
    if (!runRsync(paste0(stage, "/./", path), rsync)) return(NA_character_)
  } else {
    warning("Neither AUDIOBLAST_PEAKS_DIR nor AUDIOBLAST_PEAKS_RSYNC is set, so peaks have nowhere to go")
    return(NA_character_)
  }
  return(paste0(sub("/*$", "/", base), path))
}
