#' Measure a recording
#'
#' Measures the file a recording is held in and writes what it found to the
#' `recordings-calculated` table: its SHA-256 hash, how long it is, how many
#' channels it has, the rate it was sampled at, its bit depth, its bit rate,
#' the format it is in and its size in bytes.
#'
#' These are measurements of the file itself, and are kept apart from what a
#' source says about a recording, which is in the `recordings` table. A source
#' that says its recordings are 48 kHz can then be asked whether they are.
#'
#' The file is never decoded: its bytes are read once for the hash, and the
#' rest is what the container says of itself, so measuring an hour of audio
#' costs what measuring a second does. Measurements that need the audio itself,
#' such as its peak amplitude, belong with the analyses that decode it.
#'
#' The bit depth is what the file's own header says: how many bits of each
#' sample mean something, which for a 20-bit recording stored in 24-bit samples
#' is 20. Decoders cannot be asked, as they widen samples into a format of their
#' own. Only a lossless file has one, and one whose header cannot be read, such
#' as FLAC in Matroska, has none recorded rather than a guess.
#'
#' A recording that cannot be read is recorded as such rather than passed over,
#' so that a source of files that have moved, or that were never audio, can be
#' found by asking the table rather than by reading logs.
#'
#' @param db database connector
#' @param source Source
#' @param id id (unique within source)
#' @param path Path to the file holding the recording
#' @param force If TRUE measures the recording again, however it went before
#' @param verbose If TRUE says what is being measured
#' @return Invisibly, what came of it, which tells an agent what to do with the
#'   task it was doing (see analyse()):
#'
#'   * `measured`: the recording was measured, and the measurements written.
#'   * `kept`: it had been measured before, and was left as it was.
#'   * `unmeasurable`: no audio could be read from it, and that was written.
#'   * `retry`: the database would not keep the measurements. Nothing is known
#'     about the recording that wasn't before, so the task is worth doing again.
#'
#'   Only `retry` is worth doing again: a recording that cannot be read will not
#'   read any better for being measured twice.
#' @export
recordings_calculated <- function(db, source, id, path, force=FALSE, verbose=FALSE) {
  if (!force) {
    #A recording is measured once. Measuring it again after a fix is asked for
    #with force, or by clearing the status of the rows a fix bears on.
    known <- calculatedStatus(db, source, id)
    if (!is.null(known) && identical(known$status, "ok")) {
      if (verbose) print(paste("Already measured:", source, id))
      return(invisible("kept"))
    }
  }

  measurements <- measureFile(path)
  if (verbose) print(paste("Measured:", source, id, "-", measurementText(measurements)))

  if (!writeMeasurements(db, source, id, measurements)) {
    warning(paste("Could not write the measurements of", source, id))
    return(invisible("retry"))
  }
  if (!identical(measurements$status, "ok")) {
    warning(paste0("Could not measure ", source, " ", id, ": ", measurements$error))
    return(invisible("unmeasurable"))
  }
  return(invisible("measured"))
}

#Measurements as a line to be read by whoever is watching an agent work
measurementText <- function(measurements) {
  if (!identical(measurements$status, "ok")) {
    return(paste0(measurements$status, ", ", measurements$error))
  }
  return(paste0(round(measurements$duration, 1), "s, ",
                measurements$channels, " channel(s), ",
                measurements$sample_rate, " Hz, ",
                measurements$codec))
}
