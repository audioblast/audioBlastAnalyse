#' Analyses soundscapes in one second chunks
#'
#' Analyses performed:
#' - TDSC
#'
#' @param db database connector
#' @param source Source
#' @param id id (unique within source)
#' @param file url to file
#' @param type MIME type
#' @param duration audio duration
#' @param tmp Location to download temp file
#' @param force If TRUE recalculates all values
#' @param verbose If TRUE outputs debugging information
#' @export
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbExecute dbGetRowCount
#' @importFrom curl curl_download
#' @importFrom sonicscrewdriver readAudio rainfallDetection allChannels channels_se
#' @importFrom rjson toJSON
#' @importFrom tdsc tdsc

soundscapes_by_second <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  print(tmp)
  n <- ceiling(duration)
  if (force==TRUE) {
    deleteAnalysis(db, "analysis-tdsc", source, id)
  }

  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }

  for (i in (1:n)) {
    dl_file(file, tmp)
    duration <- av_media_info(tmp)$duration

    if (duration < 1) return()

    if (duration - (i-1) < 0) return()

    if (i == duration) {
        w <- readAudio(tmp, from=(i-1), units="seconds")
    } else {
        w <- readAudio(tmp, from=(i-1), to=i, units="seconds")
    }

    if (verbose) { print(paste("tdsc startTime:",(i-1)))}
    v <- allChannels(w, tdsc, max_D=14, channel.param=NULL, output.FUN = channels_tdsc)

    insertAnalysis(db, "analysis-tdsc", source, id, 1, i-1, v)

  }
  sql = paste0("UPDATE `recordings-calculated` SET `soundscapes_second` = 1 ",
               "WHERE `source` = ", dbQuoteString(db, source),
               " AND `id` = ", dbQuoteString(db, id), ";")
  abdbExecute(db, sql)
}

#' @export
channels_tdsc <- function(...) {
  params = list(...)
  return(toJSON(params[[1]]@a_matrix))
}