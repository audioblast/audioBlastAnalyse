#' Rainfall
#'
#' Calculates rainfall using Bedoya's method
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
#' @importFrom tuneR readWave
#' @importFrom soundecology bioacoustic_index

a_bi <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  n <- ceiling(duration/30)

  if (force==TRUE) {
    deleteAnalysis(db, "analysis-bi", source, id)
  }

  if (analysedRowCount(db, "analysis-bi", source, id) == n) {
    if (verbose) {print("File already calculated -- skipping");}
    return()
  }
  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }


  for (i in (1:n)) {
    if (rowAnalysed(db, "analysis-bi", source, id, (i-1)*30)) {
      #print("Skip existing result.")
    } else {
      if (verbose) { print(paste("bi startTime:",(i-1)*30))}
      if (duration - (i-1)*30 < 0) return()
      dl_file(file, tmp)
      if (i == duration) {
        w <- readAudio(tmp, from=(i-1)*30, units="seconds")
      } else {
        w <- readAudio(tmp, from=(i-1)*30, to=i*30, units="seconds")
      }
      if (length(w@left)==0) {
        #Where duration provided is longer than actual duration read insert a NULL
        #This prevents the file being unnecessarily downloaded and analysed again each time
        insertAnalysis(db, "analysis-bi", source, id, 30, (i-1)*30, NULL)
        next()
      }
      b <- bioacoustic_index(w)
      insertAnalysis(db, "analysis-bi", source, id, 30, (i-1)*30, b$left_area)
    }
  }
}
