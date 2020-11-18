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
#' @export
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbExecute dbGetRowCount
#' @importFrom curl curl_download
#' @importFrom tuneR readWave
#' @importFrom sonicscrewdriver rainfallDetection readAudio
#' @importFrom rjson toJSON

a_bedoya <- function(db, source, id, file, type, duration, tmp, force=FALSE) {
  n <- floor(duration/30)

  if (force==TRUE) {
    deleteAnalysis(db, "analysis-bedoya", source, id)
  }

  if (analysedRowCount(db, "analysis-bedoya", source, id) >= n - 1) {
    print("File already calculated -- skipping")
    return()
  }
  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }


  for (i in (1:n)) {
    if (rowAnalysed(db, "analysis-bedoya", source, id, (i-1)*30)) {
      #print("Skip existing result.")
    } else {

      if (i*30 - (i-1)*30 < 0) return()
      dl_file(file, tmp)
      if (i == duration) {
        w <- readAudio(tmp, from=(i-1)*30, units="seconds")
      } else {
        w <- readAudio(tmp, from=(i-1)*30, to=i*30, units="seconds")
      }
      v <- rainfallDetection(w, method="bedoya2017")
      insertAnalysis(db, "analysis-bedoya", source, id, 30, (i-1)*30, v)
    }
  }
}
