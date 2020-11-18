#' Rainfall
#'
#' Calculates rainfall using bedoya
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
#' @importFrom sonicscrewdriver rainfallDetection
#' @importFrom rjson toJSON

a_bedoya <- function(db, source, id, file, type, duration, tmp, force=FALSE) {
  if (force==TRUE) {
    deleteAnalysis(db, "analysis-bedoya", source, id)
  }

  if (analysedRowCount(db, "analysis-bedoya", source, id) >= duration - 1) {
    print("File already calculated -- skipping")
    return()
  }
  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }

  n <- floor(duration/30)
  for (i in (1:n)*30) {
    if (rowAnalysed(db, "analysis-bedoya", source, id, (n-1)*30)) {
      #print("Skip existing result.")
    } else {

      if (n*30 - (n-1)*30 < 0) return()
      dl_file(file, tmp)
      if (n == duration) {
        w <- readWave(tmp, from=(n-1)*30, units="seconds")
      } else {
        w <- readWave(tmp, from=(n-1)*30, to=n*30, units="seconds")
      }
      v <- rainfallDetection(w, method="bedoya2017")
      insertAnalysis(db, "analysis-bedoya", source, id, 30, (n-1)*30, v)
    }
  }
}
