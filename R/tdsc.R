#' Time domain signal coding
#'
#' Calculates TDSC every second
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
#' @importFrom tdsc tdsc
#' @importFrom rjson toJSON
#' @importFrom sonicscrewdriver readAudio

a_tdsc <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  if (force==TRUE) {
    deleteAnalysis(db, "analysis-tdsc", source, id)
  }

  if (analysedRowCount(db, "analysis-tdsc", source, id) >= duration - 1) {
    if (verbose) {print("File already calculated -- skipping");}
    return()
  }
  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }

  for (i in 1:duration) {
    if (rowAnalysed(db, "analysis-tdsc", source, id, (i-1))) {
      #print("Skip existing result.")
    } else {

        if (duration - (i-1) < 0) return()
        dl_file(file, tmp)
        if (i == duration) {
          w <- readAudio(tmp, from=(i-1), units="seconds")
        } else {
          w <- readAudio(tmp, from=(i-1), to=i, units="seconds")
        }
        if (length(w@left)==0) {
          #Where duration provided is longer than actual duration read insert a NULL
          #This prevents the file being unnecessarily downloaded and analysed again each time
          insertAnalysis(db, "analysis-tdsc", source, id, 1, i-1, NULL)
          next()
        }
        v <- tdsc(w, max_D=14)
        if (!is.null(v)) {
          insertAnalysis(db, "analysis-tdsc", source, id, 1, i-1, toJSON(v@a_matrix))
        }
    }
  }
  return(0)
}
