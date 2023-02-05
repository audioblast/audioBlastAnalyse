#' Analyses soundscapes in one minute chunks
#'
#' Analyses performed:
#' - ACI
#' - Bedoya rainfall
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
#' @importFrom sonicscrewdriver readAudio rainfallDetection
#' @importFrom rjson toJSON
#' @importFrom seewave ACI

soundscapes_by_minute <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  sql = paste0("SELECT * FROM `recordings-calclated` WHERE `source`='", source, "' AND `id`='", id, "' AND `soundscapes_minute` = 1")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  if (dbGetRowCount(res) == 1) {
    print("Alredy calculated soundscapes by minute.")
    return()
  }

  n <- ceiling(duration/60)

  if (force==TRUE) {
    deleteAnalysis(db, "analysis-aci", source, id)
    deleteAnalysis(db, "analysis-bedoya", source, id)
  }

  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }

  for (i in (1:n)) {
    if (duration - (i-1)*60 < 0) return()
    dl_file(file, tmp)
    if (i == duration) {
      w <- readAudio(tmp, from=(i-1)*60, units="seconds")
    } else {
      w <- readAudio(tmp, from=(i-1)*60, to=i*60, units="seconds")
    }
    if (length(w@left)==0) {
      #Where duration provided is longer than actual duration read insert a NULL
      #This prevents the file being unnecessarily downloaded and analysed again each time
      insertAnalysis(db, "analysis-aci", source, id, 60, (i-1)*60, NULL)
      insertAnalysis(db, "analysis-bedoya", source, id, 60, (i-1)*60, NULL)
      next()
    }

    if (verbose) { print(paste("aci startTime:",(i-1)*60))}
    v <- ACI(w)
    insertAnalysis(db, "analysis-aci", source, id, 60, (i-1)*60, v)
    if (verbose) { print(paste("Bedoya startTime:",(i-1)*60))}
    v <- rainfallDetection(w, method="bedoya2017")
    insertAnalysis(db, "analysis-bedoya", source, id, 60, (i-1)*60, v)

    sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `soundscapes_minute`) VALUES('", source, "', '", id, "', 1) ON DUPLICATE KEY UPDATE `soundscapes_minute` = 1;")
    dbExecute(db, sql)
  }
}
