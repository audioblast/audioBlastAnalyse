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
#' @export
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbExecute dbGetRowCount
#' @importFrom curl curl_download
#' @importFrom tuneR readWave
#' @importFrom tdsc tdsc
#' @importFrom rjson toJSON

a_tdsc <- function(db, source, id, file, type, duration, tmp, force=FALSE) {
  if (force==TRUE) {
    dbExecute(db, paste0("DELETE FROM `analysis-tdsc` WHERE source = '",source,"' AND id=",id))
    }

  sql <- paste0("SELECT `source`, `id` FROM `analysis-tdsc` WHERE `source` = '",source,"' AND id=",id)
  res <- dbSendQuery(db, sql)
  dbFetch(res)

  print(paste("Duration:", duration))
  print(paste("Row count:", dbGetRowCount(res)))
  if (dbGetRowCount(res) >= duration - 1) {
    print("File already calculated -- skipping")
    return()
  }
  if (duration == 0) {
    print("Provided duration is zero -- skipping")
    return()
  }

  for (i in 1:duration) {
    sql <- paste0("SELECT `source`, `id` FROM `analysis-tdsc` WHERE `source` = '",source,"' AND id=",id," AND startTime = ",(i-1))
    res <- dbSendQuery(db, sql)
    dbFetch(res)
    if (dbGetRowCount(res) != 0) {
      #print("Skip existing result.")
    } else {
      tryCatch({
        if (duration - (i-1) < 0) return()
        dl_file(file, tmp)
        if (i == duration) {
          w <- readWave(tmp, from=(i-1), units="seconds")
        } else {
          w <- readWave(tmp, from=(i-1), to=i, units="seconds")
        }
        v <- tdsc(w, max_D=14)
        sql <- paste0("INSERT INTO `analysis-tdsc` VALUES ('",source,"', ",id," ,1 , ",(i-1),", '", toJSON(v@a_matrix),"')")
        dbExecute(db, sql)
      }, error = function(e) {
        print(e)
      })
    }
    dbClearResult(res)
  }
}
