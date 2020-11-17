#' Time domain signal coding
#'
#' Calculates TDSC every second
#'
#' @param db database connector
#' @param force If TRUE recalculates all values
#' @export
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbExecute dbGetRowCount
#' @importFrom curl curl_download
#' @importFrom tuneR readWave
#' @importFrom tdsc tdsc
#' @importFrom rjson toJSON

a_tdsc <- function(db, force=FALSE) {
  res <- dbSendQuery(db, "SELECT source, id, `file`, Duration FROM `audioblast`.`recordings` WHERE `type` = 'audio/x-wav'")
  ss <- dbFetch(res)
  dbClearResult(res)

  for (j in 1:nrow(ss)) {
    if (force==TRUE) {
      dbExecute(db, paste0("DELETE FROM `analysis-tdsc` WHERE source = '",ss[[j,"source"]],"' AND id=",ss[[j,"id"]]))
      }

    sql <- paste0("SELECT `source`, `id` FROM `analysis-tdsc` WHERE `source` = '",ss[[j,"source"]],"' AND id=",ss[[j,"id"]])
    res <- dbSendQuery(db, sql)
    dbFetch(res)

    print(paste("Duration:", ss[[j, "Duration"]]))
    print(paste("Row count:", dbGetRowCount(res)))
    if (dbGetRowCount(res) >= ss[[j, "Duration"]] - 1) {
      print("File already calculated -- skipping")
      next()
    }

    if (ss[[j, "Duration"]] == 0) {
      print("Provided duration is zero -- skipping")
      next()
    }

    tmp <- tempfile()
    downloaded <- tryCatch({
      print(ss[[j, "file"]])
      curl_download(ss[[j,"file"]], tmp)
    }, error = function(e) {
      print(e)
    })
    if (inherits(downloaded, "error")) next()

    d <- ss[[j, "Duration"]]
    for (i in 1:d) {
      if (i == d) {
        w <- readWave(tmp, from=(i-1), units="seconds")
      } else {
        w <- readWave(tmp, from=(i-1), to=i, units="seconds")
      }

      sql <- paste0("SELECT `source`, `id` FROM `analysis-tdsc` WHERE `source` = '",ss[[j,"source"]],"' AND id=",ss[[j,"id"]]," AND startTime = ",(i-1))
      res <- dbSendQuery(db, sql)
      dbFetch(res)
      if (dbGetRowCount(res) != 0) {
        #print("Skip existing result.")
      } else {
        tryCatch({
          if (d - (i-1) < 0) next()
          v <- tdsc(w, max_D=14)
          sql <- paste0("INSERT INTO `analysis-tdsc` VALUES ('",ss[[j,"source"]],"', ",ss[[j, "id"]]," ,1 , ",(i-1),", '", toJSON(v@a_matrix),"')")
          dbExecute(db, sql)
        }, error = function(e) {
          print(e)
        })
      }
      dbClearResult(res)
    }
    unlink(tmp)
  }
}
