#' Audiowaveform
#'
#' Generate waveforms
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
a_audiowaveform <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  if (force==TRUE) {
    deleteAnalysis(db, "analysis-audiowaveform", source, id)
  }

  sql <- paste0("SELECT COUNT(*) FROM `audioblast`.`analysis-audiowaveform` WHERE `source` = '",source,"' AND id='",id,"' AND `type` = 'image300_40'")
  res <- dbSendQuery(db, sql)
  a <- dbFetch(res)[[1,1]]
  dbClearResult(res)

  if (a == 0) {
    dl_file(file, tmp)
    cmd <- paste0("audiowaveform -i ", tmp, " -o ",id,".png --width 300 --height 40 --no-axis-labels")
    print(cmd)
    system(cmd)
    cmd <- paste0("scp ",id,".png unpssh@audioblast.org:/var/www/html/cdn.audioblast.org/files/audiowaveform/300_40/")
    print(cmd)
    system(cmd)

    url <- paste0("https://cdn.audioblast.org/files/audiowaveform/300_40/", id, ".png")

    sql <- paste0("INSERT INTO `audioblast`.`analysis-audiowaveform` VALUES ('",source,"', '",id,"', 'image300_40', '",url,"');")
    print(sql)
    dbExecute(db, sql)
    unlink(paste0(id,".png"))
  }


  sql <- paste0("SELECT COUNT(*) FROM `audioblast`.`analysis-audiowaveform` WHERE `source` = '",source,"' AND id='",id,"' AND `type` = 'json10pps8bit'")
  res <- dbSendQuery(db, sql)
  a <- dbFetch(res)[[1,1]]
  dbClearResult(res)

  if (a == 0) {
    dl_file(file, tmp)

    system(paste0("audiowaveform -i ",tmp," -o ", id, ".json  --pixels-per-second 20 --bits 8"))
    r <- readChar(paste0(id,".json"), file.info(paste0(id,".json"))$size)
    sql <- paste0("INSERT INTO `audioblast`.`analysis-audiowaveform` VALUES ('",source,"','",id,"', 'json10pps8bit', '",r,"');")
    dbExecute(db, sql)
    print(sql)
    unlink(paste0("data.json"))
  }

  sql <- paste0("SELECT COUNT(*) FROM `audioblast`.`analysis-audiowaveform` WHERE `source` = '",source,"' AND id='",id,"' AND `type` = 'json200pps16bit'")
  res <- dbSendQuery(db, sql)
  a <- dbFetch(res)[[1,1]]
  dbClearResult(res)

  if (a == 0) {
    dl_file(file, tmp)

    system(paste0("audiowaveform -i ",tmp," -o ",id,".json  --pixels-per-second 200 --bits 16"))
    r <- readChar(paste0(id,".json"), file.info(paste0(id,".json"))$size)
    sql <- paste0("INSERT INTO `audioblast`.`analysis-audiowaveform` VALUES ('",source,"','",id,"', 'json200pps16bit', '",r,"');")
    dbExecute(db, sql)
    print(sql)
    unlink(paste0("data.json"))
  }
}



