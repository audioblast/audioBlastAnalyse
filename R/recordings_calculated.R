#' @importFrom cli hash_file_sha256
#' @importFrom seewave duration
#' @importFrom av av_media_info
#' @export
recordings_calculated <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  print(tmp)
  sql = paste0("SELECT * FROM `recordings-calculated` WHERE `source`='", source, "' AND `id`='", id, "' AND `hash` IS NOT NULL")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  c <- dbGetRowCount(res)
  dbClearResult(res)
  if (c == 1) {
    print("Already calculated hash.")
  } else {
    dl_file(file, tmp)
    hash <- hash_file_sha256(tmp)
    print(paste("Hash is: ", hash))
    sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `hash`) VALUES('", source, "', '", id, "', '", hash, "') ON DUPLICATE KEY UPDATE `hash` = '", hash, "';")
    abdbExecute(db, sql)
  }


  sql = paste0("SELECT * FROM `recordings-calculated` WHERE `source`='", source, "' AND `id`='", id, "' AND `duration` IS NOT NULL")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  c <- dbGetRowCount(res)
  dbClearResult(res)
  if (c == 1) {
    print("Already calculated duration.")
  } else {
    tryCatch({
      dl_file(file, tmp)
      w <- readAudio(tmp)
      d <- duration(w)
      print(paste("Duration is: ", duration))
      sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `duration`) VALUES('", source, "', '", id, "', '", duration, "') ON DUPLICATE KEY UPDATE `duration` = '", duration, "';")
      abdbExecute(db, sql)
    })
  }

  sql = paste0("SELECT * FROM `recordings-calculated` WHERE `source`='", source, "' AND `id`='", id, "' AND `channels` IS NOT NULL")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  c <- dbGetRowCount(res)
  dbClearResult(res)
  if (c == 1) {
    print("Alredy calculated channels")
  } else {
    tryCatch({
      dl_file(file, tmp)
      channels <- av_media_info(tmp)$audio[['channels']]
      print(paste("Channels: ", channels))
      if (channels != FALSE) {
        sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `channels`) VALUES('", source, "', '", id, "', '", channels, "') ON DUPLICATE KEY UPDATE `channels` = '", channels, "';")
        abdbExecute(db, sql)
      }
    })
  }
}
