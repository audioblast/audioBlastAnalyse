#' @importFrom cli hash_file_sha256
#' @importFrom seewave duration
#' @importFrom av av_media_info
#' @export
recordings_calculated <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  sql = paste0("SELECT * FROM `recordings-calculated` WHERE `source`='", source, "' AND `id`='", id, "' AND `hash` IS NOT NULL")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  if (dbGetRowCount(res) == 1) {
    print("Alredy calculated hash.")
  } else {
    dl_file(file, tmp)
    hash <- hash_file_sha256(tmp)
    print(paste("Hash is: ", hash))
    sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `hash`) VALUES('", source, "', '", id, "', '", hash, "') ON DUPLICATE KEY UPDATE `hash` = '", hash, "';")
    dbExecute(db, sql)
  }

  sql = paste0("SELECT * FROM `recordings-calculated` WHERE `source`='", source, "' AND `id`='", id, "' AND `duration` IS NOT NULL")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  if (dbGetRowCount(res) == 1) {
    print("Alredy calculated duration.")
  } else {
    dl_file(file, tmp)
    w <- readAudio(tmp)
    d <- duration(w)
    print(paste("Duration is: ", duration))
    sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `duration`) VALUES('", source, "', '", id, "', '", duration, "') ON DUPLICATE KEY UPDATE `duration` = '", duration, "';")
    dbExecute(db, sql)
  }


  sql = paste0("SELECT * FROM `recordings-calculated` WHERE `source`='", source, "' AND `id`='", id, "' AND `channels` IS NOT NULL")
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  if (dbGetRowCount(res) == 1) {
    print("Alredy calculated channels")
  } else {
    dl_file(file, tmp)
    channels <- av_media_info(file)$audio[['channels']]
    print(paste("Channels: ", channels))
    sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `channels`) VALUES('", source, "', '", id, "', '", channels, "') ON DUPLICATE KEY UPDATE `channels` = '", channels, "';")
    dbExecute(db, sql)
  }
}
