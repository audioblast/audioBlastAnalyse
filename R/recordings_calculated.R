#' @importFrom cli hash_file_sha256
#' @importFrom seewave duration
#' @importFrom av av_media_info
#' @export
recordings_calculated <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  print(tmp)
  sql = paste0("SELECT `source`, `id`, CAST(`hash` AS CHAR) as `hash`, ",
               "`duration`, `channels`, `soundscapes_minute`, `soundscapes_second` ",
               "FROM `recordings-calculated` WHERE `source`=", dbQuoteString(db, source),
               " AND `id`=",dbQuoteString(db, id), ";")
  res <- abdbGetQuery(db, sql)
  if (!is.na(res[[1, "hash"]])) {
    print("Already calculated hash.")
  } else {
    dl_file(file, tmp)
    hash <- hash_file_sha256(tmp)
    print(paste("Hash is: ", hash))
    sql = paste0("UPDATE `recordings-calculated` SET `hash` = ",
                 dbQuoteString(db, hash),
                 "WHERE `source` = ", dbQuoteString(db, source),
                 "AND `id` = ", dbQuoteString(db, id), ";")
    abdbExecute(db, sql)
  }

  if (!is.na(res[[1, "duration"]])) {
    print("Already calculated duration.")
  } else {
    tryCatch({
      dl_file(file, tmp)
      w <- readAudio(tmp)
      d <- duration(w)
      print(paste("Duration is: ", duration))
      sql = paste0("UPDATE `recordings-calculated` SET `duration` = ",
                   duration,
                   " WHERE `source` = ", dbQuoteString(db, source),
                   "AND `id` = ", dbQuoteString(db, id), ";")
      abdbExecute(db, sql)
    })
  }

  if (!is.na(res[[1, "channels"]])) {
    print("Alredy calculated channels")
  } else {
    tryCatch({
      dl_file(file, tmp)
      channels <- av_media_info(tmp)$audio[['channels']]
      print(paste("Channels: ", channels))
      if (channels != FALSE) {
        sql = paste0("UPDATE `recordings-calculated` SET `channels` = ",
                     channels,
                     " WHERE `source` = ", dbQuoteString(db, source),
                     "AND `id` = ", dbQuoteString(db, id), ";")
        abdbExecute(db, sql)
      }
    })
  }
}
