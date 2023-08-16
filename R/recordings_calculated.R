#' Calculate basic file data
#'
#' Calculates basic file data such as a sha256 hash, duration and channels.
#' @param db database connector
#' @param source Source
#' @param id id (unique within source)
#' @param file url to file
#' @param type MIME type
#' @param duration audio duration
#' @param tmp Location to download temp file
#' @param force If TRUE recalculates all values
#' @param verbose If TRUE outputs debugging information
#' @importFrom cli hash_file_sha256
#' @importFrom seewave duration
#' @importFrom av av_media_info
#' @export
recordings_calculated <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
  sql = paste0("SELECT * FROM `v-recordings-calculated` ",
               "WHERE `source`=", dbQuoteString(db, source),
               " AND `id`=",dbQuoteString(db, id), ";")
  res <- abdbGetQuery(db, sql)
  if (!is.na(res[[1, "hash"]])) {
    print("Already calculated hash.")
  } else {
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
    channels <- av_media_info(tmp)$audio[['channels']]
    print(paste("Channels: ", channels))
    if (is.numeric(channels)) {
      sql = paste0("UPDATE `recordings-calculated` SET `channels` = ",
                   channels,
                   " WHERE `source` = ", dbQuoteString(db, source),
                   "AND `id` = ", dbQuoteString(db, id), ";")
      abdbExecute(db, sql)
    }
  }
}
