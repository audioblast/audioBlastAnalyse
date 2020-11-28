#' Download web accessible files
#'
#' @param db database connector
#' @export
download <- function(db) {
  ss <- fetchDownloadableRecordings(db)

  for (i in 1:nrow(ss)) {
    dl_file(ss[i, "file"])

  }

}
