#' Run all analyses
#'
#' @param db database connector
#' @param force If TRUE recalculates all values
#' @export
analyse <- function(db, force=FALSE) {
  res <- dbSendQuery(db, "SELECT source, id, `file`, `type`, Duration FROM `audioblast`.`recordings`")
  ss <- dbFetch(res)
  dbClearResult(res)

  for (i in 1:nrow(ss)) {
    tmp <- tempfile()
    a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], ss[[i, "Duration"]], tmp, force)
    unlink(tmp)
  }

}
