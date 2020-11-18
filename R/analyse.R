#' Run all analyses
#'
#' @param db database connector
#' @param force If TRUE recalculates all values
#' @importFrom tools file_ext
#' @export
analyse <- function(db, force=FALSE) {
  res <- dbSendQuery(db, "SELECT source, id, `file`, `type`, Duration FROM `audioblast`.`recordings`")
  ss <- dbFetch(res)
  dbClearResult(res)

  for (i in 1:nrow(ss)) {
    print(ss[i, "file"])
    tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
    a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], ss[[i, "Duration"]], tmp, force)
    a_bedoya(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], ss[[i, "Duration"]], tmp, force)
   unlink(tmp)
  }

}
