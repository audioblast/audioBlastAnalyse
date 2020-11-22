#' Run all analyses
#'
#' @param db database connector
#' @param force If TRUE recalculates all values
#' @param verbose If TRUE outputs debugging information
#' @importFrom tools file_ext
#' @export
analyse <- function(db, force=FALSE, verbose=FALSE) {
  res <- dbSendQuery(db, "SELECT source, id, `file`, `type`, Duration FROM `audioblast`.`recordings`")
  ss <- dbFetch(res)
  dbClearResult(res)

  for (i in 1:nrow(ss)) {
    if (ss[i, "file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1812_22g.WAV") next()
    if (ss[i, "file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1121.WAV") next()
    if (verbose) {print(ss[i, "file"]);}
    tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
    if (verbose) {print("TDSC");}
    a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], ss[[i, "Duration"]], tmp, force, verbose)
    if (verbose) {print("bedoya");}
    a_bedoya(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], ss[[i, "Duration"]], tmp, force, verbose)
    unlink(tmp)
  }

}
