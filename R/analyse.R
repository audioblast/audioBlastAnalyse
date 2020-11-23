#' Run all analyses
#'
#' @param db database connector
#' @param sense Allows "web" for the audioBlast! API to analyse, or a source for local analysis
#' @param force If TRUE recalculates all values
#' @param verbose If TRUE outputs debugging information
#' @importFrom tools file_ext
#' @export
analyse <- function(db, sense="web", force=FALSE, verbose=FALSE) {
  if (sense=="web") {
    ss <- fetchDownloadableRecordings(db)
  } else {
    ss <-fetchRecordingsFromSource(db, sense)
  }

  for (i in 1:nrow(ss)) {
    if (ss[i, "file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1812_22g.WAV") next()
    if (ss[i, "file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1121.WAV") next()
    if (verbose) {print(ss[i, "file"]);}
    if (sense == "web") {
      tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
    } else {
      tmp <- paste0("/",ss[i, "file"])
    }
    if (verbose) {print("TDSC");}
    a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
    if (verbose) {print("bedoya");}
    a_bedoya(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
    if (sense == "web") {
        unlink(tmp)
    }
  }

}
