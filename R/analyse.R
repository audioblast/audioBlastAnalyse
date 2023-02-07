#' Run all analyses
#'
#' @param db database connector
#' @param mode "web" for online files, or name of source to analyse locally
#' @param verbose Gives verbose output if TRUE
#' @param force Forces recalculation of analyses if TRUE
#' @param base_dir Directory relative paths are located in
#' @param reverse Reverses the data frame of recordings to analyse
#' @param shuffle Shuffles rows before analysing
#' @param checkFile Provide a filename to check for debugging purposes
#' @importFrom tools file_ext
#' @export
analyse <- function(db, mode="web", verbose=FALSE, force=FALSE, base_dir="", reverse=FALSE, shuffle=FALSE, checkFile=NULL) {
  db <- DBI::dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)
  if (mode=="web") {
    ss <- fetchDownloadableRecordings(db)
  } else {
    ss <-fetchRecordingsFromSource(db, mode)
  }

  cn <- colnames(ss)
  ss <- cbind(ss, rep_len(mode, nrow(ss)), rep_len(verbose, nrow(ss)), rep_len(force, nrow(ss)), rep_len(base_dir, nrow(ss)))
  colnames(ss) <- c(cn, "mode", "verbose", "force", "base_dir")

  if (reverse) {
    ss <- ss[order(nrow(ss):1),]
  }
  if (shuffle) {
    ss <- ss[sample(nrow(ss)),]
  }

  for (i in 1:nrow(ss)) {
    if (!is.null(checkFile) && ss[i, "file"] != checkFile) {next()}

    if (file_ext(ss[i, "file"]) == "ogg") next()
    if (file_ext(ss[i, "file"]) == "zc") next()


    if (verbose) {print(ss[i, "file"]);}
    if (mode == "web") {
      tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
    } else {
      tmp <- paste0(base_dir,ss[i, "file"])
    }
    print(paste("Temp is: ", tmp))
    if (verbose) {print("TDSC");}
    #tryCatch({
    #  a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
    #})
    if (verbose) {print("Calculated propoerties of recordings");}
    tryCatch({
      recordings_calculated(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
    })
    if (verbose) {print("Soundscapes by Minute");}
    tryCatch({
      soundscapes_by_minute(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
    })
    if (mode == "web") {
      unlink(tmp)
    }
  }
}
