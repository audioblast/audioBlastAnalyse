#' Run all analyses
#'
#' @param db database connector
#' @param mode "web" for online files, or name of source to analyse locally
#' @param verbose Gives verbose output if TRUE
#' @param force Forces recalculation of analyses if TRUE
#' @param base_dir Directory relative paths are located in
#' @importFrom tools file_ext
#' @importFrom DBI dbDisconnect dbConnect
#' @export
analyse <- function(db, mode="web", verbose=FALSE, force=FALSE, base_dir="") {
  db <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)
  if (mode=="web") {
    #Todo: use todo-* queries
    ss <- fetchDownloadableRecordings(db)
    for (i in 1:nrow(ss)) {
      tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
    }
  } else {
    #Missing durations
    ss <-fetchUnanalysedRecordings(db, mode, "todo-hash_duration")
    cn <- colnames(ss)
    ss <- cbind(ss, rep_len(mode, nrow(ss)), rep_len(verbose, nrow(ss)), rep_len(force, nrow(ss)), rep_len(base_dir, nrow(ss)))
    colnames(ss) <- c(cn, "mode", "verbose", "force", "base_dir")
    if (verbose) {print("Calculated properties of recordings");}
    for (i in 1:nrow(ss)) {
      tmp <- paste0(base_dir,ss[i, "file"])
      tryCatch({
        recordings_calculated(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
    }

    #Soundscapes by minute
    ss <-fetchUnanalysedRecordings(db, mode, "todo-sm")
    cn <- colnames(ss)
    ss <- cbind(ss, rep_len(mode, nrow(ss)), rep_len(verbose, nrow(ss)), rep_len(force, nrow(ss)), rep_len(base_dir, nrow(ss)))
    colnames(ss) <- c(cn, "mode", "verbose", "force", "base_dir")
    for (i in 1:nrow(ss)) {
      tmp <- paste0(base_dir,ss[i, "file"])
      tryCatch({
        soundscapes_by_minute(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
    }
  }
  dbDisconnect(db)
  return();

  #Historic below - to be incorporated

  dbDisconnect(db)
  for (i in 1:nrow(ss)) {
    db <- DBI::dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)

    if (file_ext(ss[i, "file"]) == "ogg") next()
    if (file_ext(ss[i, "file"]) == "zc") next()


    if (verbose) {print(ss[i, "file"]);}

    if (verbose) {print("TDSC");}
    #tryCatch({
    #  a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
    #})

    if (mode == "web") {
      unlink(tmp)
    }
    dbDisconnect(db)
  }
}
