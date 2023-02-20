#' Run all analyses
#'
#' @param db database connector
#' @param mode "web" for online files, or name of source to analyse locally
#' @param source Specify source to analyse
#' @param verbose Gives verbose output if TRUE
#' @param force Forces recalculation of analyses if TRUE
#' @param base_dir Directory relative paths are located in
#' @importFrom tools file_ext
#' @importFrom DBI dbDisconnect dbConnect
#' @importFrom cli hash_sha256
#' @export
analyse <- function(db, mode="local", source="unp", verbose=FALSE, force=FALSE, base_dir="") {
  db <- dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)
  if (mode=="web") {
    #Todo: use todo-* queries
    ss <- fetchDownloadableRecordings(db)
    for (i in 1:nrow(ss)) {
      tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
    }
  } else {
      while (TRUE) {
        process_id <- hash_sha256(Sys.time())
        ss <-fetchUnanalysedRecordings(db, source, process_id)
        if (nrow(ss)>0) {
          for (i in 1:nrow(ss)) {
            tmp <- paste0(base_dir,ss[i, "file"])
            if (ss[i, "task"] == "recordings_calculated") {
              print("Recordings calculated")
              recordings_calculated(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
              deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
            } else if (ss[i, "task"] == "soundscapes_minute") {
              print("Soundscapes minutes")
              soundscapes_by_minute(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
              deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
            } else if (ss[i, "task"] == "soundscapes_second") {
              print("Soundscapes seconds")
              soundscapes_by_second(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
              deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
            }
          }

        }

    }
  }
  dbDisconnect(db)
  return();
}
