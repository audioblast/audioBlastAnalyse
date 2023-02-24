#' Run all analyses
#'
#' @param db database connector
#' @param mode "web" for online files, or "local"
#' @param source Specify source to analyse
#' @param verbose Gives verbose output if TRUE
#' @param force Forces recalculation of analyses if TRUE
#' @param base_dir Directory relative paths are located in
#' @importFrom tools file_ext
#' @importFrom DBI dbDisconnect dbConnect
#' @importFrom cli hash_sha256
#' @importFrom RMariaDB MariaDB
#' @export
analyse <- function(db, mode="local", source="unp", verbose=FALSE, force=FALSE, base_dir="") {
  db <- dbConnect(MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)
  process_id <- hash_sha256(Sys.time())

  while (TRUE) {
    if (mode=="web") {
      ss <- fetchDownloadableRecordings(db, source, process_id)
    } else {
      ss <-fetchUnanalysedRecordings(db, source, process_id)
    }
    #ToDo: check for zero rows

    # Download file if remote
    if (mode=="web") {
      tmp <- paste0(tempfile(), ".", file_ext(ss[1, "file"]))
      dl_file(ss[1, "file"], tmp)
    }

    if (nrow(ss)>0) {
      for (i in 1:nrow(ss)) {
        if (mode=="local") {
          tmp <- paste0(base_dir,ss[i, "file"])
        }
        if (ss[i, "task"] == "recordings_calculated") {
          print("Recordings calculated")
          recordings_calculated(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
        } else if (ss[i, "task"] == "soundscapes_minute") {
          print("Soundscapes minutes")
          soundscapes_by_minute(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
        } else if (ss[i, "task"] == "soundscapes_second") {
          print("Soundscapes seconds")
          soundscapes_by_second(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
        }
      }
    }
    if (mode=="web") {
      unlink(tmp)
    }
  }
  dbDisconnect(db)
  return();
}
