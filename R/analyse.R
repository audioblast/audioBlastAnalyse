#' Run all analyses
#'
#' @param db database connector
#' @param db_legacy If TRUE allows use with RMariaDB connectors that have issues with stored procedures
#' @param mode "web" for online files, or "local" for local files
#' @param source Specify source to analyse
#' @param debug If TRUE and used with id and task then allows for debugging a single recording
#' @param id Specify id of a file within source to analyse (only used with debug=T)
#' @param task Specify a task (for use when debug=T)
#' @param verbose Gives verbose output if TRUE
#' @param force Forces recalculation of analyses if TRUE
#' @param base_dir Directory relative paths are located in
#' @param retain If TRUE will save web files to base_dir
#' @param sleep Number of seconds to sleep after all jobs are complete before
#'   requesting additional work from the database. Default (NULL) cancels the task.
#' @importFrom tools file_ext
#' @importFrom DBI dbDisconnect
#' @importFrom cli hash_sha256
#' @importFrom RMariaDB MariaDB
#' @export
analyse <- function(
    db,
    db_legacy=F,
    mode="local",
    source="unp",
    debug=FALSE,
    id=NULL,
    task=NULL,
    verbose=FALSE,
    force=FALSE,
    base_dir="",
    retain=F,
    sleep = NULL
    ) {
  process_id <- hash_sha256(as.numeric(Sys.time())+Sys.getpid())

  cont <- TRUE
  while (cont) {
    if (mode=="web") {
      if (debug) {
        ss <- fetchRecordingDebug(db, source, id)
      } else {
        ss <- fetchDownloadableRecordings(db, source, process_id, legacy=db_legacy)
        if (nrow(ss) == 0) {
          if (is.null(sleep)) { cont <- FALSE;}
          else { Sys.sleep(sleep)}
        }
      }
    } else {
      if (debug) {
        ss <- fetchRecordingDebug(db, source, id)
      } else {
        ss <-fetchUnanalysedRecordings(db, source, process_id, legacy=db_legacy)
        if (nrow(ss) == 0) {
          if (is.null(sleep)) { cont <- FALSE;}
          else { Sys.sleep(sleep)}
        }
      }
    }
    if (mode=="web") {
      tmp <- paste0(tempfile(), ".", file_ext(ss[1, "file"]))
      dl_file(ss[1, "file"], tmp)
    }

    if (nrow(ss)==0) {
      if (is.null(sleep)) cont <- FALSE
      else Sys.sleep(sleep)
    }

    if (nrow(ss)>0) {
      for (i in 1:nrow(ss)) {
        if (mode=="local") {
          tmp <- paste0(base_dir,ss[i, "file"])
        }
        if (debug) {
          force <- TRUE
        } else {
          task <- ss[[i, "task"]]
        }
        if (task == "recordings_calculated") {
          print("Recordings calculated")
          recordings_calculated(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        } else if (task == "soundscapes_minute") {
          print("Soundscapes minutes")
          soundscapes_by_minute(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        } else if (task == "soundscapes_second") {
          print("Soundscapes seconds")
          soundscapes_by_second(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        }
      }
    }
    if (mode=="web") {
      if (retain) {
        nfn <- paste0(paste(ss[[i, "source"]], ss[[i, "id"]], sep="_"),".",file_ext(ss[1, "file"]))
        file.copy(tmp, paste(base_dir, nfn, sep="/"))
      }
      unlink(tmp)
    }
    if (debug) {
      cont <- FALSE
    }
  }
  dbDisconnect(db)
  return();
}
