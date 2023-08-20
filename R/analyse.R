#' Run audioBlast analyses
#'
#' This is the main function for analysing recordings from audioBlast in the R
#' environment.
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
    retain=FALSE,
    sleep = NULL
    ) {

  # Generate a unique process_id. This is used to identify this analysis process to
  # the audioBlast database when assigning outstanding analysis tasks to this process.
  process_id <- hash_sha256(as.numeric(Sys.time())+Sys.getpid())

  cont <- TRUE
  while (cont) {
    if (mode=="web") {
      # The web mode is used to analyse files from a website (such as
      # https://bio.acousti.ca). Files must be downloaded before analysis (and may
      # be retained if retain=TRUE).
      if (debug) {
        # Debug mode is used to debug an individual recording
        ss <- fetchRecordingDebug(db, source, id)
      } else {
        # For a recording with 1 or more oustanding tasks, get all oustanding
        # tasks for that recording.
        ss <- fetchDownloadableRecordings(db, source, process_id, legacy=db_legacy)
        if (nrow(ss) == 0) {
          if (is.null(sleep)) {
            # Stop further execution
            cont <- FALSE;
          } else {
            # Sleep for sleep seconds before checking for new tasks
            Sys.sleep(sleep)
          }
        }
      }
    } else {
      # Files for analysis are mounted locally
      if (debug) {
        # Debug mode is used to debug an individual recording
        ss <- fetchRecordingDebug(db, source, id)
      } else {
        # Fetch 10 outstanding tasks on locally mounted files
        ss <-fetchUnanalysedRecordings(db, source, process_id, legacy=db_legacy)
        if (nrow(ss) == 0) {
          if (is.null(sleep)) {
            # Stop further execution
            cont <- FALSE;
          } else {
            # Sleep for sleep seconds before checking for new tasks
            Sys.sleep(sleep)
          }
        }
      }
    }
    if (mode=="web") {
      # Download file into system temp directory
      tmp <- paste0(tempfile(), ".", file_ext(ss[1, "file"]))
      dl_file(ss[1, "file"], tmp)
    }

    if (nrow(ss)>0) {
      for (i in 1:nrow(ss)) {
        if (mode=="local") {
          # tmp is path to file
          tmp <- paste0(base_dir,ss[i, "file"])
        }
        if (debug) {
          # In dbeug mode redo all analyses for checking
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
        # Move downloaded temp file to base_dir with appropriate filename
        nfn <- paste0(paste(ss[[i, "source"]], ss[[i, "id"]], sep="_"),".",file_ext(ss[1, "file"]))
        file.copy(tmp, paste(base_dir, nfn, sep="/"))
      }
      # Delete temporary file
      unlink(tmp)
    }
    if (debug) {
      # Debug mode is for debugging a single recording
      cont <- FALSE
    }
  }
  dbDisconnect(db)
  return();
}
