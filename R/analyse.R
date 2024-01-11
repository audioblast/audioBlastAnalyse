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
#' @importFrom cli hash_sha256
#' @export
analyse <- function(
    db,
    db_legacy=FALSE,
    mode="local",
    source="unp",
    debug=FALSE,
    id=NULL,
    task=NULL,
    verbose=FALSE,
    force=FALSE,
    base_dir="",
    retain=FALSE,
    sleep = NULL,
    save.path="."
    ) {

  # Parameters check
  if (!inherits(db, "MariaDBConnection")) stop("db is not a MariaDBConnection.")
  if (!is.logical(db_legacy)) stop("db_legacy must be logical.")
  if (!(mode %in% c("local", "web"))) stop("mode must be one of: web, local.")
  if (!is.character(source)) stop("source must be a character vector.")
  if (!is.logical(debug)) stop("debug must be logical.")
  if (debug==FALSE) {
    if (!is.null(id)) warning("id specified when debug=FALSE. This will have no effect.")
    if (!is.null(task)) warning("task specified when debug=FALSE. This will have no effect.")
  }
  if (debug==TRUE) {
    if (!is.character(id)) stop("id must be a character vector.")
    if (is.null(id)) stop("id must be specified when debug=TRUE")
    if (!is.character(task)) stop("task must be a character vector.")
    if (is.null(task)) stop("task must be specified when debug=TRUE")
  }
  if (!is.logical(verbose)) stop("verbose must be logical.")
  if (!is.logical(force)) stop("force must be logical.")
  if (!is.character(base_dir)) stop("base_dir must be a character vector.")
  if (!is.logical(retain)) stop("retain must be logical.")
  if (!(is.null(sleep) || is.numeric(sleep))) stop("sleep must be NULL or numeric")

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
      if (retain==TRUE) {
        nfn <- paste0(paste(ss[[1, "source"]], ss[[1, "id"]], sep="_"),".",file_ext(ss[1, "file"]))
        if (file.exists(nfn)) {
          tmp <- nfn
        } else {
          # Download file into system temp directory
          tmp <- paste0(tempfile(), ".", file_ext(ss[1, "file"]))
          dl_file(ss[1, "file"], tmp)
        }
      } else {
        # Download file into system temp directory
        tmp <- paste0(tempfile(), ".", file_ext(ss[1, "file"]))
        dl_file(ss[1, "file"], tmp)
      }
    }

    if (nrow(ss)>0) {
      for (i in 1:nrow(ss)) {
        print(paste("ID: ", ss[i, "id"]))
        if (mode=="local") {
          # tmp is path to file
          tmp <- paste0(base_dir,ss[i, "file"])
        }
        if (debug) {
          # In debug mode redo all analyses for checking
          force <- TRUE
        } else {
          task <- ss[[i, "task"]]
        }
        if (task == "recordings_calculated") {
          if (verbose) print("Recordings calculated")
          recordings_calculated(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        } else if (task == "soundscapes_minute") {
          if (verbose) print("Soundscapes minutes")
          soundscapes_by_minute(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        } else if (task == "soundscapes_second") {
          if (verbose) print("Soundscapes seconds")
          soundscapes_by_second(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        } else if (task == "soundscapes_spec") {
          soundscapes_spectro(db, ss[[i, "source"]], ss[[i, "id"]], tmp, ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose, save.path)
          deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], task, process_id)
        }
      }
    } else {
      if (verbose) print("No outstanding tasks")
      cont <- FALSE
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
  return();
}
