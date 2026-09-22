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
#' @param base_dir Directory the recordings are in: the directory relative
#'   paths are located in, and the one downloaded files are kept in. A
#'   downloaded recording is kept under the source and id it is held by, so
#'   that it is fetched once however many times it is analysed, and however
#'   many agents analyse it on one machine.
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
    base_dir=""
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

  # Generate a unique process_id. This is used to identify this analysis process to
  # the audioBlast database when assigning outstanding analysis tasks to this process.
  process_id <- hash_sha256(as.numeric(Sys.time())+Sys.getpid())

  cont <- TRUE
  while (cont) {
    if (debug) {
      # Debug mode is used to debug an individual recording
      ss <- fetchRecordingDebug(db, source, id)
    } else if (mode=="web") {
      # The web mode is used to analyse files from a website (such as
      # https://bio.acousti.ca). Files are downloaded before analysis, and kept
      # in base_dir so that a recording is only ever downloaded once. For a
      # recording with 1 or more outstanding tasks, get all outstanding tasks
      # for that recording.
      ss <- fetchDownloadableRecordings(db, source, process_id, legacy=db_legacy)
    } else {
      # Files for analysis are mounted locally: fetch 10 outstanding tasks
      ss <- fetchUnanalysedRecordings(db, source, process_id, legacy=db_legacy)
    }
    # The tasks claimed above are all of one recording, so its file is fetched
    # once and read by each of them. Nothing is fetched when nothing was
    # claimed: there is then no recording to fetch the file of.
    if (mode=="web" && nrow(ss) > 0) {
      tmp <- webFile(ss[1, "file"], ss[[1, "source"]], ss[[1, "id"]], base_dir, verbose)
    }

    if (nrow(ss)>0) {
      for (i in 1:nrow(ss)) {
        if (verbose) print(paste("ID: ", ss[i, "id"]))
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
        doTask(db, task, ss[[i, "source"]], ss[[i, "id"]], tmp, process_id, force, verbose)
      }
    } else {
      if (verbose) print("No outstanding tasks")
      cont <- FALSE
    }
    if (debug) {
      # Debug mode is for debugging a single recording
      cont <- FALSE
    }
  }
  return();
}

#Does one of the tasks an agent has claimed, and settles the claim on it: a
#task that has been done is crossed off, and one that has not is given back for
#another agent to do.
#
#Only recordings_calculated is done for now. The soundscape analyses are still
#here as functions, and can be called directly, but no longer run from a
#claimed task: the per-minute analyses are being dropped, and what becomes of
#the rest is not settled.
#
#A task of any other kind is given back rather than passed over. An agent that
#claims a task it will not do would otherwise hold it for good, and the task
#would be counted as being in hand while nobody was doing it.
doTask <- function(db, task, source, id, path, process, force=FALSE, verbose=FALSE) {
  if (!identical(task, "recordings_calculated")) {
    warning(paste0("Not a task this agent does, and given back: ", task))
    releaseToDo(db, source, id, task, process)
    return(invisible("released"))
  }

  if (verbose) print("Recordings calculated")
  outcome <- recordings_calculated(db, source, id, path, force, verbose)

  #Measurements the database would not keep are worth making again, so the
  #task goes back. Anything else is done with: a recording that cannot be read
  #will not read any better for being measured twice.
  if (outcome == "retry") {
    releaseToDo(db, source, id, task, process)
    return(invisible("released"))
  }
  deleteToDo(db, source, id, task, process)
  return(invisible(outcome))
}
