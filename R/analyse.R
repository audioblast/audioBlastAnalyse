#' Run audioBlast analyses
#'
#' This is the main function for analysing recordings from audioBlast in the R
#' environment.
#'
#' @param db database connector
#' @param db_legacy Deprecated, and does nothing. Claiming no longer needs a
#'   second path: the database is asked to claim with a routine that answers
#'   with nothing, and the agent reads what it was given itself, which works
#'   whatever the client library was built against.
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
#' @param tasks The kinds of task to ask for. An agent is only offered these,
#'   rather than everything abaR is registered for, so that it does not claim
#'   work it would only give straight back. Defaults to what doTask() does.
#' @param n How many tasks to claim at a time. A task is given back by itself
#'   if the agent stops, so this can be raised: finishing any one task tells
#'   the database the agent is still alive and holding the rest.
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
    tasks=tasksDone(),
    n=10
    ) {

  # Parameters check
  if (!inherits(db, "MariaDBConnection")) stop("db is not a MariaDBConnection.")
  if (!is.logical(db_legacy)) stop("db_legacy must be logical.")
  if (isTRUE(db_legacy)) {
    warning(paste("db_legacy is deprecated and does nothing: claiming now works",
                  "the same way everywhere. It can be dropped from the call."))
  }
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
  if (!is.character(tasks)) stop("tasks must be a character vector.")
  if (length(tasks) == 0) stop("tasks must name at least one kind of task.")
  if (!is.numeric(n) || length(n) != 1 || n < 1) stop("n must be a number of tasks, and at least 1.")

  # Generate a unique process_id. This is used to identify this analysis process to
  # the audioBlast database when assigning outstanding analysis tasks to this process.
  process_id <- hash_sha256(as.numeric(Sys.time())+Sys.getpid())

  speakUtf8(db)

  cont <- TRUE
  empties <- 0
  #How many times each recording has failed to download, by source and id
  failures <- numeric()
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
      ss <- fetchDownloadableRecordings(db, source, process_id, tasks=tasks)
    } else {
      # Files for analysis are mounted locally
      ss <- fetchUnanalysedRecordings(db, source, process_id, tasks=tasks, n=n)
    }
    # The tasks claimed above are all of one recording, so its file is fetched
    # once and read by each of them. Nothing is fetched when nothing was
    # claimed: there is then no recording to fetch the file of.
    given_back <- FALSE
    if (mode=="web" && nrow(ss) > 0) {
      type <- if ("type" %in% names(ss)) ss[[1, "type"]] else NA_character_
      tmp <- webFile(ss[1, "file"], ss[[1, "source"]], ss[[1, "id"]], base_dir, verbose, type)
      if (is.na(tmp)) {
        recording <- paste(ss[[1, "source"]], ss[[1, "id"]])
        #A recording being debugged has no claimed tasks to give back
        if (debug) stop(paste("Could not download", recording))
        failures[recording] <- if (is.na(failures[recording])) 1 else failures[recording] + 1
        if (failures[recording] < downloadAttempts()) {
          #Nothing is known about the recording that was not before, so its
          #tasks go back to be claimed again, after a wait for whatever went
          #wrong to come right
          warning(paste("Could not download", recording, "and gave its tasks back"))
          for (i in seq_len(nrow(ss))) {
            releaseToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process_id)
          }
          ss <- ss[0, , drop=FALSE]
          given_back <- TRUE
          pause(spread(10 * failures[recording]))
        } else {
          #A recording that cannot be downloaded however often it is asked for
          #is measured at its address, where there is no file: it is recorded
          #as missing, with its address in the error, rather than holding the
          #agent for ever
          tmp <- ss[1, "file"]
        }
      }
    }

    if (nrow(ss)>0) {
      empties <- 0
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
    } else if (!debug && !given_back) {
      #A claim that came back empty is not yet a sign that the work is done:
      #another agent may have won every task this one tried for, or the claim
      #may have failed. So the agent asks again, a while later, and only stops
      #when asking has come back empty for long enough.
      empties <- empties + 1
      wait <- emptyClaimWait(empties)
      if (is.na(wait)) {
        if (verbose) print("No outstanding tasks")
        cont <- FALSE
      } else {
        if (verbose) print(paste("Nothing claimed, asking again in", round(wait), "s"))
        pause(wait)
      }
    }
    if (debug) {
      # Debug mode is for debugging a single recording
      cont <- FALSE
    }
  }
  return();
}

#How long an agent waits before claiming again, after the given number of
#claims in a row have come back empty, or NA once that has happened often
#enough to believe there is no work left.
#
#Every agent asks for the first unclaimed recording, so agents that ask at
#the same moment are all given the same one, and all but one of them come
#away with nothing. An agent that took that for the end of the work would
#stop while there was still work to do. The waits are spread at random so
#that agents that collided once do not collide again; they add up to about two
#minutes before an agent gives up.
emptyClaimWait <- function(empties) {
  waits <- c(1, 5, 15, 30, 60)
  if (empties > length(waits)) return(NA_real_)
  return(waits[empties] * stats::runif(1, 0.5, 1.5))
}

#How many times an agent tries to download a recording before it records the
#recording as missing
downloadAttempts <- function() {
  return(3)
}

#Waits, as its own function so that tests need not
pause <- function(seconds) {
  Sys.sleep(seconds)
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
