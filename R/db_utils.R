#' @importFrom DBI dbExecute dbQuoteString dbQuoteIdentifier dbSendStatement dbGetQuery
deleteAnalysis <- function(db, table, source, id){
  abdbExecute(
    db,
    paste0("DELETE FROM `",table,"` WHERE source = '",source,"' AND id='",id,"'")
    )
}

#' Delete all analyses for a recording
#'
#' @param db A database connection
#' @param source Source of recording
#' @param id ID of recording
#' @param justR If FALSE delete all analyses for recording. If TRUE (default) only
#'   delete analyses made by this package.
deleteAllAnalyses <- function(db, source, id, justR=TRUE) {
  deleteAnalysis(db, "analysis-aci", source, id)
  deleteAnalysis(db, "analysis-adi", source, id)
  deleteAnalysis(db, "analysis-bedoya", source, id)
  deleteAnalysis(db, "analysis-bi", source, id)
  deleteAnalysis(db, "analysis-evenness", source, id)
  deleteAnalysis(db, "analysis-H", source, id)
  deleteAnalysis(db, "analysis-M", source, id)
  deleteAnalysis(db, "analysis-ndsi", source, id)
  deleteAnalysis(db, "analysis-sh", source, id)
  deleteAnalysis(db, "analysis_sec-tdsc", source, id)
  deleteAnalysis(db, "analysis-th", source, id)

  if (justR==FALSE) {
    #Where the recording's peaks are is held beside its measurements, not in a
    #table of its own
    abdbExecute(db, "UPDATE `recordings-calculated` SET `peaks_url` = NULL WHERE `source` = ? AND `id` = ?;",
                params=list(source, id))
  }
}

insertAnalysis <- function(db, table, source, id, startTime, result, complete){
  for (channel in 1:length(result)) {
    sql <- paste0("INSERT INTO `", table, "` VALUES ('",source,"', '",id,"', ", channel, ", ", startTime,", '", result[channel],"', ",complete, ") ON DUPLICATE KEY UPDATE `value` = '", result[channel], "';")
    abdbExecute(db, sql)
  }
}

fetchRecordingDebug <- function(db, source, id) {
  sql <- paste0("SELECT * FROM `recordings` WHERE `source`=",
                dbQuoteString(db, source),
                " AND `id` = ",
                dbQuoteString(db,id), ";")
  ss <- abdbGetQuery(db, sql)
  return(ss)
}

#Has the connection speak UTF-8, as the database does.
#
#A connection speaks whatever the client and server settle on when it is
#made, which can be Latin-1: then every value is turned into Latin-1 on its way
#out, a recording whose id holds an umlaut reaches the agent as bytes that are
#not UTF-8 and cannot be read as text, and a character Latin-1 does not have
#comes back as a question mark, so that measurements would be written, and
#tasks crossed off, under an id that is not the recording's.
speakUtf8 <- function(db) {
  return(abdbExecute(db, "SET NAMES utf8mb4;"))
}

#This agent, as `tasks-agents` names it. That table is PRIMARY KEY (`task`),
#so a task belongs to one agent and no second R agent can be registered
#alongside this one: which of abaR's tasks an agent actually does is said by
#asking for them, not by calling itself something else.
agentName <- function() {
  return("abaR")
}

#The kinds of task this agent does. Anything else it were offered would be
#claimed and given straight back, which is work for the database and none for
#the agent: of the tasks abaR is registered for, most are soundscapes that
#doTask() no longer runs. Waveform peaks are asked for only where audiowaveform
#is installed, for the same reason: an agent without it would give back every
#one it claimed.
tasksDone <- function() {
  return(c("recordings_calculated", if (hasAudiowaveform()) "waveform_peaks"))
}

#The kinds of task to ask for, written as FIND_IN_SET reads them. It matches
#the list exactly, so a space after a comma would quietly cost the agent work.
taskList <- function(tasks) {
  if (length(tasks) == 0) stop("tasks must name at least one kind of task.")
  return(paste(gsub(" ", "", tasks), collapse=","))
}

#No work, in the shape heldBy() answers in. A claim that could not be made at
#all gives this rather than nothing, so that an agent asks again instead of
#falling over on an answer that is not there.
noTasks <- function() {
  return(data.frame(source=character(), id=character(), file=character(),
                    type=character(), Duration=numeric(), task=character(),
                    process=character(), rec_key=integer(), stringsAsFactors=FALSE))
}

#What the database answered, or no work if it could not be asked
claimed <- function(ss) {
  if (is.null(ss) || !is.data.frame(ss)) return(noTasks())
  return(ss)
}

#Claiming is done by the database and read back by the agent, in two steps
#rather than one.
#
#`claim-tasks` and `claim-tasks-by-file` take the work and answer with nothing;
#the agent then reads what it was given with a plain SELECT of its own. It is
#answering with rows that the client build on the NHM HPC cannot be relied on
#for, not the procedure itself -- which is why `delete-task`, answering with
#nothing, has always been called there. Splitting it this way means one path
#for every agent everywhere, and the claim itself stays in the database, where
#the agents written in other languages use the same one.
#
#Claiming is a race the database settles rather than one the agents avoid.
#`tasks-progress` is unique on (source, id, task), so several agents may try
#for the same task and only one row survives; INSERT IGNORE lets the losers
#carry on rather than fail. Each agent then reads back what it actually won,
#which may be fewer than it asked for, and may be none. Two agents doing one
#task would be wasteful rather than wrong in any case, since the measurements
#replace rather than repeat.

#Every outstanding task of one recording, claimed together, so that a recording
#fetched over the web is downloaded once and measured for all of them.
fetchDownloadableRecordings <- function(db, source, process_id, tasks=tasksDone()) {
  abdbExecute(db, "CALL `claim-tasks-by-file`(?, ?, ?, ?);",
              params=list(as.character(process_id), as.character(source),
                          agentName(), taskList(tasks)))
  return(heldBy(db, process_id))
}

#Tasks to be getting on with, claimed so that no other agent is given them at
#the same time.
fetchUnanalysedRecordings <- function(db, source, process_id, tasks=tasksDone(),
                                      n=10) {
  abdbExecute(db, "CALL `claim-tasks`(?, ?, ?, ?, ?);",
              params=list(as.character(process_id), as.integer(n),
                          as.character(source), agentName(), taskList(tasks)))
  return(heldBy(db, process_id))
}

#What this agent came away with: each task it holds, once, with the file of the
#recording it is of. A plain SELECT, so that it does not matter what the client
#library was built against.
#
#It is read from the claims themselves rather than from `tasks-data`, which
#joins them to `recordings`. That view hides a claim on a task whose recording
#has been deleted, which the agent then neither did nor cleared: it sat claimed
#for five minutes, was offered to the next agent, and came back empty again, and
#a run of them stopped agents as though there were no work left. Here such a
#claim comes back with no recording, its `rec_key` NULL, for clearDeleted() to
#clear. The view also gave a task held twice over in `tasks` twice, so that it
#was done twice.
heldBy <- function(db, process_id) {
  return(claimed(abdbGetQuery(
    db,
    paste("SELECT `tasks-progress`.`source`, `tasks-progress`.`id`, `recordings`.`file`,",
          "`recordings`.`type`, `recordings`.`Duration`, `tasks-progress`.`task`,",
          "`tasks-progress`.`process`, `recordings`.`rec_key`",
          "FROM `tasks-progress` LEFT JOIN `recordings`",
          "ON `recordings`.`source` = `tasks-progress`.`source`",
          "AND `recordings`.`id` = `tasks-progress`.`id`",
          "WHERE `tasks-progress`.`process` = ?;"),
    params=list(as.character(process_id)))))
}

#Whether a recording has been deleted from `recordings`, as one may be after its
#tasks were set, or while an agent holds them. Only a database that answers,
#and answers that there is no such recording, says so: one that could not be
#asked says nothing, as clearing a task on its say-so would lose work that is
#still to be done.
recordingGone <- function(db, source, id) {
  found <- abdbGetQuery(
    db,
    "SELECT `rec_key` FROM `recordings` WHERE `source` = ? AND `id` = ? LIMIT 1;",
    params=list(as.character(source), as.character(id)))
  return(is.data.frame(found) && nrow(found) == 0)
}

#The tasks claimed, less those of recordings that have been deleted, which are
#cleared. Such a task can never be done -- there is no file to fetch and nothing
#to measure -- so it is not given back for the next agent to trip over, nor
#counted as having failed: it is crossed off, and the agent gets on with the
#rest. A recording withdrawn because it may not be shared is then neither
#downloaded nor measured.
clearDeleted <- function(db, ss, process, verbose=FALSE) {
  gone <- is.na(ss$rec_key)
  for (i in which(gone)) {
    if (verbose) {
      print(paste("Recording deleted, task cleared:", ss[[i, "source"]], ss[[i, "id"]],
                  ss[[i, "task"]]))
    }
    deleteToDo(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "task"]], process)
  }
  return(ss[!gone, , drop=FALSE])
}

#Crosses a task off: it has been done, and nobody need do it again.
deleteToDo <- function(db, source, id, task, process) {
  abdbExecute(
    db,
    "CALL `delete-task`(?, ?, ?, ?);",
    params=list(as.character(process), as.character(source), as.character(id),
                as.character(task)))
}

#Gives a task back, so that it is offered to whoever asks next rather than
#held by an agent that is not going to do it. Only the claim on the task is
#removed; the task itself stays to be done.
#
#A claim is never given up by itself: a task left in the hands of an agent
#that has stopped, or that does not do that kind of work, is counted as being
#done and is offered to nobody.
releaseToDo <- function(db, source, id, task, process) {
  abdbExecute(
    db,
    paste("DELETE FROM `tasks-progress`",
          "WHERE `process` = ? AND `source` = ? AND `id` = ? AND `task` = ?;"),
    params=list(as.character(process), as.character(source), as.character(id),
                as.character(task)))
}

