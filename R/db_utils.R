#' @importFrom DBI dbExecute dbQuoteString dbQuoteIdentifier dbSendStatement dbGetQuery
deleteAnalysis <- function(db, table, source, id){
    abdbExecute(
      db,
      paste0("DELETE FROM `",table,"` WHERE source = '",source,"' AND id='",id,"'")
      )
}

analysedRowCount <- function(db, table, source, id){
    sql <- paste0("SELECT COUNT(*) FROM `", table, "` WHERE `source` = '",source,"' AND `id`='",id,"'")
    res <- dbSendQuery(db, sql)
    a <- dbFetch(res)[[1,1]]
    dbClearResult(res)
    return(a)
}

rowAnalysed <- function(db, table, source, id, startTime, duration=NULL){
  if (is.null(duration)) {
    sql <- paste0("SELECT `source`, `id` FROM `",table,"` WHERE `source` = '",source,"' AND id='",id,"' AND startTime = ",startTime)
  } else {
    sql <- paste0("SELECT `source`, `id` FROM `",table,"` WHERE `source` = '",source,"' AND id='",id,"' AND startTime = ",startTime," AND duration = ",duration)
  }
  res <- dbSendQuery(db, sql)
  dbFetch(res)
  rc <- dbGetRowCount(res)
  dbClearResult(res)
  if (rc != 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

insertAnalysis <- function(db, table, source, id, duration, startTime, result){
  for (channel in 1:length(result)) {
    sql <- paste0("INSERT INTO `", table, "` VALUES ('",source,"', '",id,"', '", duration, "', ",channel, ", ", startTime,", '", result[channel],"') ON DUPLICATE KEY UPDATE `value` = '", result[channel], "';")
    abdbExecute(db, sql)
  }
}

fetchDownloadableRecordings <- function(db, source, process_id) {
  sql <- paste0("CALL `get-tasks-by-file`(",
                dbQuoteString(db, process_id), ",",
                dbQuoteString(db, source), ");")
  ss <- abdbGetQuery(db, sql)
  return(ss)
}

fetchUnanalysedRecordings <- function(db, source, process_id) {
  sql <- paste0("CALL `get-tasks`(",
                dbQuoteString(db, process_id),
                ", 10, ",
                dbQuoteString(db, source), ");")
  ss <- abdbGetQuery(db, sql)
  return(ss)
}

deleteToDo <- function(db, source, id, task, process) {
  sql <- paste0("CALL `delete-task`(",
                dbQuoteString(db, process), ", ",
                dbQuoteString(db, source), ", ",
                dbQuoteString(db, id), ", ",
                dbQuoteString(db, task), ");"
                )
  abdbExecute(db, sql)
}

fetchSoundscapes <- function(db) {
  sql <- "SELECT * FROM audioblast.soundscapes
	          LEFT JOIN `soundscape-metadata`
            ON `soundscape-metadata`.`source` = `soundscapes`.`source`
		        AND `soundscape-metadata`.`id` = `soundscapes`.`id`;"
  res <- dbSendQuery(db, sql)
  ss <- dbFetch(res)
  dbClearResult(res)
  return(ss)
}
