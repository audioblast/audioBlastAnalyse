#' @importFrom DBI dbExecute dbQuoteString dbQuoteIdentifier dbSendStatement dbGetQuery
deleteAnalysis <- function(db, table, source, id){
    abdbExecute(
      db,
      paste0("DELETE FROM `",table,"` WHERE source = '",source,"' AND id='",id,"'")
      )
}

insertAnalysis <- function(db, table, source, id, duration, startTime, result){
  for (channel in 1:length(result)) {
    sql <- paste0("INSERT INTO `", table, "` VALUES ('",source,"', '",id,"', '", duration, "', ",channel, ", ", startTime,", '", result[channel],"') ON DUPLICATE KEY UPDATE `value` = '", result[channel], "';")
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

fetchDownloadableRecordings <- function(db, source, process_id, legacy=FALSE) {
  if (legacy==TRUE) {
    sql <- paste0("SELECT `id` FROM `tasks` WHERE `source` = '", source, "' ORDER BY RAND() LIMIT 1;")
    ss <- abdbGetQuery(db, sql)
    sql <- paste0("INSERT INTO `tasks-progress`(`process`, `started`, `source`, `id`, `task`) ",
                  "SELECT '", process_id, "', NOW(), `tasks`.`source`, `tasks`.`id`, `tasks`.`task` ",
                  "FROM `tasks` LEFT JOIN `tasks-progress` ",
                  "ON `tasks`.`source` = `tasks-progress`.`source` AND `tasks`.`id` = `tasks-progress`.`id` ",
                  "WHERE `tasks`.`source` = '", source, "' ",
                  "AND `tasks`.`id` = '", ss[1, "id"], "' ",
                  "AND `tasks-progress`.`started` IS NULL;")
    abdbExecute(db, sql)
    sql <- paste0("SELECT * FROM `tasks-data` WHERE `process` = '", process_id, "';")
    ss <- abdbGetQuery(db, sql)
    return(ss)
  }
  sql <- paste0("CALL `get-tasks-by-file`(",
                dbQuoteString(db, process_id), ",",
                dbQuoteString(db, source), ");")
  ss <- abdbGetQuery(db, sql)
  return(ss)
}

fetchUnanalysedRecordings <- function(db, source, process_id, legacy=FALSE) {
  #Legacy provides a means of running on NHM HPC
  if (legacy==FALSE) {
    sql <- paste0("CALL `get-tasks`(",
                  dbQuoteString(db, process_id),
                  ", 10, ",
                  dbQuoteString(db, source), ");")
    ss <- abdbGetQuery(db, sql)
    return(ss)
  } else {
    sql <- paste0("INSERT INTO `tasks-progress`(`process`, `started`, `source`, `id`, `task`) ",
                  "SELECT '", process_id, "', NOW(), `tasks`.`source`, `tasks`.`id`, `tasks`.`task` ",
                  "FROM `tasks` LEFT JOIN `tasks-progress` ",
	                "ON `tasks`.`source` = `tasks-progress`.`source` ",
                  "AND `tasks`.`id` = `tasks-progress`.`id` ",
                  "WHERE `tasks`.`source` = '", source, "' ",
                  "AND `tasks-progress`.`started` IS NULL ",
                  "ORDER BY RAND() ",
                  "LIMIT 10;")
    abdbExecute(db, sql)
    sql <- paste0("SELECT * FROM `tasks-data` WHERE `process` = '", process_id, "';")
    ss <- abdbGetQuery(db, sql)
    return(ss)
  }

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

