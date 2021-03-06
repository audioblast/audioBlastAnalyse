deleteAnalysis <- function(db, table, source, id){
    dbExecute(
      db,
      paste0("DELETE FROM `",table,"` WHERE source = '",source,"' AND id='",id,"'")
      )
}

analysedRowCount <- function(db, table, source, id){
    sql <- paste0("SELECT COUNT(*) FROM `", table, "` WHERE `source` = '",source,"' AND id='",id,"'")
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
    sql <- paste0("INSERT INTO `", table, "` VALUES ('",source,"', '",id,"', '", duration, "', ",startTime,", '", result,"')")
    dbExecute(db, sql)
}

fetchDownloadableRecordings <- function(db) {
  res <- dbSendQuery(
    db,
    paste0("SELECT source, id, `file`, `type`, Duration FROM `audioblast`.`recordings` WHERE `file` LIKE 'http%';")
    )
  ss <- dbFetch(res)
  dbClearResult(res)
  return(ss)
}

fetchRecordingsFromSource <- function(db, sense) {
  res <- dbSendQuery(
    db,
    paste0("SELECT source, id, `file`, `type`, Duration FROM `audioblast`.`recordings` WHERE `source` = '",sense,"';")
  )
  ss <- dbFetch(res)
  dbClearResult(res)
  return(ss)
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
