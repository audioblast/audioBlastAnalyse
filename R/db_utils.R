deleteAnalysis <- function(db, table, source, id){
    dbExecute(db, paste0("DELETE FROM `",table,"` WHERE source = '",source,"' AND id=",id))
}

analysedRowCount <- function(db, table, source, id){
    sql <- paste0("SELECT `source`, `id` FROM `", table, "` WHERE `source` = '",source,"' AND id=",id)
    res <- dbSendQuery(db, sql)
    dbFetch(res)
    rc <- dbGetRowCount(res)
    dbClearResult(res)
    return(rc)
}

rowAnalysed <- function(db, table, source, id, startTime){
    sql <- paste0("SELECT `source`, `id` FROM `",table,"` WHERE `source` = '",source,"' AND id=",id," AND startTime = ",startTime)
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

