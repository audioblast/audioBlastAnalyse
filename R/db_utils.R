deleteAnalysis <- function(db, table, source, id){
  tryCatch({
    dbExecute(db, paste0("DELETE FROM `",table,"` WHERE source = '",source,"' AND id=",id))
  }, error = function(e) {
    print(e)
  })
}

analysedRowCount <- function(db, table, source, id){
  tryCatch({
    sql <- paste0("SELECT `source`, `id` FROM `", table, "` WHERE `source` = '",source,"' AND id=",id)
    res <- dbSendQuery(db, sql)
    dbFetch(res)
    return(dbGetRowCount(res))
  }, error = function(e) {
    print(e)
  })
}

rowAnalysed <- function(db, table, source, id, startTime){
  tryCatch({
    sql <- paste0("SELECT `source`, `id` FROM `",table,"` WHERE `source` = '",source,"' AND id=",id," AND startTime = ",startTime)
    print(sql)
    res <- dbSendQuery(db, sql)
    dbFetch(res)
    if (dbGetRowCount(res) != 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    print(e)
  })
}

insertAnalysis <- function(db, table, source, id, duration, startTime, result){
  tryCatch({
    print(sql)
    sql <- paste0("INSERT INTO `", table, "` VALUES ('",source,"', '",id,"', '", duration, "', ",startTime,", '", result,"')")
    dbExecute(db, sql)
  }, error = function(e) {
    print(e)
  })
}

