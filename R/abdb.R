backoff <- function() {
  return(c(1,2,3,5,10,30,60))
}

abdbExecute <- function(db, query) {
  for (i in backoff()) {
    ret <- tryCatch({
      dbExecute(db, query)
    },
    error=function(cond) {
      return(FALSE)
    })
    if (ret != FALSE) {
      return(ret)
    } else {
      Sys.sleep(i)
    }
  }
}

abdbGetQuery <- function(db, query) {
  for (i in backoff()) {
    ret <- tryCatch({
      dbGetQuery(db, query)
    },
    error=function(cond) {
      return(FALSE)
    })
    if (ret != FALSE) {
      return(ret)
    } else {
      print(paste(Sleep:", i"))
      Sys.sleep(i)
    }
  }
}
