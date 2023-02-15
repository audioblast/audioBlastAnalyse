backoff <- function() {
  return(c(1,2,3,5,10,30,60))
}

abdbExecute <- function(db, query) {
  for (i in backoff()) {
    ret <- tryCatch({
      return(dbExecute(db, query))
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
      return(dbGetQuery(db, query))
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
