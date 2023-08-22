backoff <- function() {
  return(c(1,1,2,3,5,10,30,60))
}

abdbExecute <- function(db, query) {
  for (i in backoff()) {
    ret <- tryCatch({
      dbExecute(db, query)
    },
    error=function(cond) {
      print("Error:")
      print(cond)
      -1
    })
    if (ret >= 0) {
      return()
    } else {
      print(paste("Sleep:", i))
      Sys.sleep(i)
      next()
    }
    stop("Too many retires of query")
  }
  invisible(query)
}

abdbGetQuery <- function(db, query) {
  print(query)
  for (i in backoff()) {
    ret <- tryCatch({
      dbGetQuery(db, query)
    },
    error=function(cond) {
      print("Error:")
      print(cond)
      FALSE
    })
    if (ret == FALSE) {
      print(paste("Sleep:", i))
      Sys.sleep(i)
    } else {
      return(ret)
    }
  }
}
