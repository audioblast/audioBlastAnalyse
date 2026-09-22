backoff <- function() {
  return(c(1,1,2,3,5,10,30,60))
}

#A statement, retried behind the backoff above while the database refuses it,
#as an agent that loses its connection should wait for it rather than lose the
#work it has done. Values are bound rather than written into the statement, so
#that a recording whose id holds a quote is written as itself.
#
#Returns TRUE once the statement succeeds, and FALSE with a warning once the
#retries are spent: a caller that must know whether its work was kept can ask.
abdbExecute <- function(db, query, params=NULL) {
  for (i in backoff()) {
    ret <- tryCatch({
      if (is.null(params)) dbExecute(db, query) else dbExecute(db, query, params=params)
    },
    error=function(cond) {
      print("Error:")
      print(cond)
      -1
    })
    if (!is.na(ret) && ret >= 0) {
      return(invisible(TRUE))
    }
    print(paste("Sleep:", i))
    Sys.sleep(i)
  }
  warning(paste("Gave up on a statement after", length(backoff()), "attempts:", query))
  return(invisible(FALSE))
}

#A query, retried as abdbExecute() retries a statement, giving what the
#database answered. Returns NULL with a warning once the retries are spent,
#which a caller must tell apart from a query that matched nothing.
abdbGetQuery <- function(db, query, params=NULL) {
  #A value no query can answer with, so that a query answering FALSE, or
  #nothing at all, is not read as having failed
  failed <- new.env()
  for (i in backoff()) {
    ret <- tryCatch({
      if (is.null(params)) dbGetQuery(db, query) else dbGetQuery(db, query, params=params)
    },
    error=function(cond) {
      print("Error:")
      print(cond)
      failed
    })
    if (!identical(ret, failed)) {
      return(ret)
    }
    print(paste("Sleep:", i))
    Sys.sleep(i)
  }
  warning(paste("Gave up on a query after", length(backoff()), "attempts:", query))
  return(NULL)
}
