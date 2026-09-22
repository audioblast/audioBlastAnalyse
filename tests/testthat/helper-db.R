#Runs code against a mocked database, returning what it did: the statements it
#executed, each with its SQL and the parameters bound to it, and the queries it
#asked, in the order it made them. A query is answered by the next of the rows,
#so that a test says what the database holds without one running.
#
#Statements are retried behind a backoff (see abdbExecute()), which is mocked
#to no delay: a test of a failing statement must not wait out its retries.
mockDB <- function(code, rows=list()) {
  executed <- list()
  queried <- character()
  if (is.data.frame(rows)) rows <- list(rows)

  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      executed[[length(executed) + 1]] <<- list(sql=statement, params=params)
      1L
    },
    dbGetQuery=function(conn, statement, params=NULL, ...) {
      queried <<- c(queried, statement)
      if (length(rows) == 0) return(noRows())
      return(rows[[min(length(queried), length(rows))]])
    },
    backoff=function() c(0, 0))

  value <- code
  return(list(value=value, executed=executed, queried=queried))
}

#What a query answers when it matches nothing
noRows <- function() {
  return(data.frame())
}

#The rows of a table a query answers with, named by their columns
someRows <- function(...) {
  return(data.frame(..., stringsAsFactors=FALSE, check.names=FALSE))
}

#The one statement a mocked database was asked to execute, or the nth of them
onlyStatement <- function(mocked, n=1) {
  if (length(mocked$executed) == 0) stop("No statement was executed")
  return(mocked$executed[[n]])
}
