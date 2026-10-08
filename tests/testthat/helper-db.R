#Runs code against a mocked database, returning what it did: the statements it
#executed, each with its SQL and the parameters bound to it, and the queries it
#asked, in the order it made them. A query is answered by the next of the rows,
#so that a test says what the database holds without one running.
#
#Whether a recording is still there (see recordingGone()) is answered apart from
#the rows: every recording is, but those whose ids are given as deleted. A test
#of the work done on a recording need not then say that the recording exists.
#The ids asked after are kept, in order, as `recordings`.
#
#Statements are retried behind a backoff (see abdbExecute()), which is mocked
#to no delay: a test of a failing statement must not wait out its retries.
mockDB <- function(code, rows=list(), deleted=character()) {
  executed <- list()
  queried <- character()
  recordings <- character()
  if (is.data.frame(rows)) rows <- list(rows)

  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      executed[[length(executed) + 1]] <<- list(sql=statement, params=params)
      1L
    },
    dbGetQuery=function(conn, statement, params=NULL, ...) {
      if (isRecordingQuery(statement)) {
        recordings <<- c(recordings, params[[2]])
        if (params[[2]] %in% deleted) return(noRows())
        return(someRows(rec_key=1L))
      }
      queried <<- c(queried, statement)
      if (length(rows) == 0) return(noRows())
      return(rows[[min(length(queried), length(rows))]])
    },
    backoff=function() c(0, 0))

  value <- code
  return(list(value=value, executed=executed, queried=queried, recordings=recordings))
}

#Whether a query asks if a recording is still there
isRecordingQuery <- function(statement) {
  return(startsWith(statement, "SELECT `rec_key` FROM `recordings`"))
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
