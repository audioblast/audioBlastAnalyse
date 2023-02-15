abdbExecute <- function(db, query) {
  for (i in 1:5) {
    ret <- tryCatch({
      return(dbExecute(db, query))
    },
    error=function(cond) {
      return(FALSE)
    })
    if (ret != FALSE) {
      return(ret)
    }
  }
}
