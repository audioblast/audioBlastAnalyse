#Downloads a recording to a file, leaving a file that is already there alone.
#Returns whether the download finished, so that a file cut short -- by the
#connection dropping, or by the agent being stopped part way through -- is not
#taken for the recording.
#' @importFrom utils download.file
dl_file <- function(file, tmp) {
  if (file.exists(tmp)) return(TRUE)
  if (Sys.info()[['sysname']] == "Windows") {
    #A recording of any size takes longer to fetch than download.file allows
    #for by default. The limit is put back however the download goes.
    timeout <- getOption('timeout')
    on.exit(options(timeout=timeout))
    options(timeout=0)
    status <- tryCatch(download.file(file, destfile=tmp, method="libcurl"),
                       error=function(e) -1)
    return(identical(as.integer(status), 0L))
  }
  status <- runCommand(paste("wget --retry-connrefused --waitretry=0 --tries=1000 -O",
                             shQuote(tmp), shQuote(file)))
  return(downloadFinished(status))
}

#Whether wget, ending with the given status, finished what it was asked to
#fetch. 0 is a download that finished. 8 is a server that answered with an
#error, such as a recording that is no longer there: what it answered with is
#the whole of what there is to be had, and is measured as such (as unreadable,
#most often), which is how a source of recordings that have moved is found.
#Anything else -- a network that failed, a file that could not be written, a
#wget that was stopped -- leaves a file that is only part of something.
downloadFinished <- function(status) {
  status <- suppressWarnings(as.integer(status))
  return(length(status) == 1 && !is.na(status) && status %in% c(0L, 8L))
}

#Runs a command, giving its exit status, as its own function so that tests
#need not
runCommand <- function(command) {
  return(system(command))
}
