#Downloads a recording to a file, leaving a file that is already there alone.
#' @importFrom utils download.file
dl_file <- function(file, tmp) {
  if (file.exists(tmp)) return()
  if (Sys.info()[['sysname']] == "Windows") {
    #A recording of any size takes longer to fetch than download.file allows
    #for by default. The limit is put back however the download goes.
    timeout <- getOption('timeout')
    on.exit(options(timeout=timeout))
    options(timeout=0)
    download.file(file, destfile=tmp, method="libcurl")
  } else {
    system(paste("wget --retry-connrefused --waitretry=0 --tries=1000 -O", tmp, file))
  }
}
