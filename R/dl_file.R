#' @importFrom utils download.file
dl_file <- function(file, tmp=NULL) {
  if (file.exists(tmp)) return()
  if (is.null(tmp)) {
    system(paste("wget --retry-connrefused --waitretry=0 --tries=1000 ", file))
  } else {
    if (Sys.info()[['sysname']] == "Windows") {
      timeout <- getOption('timeout')
      options(timeout=0)
      download.file(file, destfile=tmp, method="libcurl")
      options(timeout=timeout)
    } else {
      system(paste("wget --retry-connrefused --waitretry=0 --tries=1000 -O",tmp, file))
    }
  }
}
