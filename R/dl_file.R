#' @importFrom utils download.file
dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    tryCatch(download.file(file,
                                  destfile=tmp,
                                  method="auto"),
                    error=function(e) 1)
  }
}
