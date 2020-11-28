dl_file <- function(file, tmp=NULL) {
  if (is.null(tmp)) {
    system(paste("wget --retry-connrefused --waitretry=0 --tries=1000 ", file))
  } else if (!file.exists(tmp)){
    system(paste("wget --retry-connrefused --waitretry=0 --tries=1000 -O",tmp, file))
  }
}
