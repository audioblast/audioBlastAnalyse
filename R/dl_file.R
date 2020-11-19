dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    system(paste("wget --retry-connrefused --tries=100 -O",tmp, file))
  }
}
