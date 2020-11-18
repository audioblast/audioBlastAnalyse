dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    system(paste("wget --retry-connrefused -O",tmp, file))
  }
}
