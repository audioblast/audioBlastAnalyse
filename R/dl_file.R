dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    system(paste("wget --retry-connrefused --waitretry=0 --tries=1000 -O",tmp, file))
  }
}
