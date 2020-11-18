dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    system(paste("curl -o",tmp," --retry 5", file))
  }
}
