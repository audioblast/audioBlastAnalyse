#' @importFrom utils download.file
dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    curl_download(file, tmp)
  }
}
