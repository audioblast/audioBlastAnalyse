#' @importFrom curl curl_download new_handle
dl_file <- function(file, tmp) {
  if(!file.exists(tmp)){
    curl_download(file, tmp, handle=new_handle(verbose = TRUE))
  }
}
