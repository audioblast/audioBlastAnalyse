#' Run all analyses
#'
#' @param db database connector
#' @param cores Number of cores to use
#' @importFrom tools file_ext
#' @importFrom parallel makeCluster clusterCall clusterExport parRapply
#' @export
analyse <- function(db, sense="web", verbose=FALSE, force=FALSE, base_dir="", cores=1) {
  db <- DBI::dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)
  if (sense=="web") {
    ss <- fetchDownloadableRecordings(db)
  } else {
    ss <-fetchRecordingsFromSource(db, sense)
  }

  cn <- colnames(ss)
  ss <- cbind(ss, rep_len(sense, nrow(ss)), rep_len(verbose, nrow(ss)), rep_len(force, nrow(ss)), rep_len(base_dir, nrow(ss)))
  colnames(ss) <- c(cn, "sense", "verbose", "force", "base_dir")

  cl <- makeCluster(cores, outfile="")
  clusterCall(cl, function() library(DBI))
  clusterCall(cl, function() library(RMariaDB))
  clusterExport(cl, "port")
  clusterExport(cl, "dbuser")
  clusterExport(cl, "dbname")
  clusterExport(cl, "host")
  clusterExport(cl, "port")
  clusterExport(cl, "password")
  parRapply(cl, ss, analyseFile)
}

analyseFile <- function(inf){
  if (inf["file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1812_22g.WAV") {return(0)}
  db <- DBI::dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)

  if (as.character(inf["sense"]) == "web") {
    tmp <- paste0(tempfile(),".",file_ext(inf["file"]))
  } else {
    tmp <- paste0(inf["base_dir"],inf["file"])
  }

  a_tdsc(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])
  a_bedoya(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])

  if (inf["sense"] =="web") {
    unlink(tmp)
  }

  DBI::dbDisconnect(db)
  return(0)
}
