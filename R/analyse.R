#' Run all analyses
#'
#' @param db database connector
#' @param sense "web" for online files, or name of source to analyse locally
#' @param verbose Gives verbose output if TRUE
#' @param force Forces recalculation of analyses if TRUE
#' @param base_dir Directory relative paths are located in
#' @param cores Number of cores to use
#' @param reverse Reverses the data frame of recordings to analyse
#' @param shuffle Shuffles rows before analysing
#' @param checkFile Provide a filename to check for debugging purposes
#' @importFrom tools file_ext
#' @importFrom parallel makeCluster clusterCall clusterExport parRapply
#' @export
analyse <- function(db, sense="web", verbose=FALSE, force=FALSE, base_dir="", cores=1, reverse=FALSE, shuffle=FALSE, checkFile=NULL) {
  db <- DBI::dbConnect(RMariaDB::MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)
  if (sense=="web") {
    ss <- fetchDownloadableRecordings(db)
  } else {
    ss <-fetchRecordingsFromSource(db, sense)
  }

  cn <- colnames(ss)
  ss <- cbind(ss, rep_len(sense, nrow(ss)), rep_len(verbose, nrow(ss)), rep_len(force, nrow(ss)), rep_len(base_dir, nrow(ss)))
  colnames(ss) <- c(cn, "sense", "verbose", "force", "base_dir")

  if (reverse) {
    ss <- ss[order(nrow(ss):1),]
  }
  if (shuffle) {
    ss <- ss[sample(nrow(ss)),]
  }

  if (cores == 1) {
    for (i in 1:nrow(ss)) {
      if (!is.null(checkFile) && ss[i, "file"] != checkFile) {next()}

      if (file_ext(ss[i, "file"]) == "MP3") next()
      if (file_ext(ss[i, "file"]) == "mp3") next()
      if (file_ext(ss[i, "file"]) == "ogg") next()
      if (file_ext(ss[i, "file"]) == "zc") next()


      if (verbose) {print(ss[i, "file"]);}
      if (sense == "web") {
        tmp <- paste0(tempfile(),".",file_ext(ss[i, "file"]))
      } else {
        tmp <- paste0(base_dir,ss[i, "file"])
      }
      if (verbose) {print("audiowaveform");}
      tryCatch({
        a_audiowaveform(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
      if (verbose) {print("TDSC");}
      tryCatch({
        a_tdsc(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
      if (verbose) {print("bedoya");}
      tryCatch({
        a_bedoya(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
      if (verbose) {print("bioacoustic index");}
      tryCatch({
        a_bi(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
      if (verbose) {print("ACI");}
      tryCatch({
        a_aci(db, ss[[i, "source"]], ss[[i, "id"]], ss[[i, "file"]], ss[[i, "type"]], as.numeric(ss[[i, "Duration"]]), tmp, force, verbose)
      })
      if (sense == "web") {
        unlink(tmp)
      }
    }
  } else {
    cl <- makeCluster(cores, outfile="")
    clusterCall(cl, function() library(DBI))
    clusterCall(cl, function() library(RMariaDB))
    clusterExport(cl, "port")
    clusterExport(cl, "dbuser")
    clusterExport(cl, "dbname")
    clusterExport(cl, "host")
    clusterExport(cl, "port")
    clusterExport(cl, "password")
    parRapply(cl, ss, analyseFileCluster)
  }
}

#' Analyse an individual file
#'
#' @param inf Information passed to the function
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RMariaDB MariaDB
#' @export
analyseFileCluster <- function(inf){
  if (inf["file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1812_22g.WAV") {return(0)}
  if (inf["file"] == "http://bio.acousti.ca/sites/default/files/MASAPO19910526_1121.WAV") {return(0)}
  db <- dbConnect(MariaDB(), user=dbuser, password=password, dbname=dbname, host=host, port=port)

  if (as.character(inf["sense"]) == "web") {
    tmp <- paste0(tempfile(),".",file_ext(inf["file"]))
  } else {
    tmp <- paste0(inf["base_dir"],inf["file"])
  }

  a_bedoya(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])
  a_tdsc(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])
  a_bi(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])
  a_aci(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])
  a_audiowaveform(db, inf["source"], inf["id"], inf["file"], inf["type"], as.numeric(inf["Duration"]), tmp, inf["force"], inf["verbose"])

  if (inf["sense"] =="web") {
    unlink(tmp)
  }

  dbDisconnect(db)
  return(0)
}
