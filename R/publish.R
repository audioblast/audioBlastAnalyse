#Putting what the agent makes (waveform peaks, spectrogram tiles) where
#audioBLAST! serves it from.
#
#They are put either in the directory AUDIOBLAST_FILES_DIR names, where an
#agent runs beside the files it serves, or by rsync to the destination
#AUDIOBLAST_FILES_RSYNC names, and are served at their path under
#AUDIOBLAST_FILES_URL, by default https://files.audioblast.org/. The
#AUDIOBLAST_PEAKS_ names these settings were first given, for peaks alone, are
#read where the AUDIOBLAST_FILES_ ones are not set.

#A setting for where files are put or served from, by its last part (DIR,
#RSYNC or URL)
filesSetting <- function(name, default="") {
  value <- Sys.getenv(paste0("AUDIOBLAST_FILES_", name))
  if (!nzchar(value)) value <- Sys.getenv(paste0("AUDIOBLAST_PEAKS_", name), default)
  return(value)
}

#Puts files where they are served from, each under the path given for it
#("/"-separated, relative to where they are served from), and gives the
#address of the last, or NA where any could not be put.
#
#Nothing is served half written, and the last file is put only once every
#other one has been, so that a manifest naming the others never names one that
#is not there yet. In a directory each file is written beside where it goes and
#moved there. By rsync, which writes each file beside where it goes too, the
#last goes in a second pass.
publishFiles <- function(files, paths,
                         dir=filesSetting("DIR"),
                         rsync=filesSetting("RSYNC"),
                         base=filesSetting("URL", "https://files.audioblast.org/")) {
  stopifnot(length(files) == length(paths), length(files) >= 1)
  if (nzchar(dir)) {
    for (i in seq_along(files)) {
      dest <- file.path(dir, paths[i])
      dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
      part <- paste0(dest, ".", Sys.getpid(), ".part")
      put <- file.copy(files[i], part, overwrite=TRUE) && file.rename(part, dest)
      unlink(part)
      if (!put) return(NA_character_)
    }
  } else if (nzchar(rsync)) {
    #Staged under the paths they are to have, so that rsync makes the
    #directories they need where there are none yet
    stage <- tempfile("publish")
    on.exit(unlink(stage, recursive=TRUE), add=TRUE)
    for (i in seq_along(files)) {
      staged <- file.path(stage, paths[i])
      dir.create(dirname(staged), recursive=TRUE, showWarnings=FALSE)
      if (!file.copy(files[i], staged)) return(NA_character_)
    }
    froms <- paste0(stage, "/./", paths)
    last <- length(froms)
    if (last > 1 && !runRsync(froms[-last], rsync)) return(NA_character_)
    if (!runRsync(froms[last], rsync)) return(NA_character_)
  } else {
    warning("Neither AUDIOBLAST_FILES_DIR nor AUDIOBLAST_FILES_RSYNC (nor their AUDIOBLAST_PEAKS_ names) is set, so files have nowhere to go")
    return(NA_character_)
  }
  return(paste0(sub("/*$", "/", base), paths[length(paths)]))
}

#Copies files to an rsync destination, giving whether they were copied. Each
#is named as it is to be named there from the /./ in its path on, which rsync
#keeps (--relative), making the directories it needs.
runRsync <- function(from, to) {
  status <- tryCatch(
    system2("rsync", c("--relative", "--chmod=D755,F644", shQuote(from), shQuote(to)),
            stdout=FALSE, stderr=FALSE),
    error=function(e) -1L)
  return(identical(as.integer(status), 0L))
}
