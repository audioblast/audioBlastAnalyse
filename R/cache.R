#The file a recording is held in, ready to be read, or NA where it could not be
#downloaded whole.
#
#A recording that has been downloaded is kept, named for the recording it holds
#rather than for the address it came from, so that an agent meeting the
#recording again, or another agent working on the same machine, reads the file
#that is there rather than fetching it afresh. Downloading a recording costs
#more than every analysis made of it, and a source is not to be asked twice for
#what it has already given.
#' @importFrom tools file_ext
webFile <- function(url, source, id, dir="", verbose=FALSE, type=NA_character_) {
  path <- cachePath(dir, source, id, url, type)
  if (file.exists(path)) {
    if (verbose) print(paste("Already downloaded:", path))
    return(path)
  }
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)

  #Downloaded beside where it is to be kept and moved there once it is whole.
  #A download that stops half way would otherwise be left in the cache to be
  #read as the recording ever after, and two agents downloading one recording
  #at once would each read what the other had half written.
  part <- paste0(path, ".", Sys.getpid(), ".part")
  unlink(part)
  if (verbose) print(paste("Downloading:", url))
  if (!dl_file(url, part)) {
    #What there is of it is not the recording, and is neither kept nor read
    unlink(part)
    return(NA_character_)
  }
  if (!file.rename(part, path)) {
    #Renaming fails across file systems, and where another agent has just put
    #the same recording there
    file.copy(part, path, overwrite=TRUE)
    unlink(part)
  }
  return(path)
}

#Where the file of a recording is kept: a directory for the source, holding a
#file named for the recording's id within it. The two together name a recording
#in audioBlast!, so they name its file here. A directory that is not given is
#the one being worked in.
#
#The file is given the extension its address ends in, or where it ends in none
#(xeno-canto's end in /download) the one its MIME type is known by, so that
#the cache can be read by eye and opened by other programs. Nothing here needs
#it: ffmpeg reads a file for what it is, whatever it is called.
cachePath <- function(dir, source, id, file, type=NA_character_) {
  if (!nzchar(dir)) dir <- "."
  ext <- extension(file)
  if (!nzchar(ext)) ext <- mimeExtension(type)
  return(file.path(dir, safeName(source), paste0(safeName(id), ext)))
}

#The extension a file of the given MIME type is known by, with the dot, or ""
#for a type that is not an audio or video format known here. Parameters, such
#as a codec or a character set, are no part of the type.
mimeExtension <- function(type) {
  type <- tolower(trimws(sub(";.*$", "", as.character(type)[1])))
  if (length(type) == 0 || is.na(type)) return("")
  extensions <- c(
    "audio/mpeg"="mp3", "audio/mp3"="mp3", "audio/mpeg3"="mp3", "audio/x-mpeg"="mp3",
    "audio/wav"="wav", "audio/x-wav"="wav", "audio/wave"="wav", "audio/vnd.wave"="wav",
    "audio/flac"="flac", "audio/x-flac"="flac",
    "audio/ogg"="ogg", "application/ogg"="ogg", "audio/opus"="opus",
    "audio/mp4"="m4a", "audio/x-m4a"="m4a", "audio/m4a"="m4a", "audio/aac"="aac",
    "audio/aiff"="aiff", "audio/x-aiff"="aiff",
    "audio/x-wavpack"="wv", "audio/webm"="webm",
    "video/mp4"="mp4", "video/quicktime"="mov", "video/webm"="webm")
  if (!(type %in% names(extensions))) return("")
  return(paste0(".", extensions[[type]]))
}

#A name that stands for a value in a file system, whatever the value holds.
#
#A source or an id is what its source chose to call a recording, and audioBlast!
#holds ids that a file system will not take as names: an id may hold a slash, a
#colon or a quote, may be longer than a name may be, and on Windows may be a
#word the system keeps for itself. A value that is already a name is used as it
#is, so that a cache can be read by eye; one that is not is cut to length and
#given a hash of what it was, so that two values can never come to one name.
#' @importFrom cli hash_sha256
safeName <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x)) x <- ""
  #A value that is not UTF-8, as one read over a connection speaking Latin-1
  #is not, cannot be read as characters at all, and is read as bytes instead.
  #Only then: read as bytes, a character of two bytes in UTF-8 would be two
  #dashes rather than one, and a recording already kept would be looked for
  #under a new name.
  safe <- gsub("[^A-Za-z0-9._-]", "-", x, useBytes=!validUTF8(x))
  named <- identical(safe, x) && nchar(safe) > 0 && nchar(safe) <= 100 &&
    !grepl("^[.]+$", safe) && !isReservedName(safe)
  if (named) return(safe)
  return(paste0(substr(safe, 1, 100), "-", substr(hash_sha256(x), 1, 16)))
}

#Whether a name is one Windows keeps for itself, and will not let a file have
#however it is spelled or whatever is put after it
isReservedName <- function(name) {
  return(grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])([.]|$)", name, ignore.case=TRUE))
}

#The extension of the file an address points at, with the dot, or "" where it
#does not end in something that looks like one. What is after a ? or a # is no
#part of the path, and so no part of the name.
extension <- function(file) {
  ext <- tolower(file_ext(sub("[?#].*$", "", as.character(file)[1])))
  if (!grepl("^[a-z0-9]{1,5}$", ext)) return("")
  return(paste0(".", ext))
}
