#The file a recording is held in, ready to be read, or NA where it could not be
#downloaded whole.
#
#A recording that has been downloaded is kept, named for the recording it holds
#rather than for the address it came from, so that an agent meeting the
#recording again, or another agent working on the same machine, reads the file
#that is there rather than fetching it afresh. Downloading a recording costs
#more than every analysis made of it, and a source is not to be asked twice for
#what it has already given.
#
#Twice for what it has given, that is. A file in the cache with nothing in it
#was never given by anybody, and is fetched again: 394 of the 1,131,714 files
#cached from xeno-canto held nothing at all, and because this function began by
#giving back any file it found, every one of them was read as the recording ever
#after. 90 of those recordings the source serves perfectly today, and would have
#stayed empty for ever. So the cache heals (see recordingKept()).
#
#refetch fetches again whatever is already in the cache, for repairing a cache
#whose files cannot be trusted (see repairCache()).
#' @importFrom tools file_ext
webFile <- function(url, source, id, dir="", verbose=FALSE, type=NA_character_,
                    refetch=FALSE) {
  #Asked of the file system first and of the source not at all: a recording
  #already downloaded is found without a word to the source, as it always was.
  path <- cachePath(dir, source, id, url, type)
  if (!refetch) {
    held <- recordingHeld(dir, source, id, url, type)
    if (!is.na(held)) {
      if (verbose) print(paste("Already downloaded:", held))
      return(held)
    }
  }
  #An empty file is not the recording and will not be left standing in for it,
  #under any of the names it could have been left under; but it is only removed
  #once there is a recording to put in its place.
  stubs <- emptyHeld(dir, source, id, url, type)

  #Nothing is held, so the source is asked: first where the recording really is,
  #then what it is.
  address <- audioAddress(url, type, verbose)
  answer <- responseHeaders(address)
  if (!nzchar(extension(address)) && !nzchar(mimeExtension(type))) {
    #audioBlastIngest records the type of every recording from one HEAD request
    #and leaves it empty where that request failed, and cachePath() has nothing
    #else to name a /download address by. 324 files in the cache are named by a
    #bare numeric id for want of a type, though they are ordinary audio: all
    #four tested answer audio/mpeg, which mimeExtension() knows as .mp3. The
    #type was missing from the database, not unknown to the source, so the
    #source is asked rather than the recording left unnamed.
    type <- answer$type
  }
  named <- cachePath(dir, source, id, address, type)
  if (!identical(named, path)) {
    #The name has moved under us: the address we now have, or the type, gives it
    #an extension it did not have. It may be kept under that name already, by an
    #earlier pass or by audioBlastBirdNET, and is not to be fetched twice.
    if (!refetch && recordingKept(named)) {
      if (verbose) print(paste("Already downloaded:", named))
      return(named)
    }
    if (file.exists(named)) stubs <- c(stubs, named)
    path <- named
  }
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)

  #Downloaded beside where it is to be kept and moved there once it is whole.
  #A download that stops half way would otherwise be left in the cache to be
  #read as the recording ever after, and two agents downloading one recording
  #at once would each read what the other had half written.
  part <- paste0(path, ".", Sys.getpid(), ".part")
  unlink(part)
  if (verbose) print(paste("Downloading:", address))
  if (!dl_file(address, part)) {
    #What there is of it is not the recording, and is neither kept nor read
    unlink(part)
    if (sourceHasNot(answer$status)) {
      recordGone(dir, source, id, address, answer$status,
                 "the source answered that it has not got it")
    }
    return(NA_character_)
  }
  #The download finished, which is not the same as a recording having come down
  why <- notRecording(part, answer$type, answer$length)
  if (nzchar(why)) {
    if (verbose) print(paste("Not a recording:", address, "--", why))
    #A recording that came down short is a connection that gave out, not a
    #source that has not got the recording, and nothing is written down about
    #it: it will be asked for again and may come down whole next time.
    absent <- sourceHasNot(answer$status) || !shortOfLength(part, answer$length)
    unlink(part)
    if (absent) recordGone(dir, source, id, address, answer$status, why)
    return(NA_character_)
  }
  if (!file.rename(part, path)) {
    #Renaming fails across file systems, and where another agent has just put
    #the same recording there
    file.copy(part, path, overwrite=TRUE)
    unlink(part)
  }
  #Now, and only now, with the recording in hand
  unlink(setdiff(stubs, path))
  return(path)
}

#The file a recording is already kept in, or NA where it is not kept at all.
#
#Where the address and the type between them give the file an extension there is
#one place to look and it is looked in. Where they do not -- a xeno-canto
#/download address, and a type audioBlastIngest never managed to record -- the
#recording may be kept under any of the names an extension would have given it:
#under the name a pass that did learn the type from the source gave it, or under
#the bare id that audioBlastBirdNET gives it and that 324 files in the cache
#carry. So every name it could be under is looked for, which is a dozen
#questions put to the file system instead of two put to the source, and the
#source is dearer than the file system by a factor of a hundred.
#
#This looks for a recording; it does not name one. cachePath() still decides
#what a recording downloaded now is called, and decides it exactly as
#audioBlastBirdNET's cache_path() does.
recordingHeld <- function(dir, source, id, url, type=NA_character_) {
  for (path in cacheNames(dir, source, id, url, type)) {
    if (recordingKept(path)) return(path)
  }
  return(NA_character_)
}

#The files kept for a recording that hold nothing: the 394, under whichever of
#its names they were left under. They are never given back as the recording,
#whatever is asked of the cache, so they are in nobody's way; they are cleared
#when there is a recording to put in their place, so that nothing downstream
#that goes looking by eye or by its own reckoning can find one and read it as a
#recording of silence.
emptyHeld <- function(dir, source, id, url, type=NA_character_) {
  held <- cacheNames(dir, source, id, url, type)
  held <- held[file.exists(held)]
  return(held[!vapply(held, recordingKept, TRUE, USE.NAMES=FALSE)])
}

#Every name a recording could be kept under in this cache: the one the address
#and the type give it, and, where between them they give it no extension, that
#name with each extension this package knows.
#
#Judged from the address and the type rather than from the name they produce,
#because an id may have a dot in it -- TSA's and iNaturalist's are whatever
#their source called them -- and be no less in want of an extension for that.
cacheNames <- function(dir, source, id, url, type=NA_character_) {
  path <- cachePath(dir, source, id, url, type)
  if (nzchar(extension(url)) || nzchar(mimeExtension(type))) return(path)
  return(c(path, paste0(path, unique(paste0(".", mimeExtensions())))))
}

#Whether the file kept at path is a recording, rather than something left there
#in place of one.
#
#Only emptiness is held against a file that is already in the cache. It is what
#all 394 of the bad files have in common and it is certain: nothing any source
#ever sent is a recording of no length. The closer look notRecording() takes is
#for a file that has just come down, where there is an answer from the source to
#read it against and where fetching it again costs only the fetch. A file in the
#cache may be in a format whose first bytes this package does not know, and
#throwing it away and fetching it again on every pass would be worse than the
#fault it was looking for.
recordingKept <- function(path) {
  if (!file.exists(path)) return(FALSE)
  size <- file.size(path)
  return(!is.na(size) && size > 0)
}

#Where it is written down that a source has not got a recording it was asked
#for: the recording's name with .gone after it, beside where the recording
#itself would have been kept.
#
#Named as the recording would be named with nothing to go on for an extension,
#so that the note can be found without knowing what the recording would have
#been called -- which, for a recording nobody has ever managed to fetch, nobody
#does know.
goneMarker <- function(dir, source, id) {
  return(paste0(cachePath(dir, source, id, ""), ".gone"))
}

#Writes down that a source has not got a recording it was asked for.
#
#An empty file used to be what said this, and that was the whole trouble: read
#as audio an empty file is a recording of silence, and nothing downstream could
#tell one from a download that had gone wrong. So it is said in a file that
#nothing will mistake for a recording, holding when the source was asked, what
#it answered and where it was asked.
#
#It is a note and not a verdict. Nothing in webFile() reads it to decide whether
#to ask again, and nothing should: of the 394 recordings whose cached files were
#empty, 90 are served perfectly today. A source that has lost a recording may
#find it, and will be asked again. Nor does a note stop the agent if it cannot
#be written -- a cache that cannot be written to is not a reason to abandon an
#analysis.
recordGone <- function(dir, source, id, address, status, why) {
  marker <- goneMarker(dir, source, id)
  dir.create(dirname(marker), recursive=TRUE, showWarnings=FALSE)
  line <- paste(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                if (is.na(status)) "-" else status, why, address)
  return(invisible(tryCatch({writeLines(line, marker); TRUE}, error=function(e) FALSE)))
}

#Removes what is left of downloads that were never finished, giving back what it
#removed.
#
#A download is written to a file named for the recording and for the pid writing
#it, and the pid that writes it is the only one that ever unlinks it. An agent
#stopped between the download and the rename therefore leaves its file behind
#for good: four are on the share, each of them beside a complete sibling, one of
#them byte for byte identical to its sibling.
#
#Cleared here and not on the way to a download. Nothing ever reads a file whose
#name ends in .part, so one left behind is in nobody's way and costs only room;
#and finding one means reading a directory that holds a million files, which is
#not a price to pay on every fetch to recover four files. So it is done when the
#cache is being gone over deliberately (see repairCache()).
#
#Only the stale ones. A download in progress writes to its file continuously, so
#one touched within age seconds may be an agent at work and is left alone.
sweepParts <- function(dir, age=86400, verbose=FALSE) {
  if (!nzchar(dir)) dir <- "."
  parts <- list.files(dir, pattern="[.][0-9]+[.]part$", recursive=TRUE,
                      all.files=TRUE, full.names=TRUE)
  if (length(parts) == 0) return(invisible(character()))
  touched <- file.mtime(parts)
  stale <- parts[!is.na(touched) & touched < Sys.time() - age]
  if (verbose) for (part in stale) print(paste("Unfinished download removed:", part))
  unlink(stale)
  return(invisible(stale))
}

#Fetches again the recordings the cache cannot be trusted over, and clears what
#is left of downloads that were never finished. Gives back the file each
#recording is now held in, named for the recording, or NA for one that still
#could not be downloaded.
#
#recordings is what fetchDownloadableRecordings() gives: rows with a source, an
#id, the address the recording is at in file, and the type the database holds
#for it. A vector of ids would be a shorter thing to pass, but an id is not
#enough to fetch a recording by -- the address is what is needed -- so the rows
#are what this takes.
#
#Every recording named is fetched again whatever is in the cache for it, because
#the reason for calling this is that what is in the cache cannot be trusted:
#each of the 394 empty files was a file that file.exists() was perfectly happy
#with.
repairCache <- function(recordings, dir="", verbose=FALSE, age=86400) {
  sweepParts(dir, age=age, verbose=verbose)
  paths <- rep(NA_character_, nrow(recordings))
  for (i in seq_len(nrow(recordings))) {
    type <- if ("type" %in% names(recordings)) recordings[[i, "type"]] else NA_character_
    paths[i] <- webFile(recordings[i, "file"], recordings[[i, "source"]],
                        recordings[[i, "id"]], dir, verbose, type, refetch=TRUE)
  }
  names(paths) <- paste(recordings[["source"]], recordings[["id"]])
  return(paths)
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
  extensions <- mimeExtensions()
  if (!(type %in% names(extensions))) return("")
  return(paste0(".", extensions[[type]]))
}

#The extension each MIME type this package knows is named by, without the dot.
#
#The same list it always was, said in a function of its own only so that the one
#other place that needs it -- recordingHeld(), looking for a recording under
#every name an extension could have given it -- reads it here rather than
#keeping a second copy that could come to disagree with this one.
#audioBlastBirdNET's cache.py holds the matching copy, and this list is to be
#changed there too or not at all.
mimeExtensions <- function() {
  return(c(
    "audio/mpeg"="mp3", "audio/mp3"="mp3", "audio/mpeg3"="mp3", "audio/x-mpeg"="mp3",
    "audio/wav"="wav", "audio/x-wav"="wav", "audio/wave"="wav", "audio/vnd.wave"="wav",
    "audio/flac"="flac", "audio/x-flac"="flac",
    "audio/ogg"="ogg", "application/ogg"="ogg", "audio/opus"="opus",
    "audio/mp4"="m4a", "audio/x-m4a"="m4a", "audio/m4a"="m4a", "audio/aac"="aac",
    "audio/aiff"="aiff", "audio/x-aiff"="aiff",
    "audio/x-wavpack"="wv", "audio/webm"="webm",
    "video/mp4"="mp4", "video/quicktime"="mov", "video/webm"="webm"))
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
