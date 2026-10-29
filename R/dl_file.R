#Downloads a recording to a file, leaving a file that is already there alone.
#Returns whether the download finished, so that a file cut short -- by the
#connection dropping, or by the agent being stopped part way through -- is not
#taken for the recording.
#
#Finishing is not the same as having fetched a recording: a server with nothing
#to give answers all the same, and what it answers with is written to the file
#just as a recording would be. What came down is looked at before it is kept
#(see notRecording()).
#' @importFrom utils download.file
dl_file <- function(file, tmp) {
  if (file.exists(tmp)) return(TRUE)
  if (Sys.info()[['sysname']] == "Windows") {
    #A recording of any size takes longer to fetch than download.file allows
    #for by default. The limit is put back however the download goes.
    timeout <- getOption('timeout')
    on.exit(options(timeout=timeout))
    options(timeout=0)
    #mode="wb" always. download.file() writes in text mode unless the address
    #ends in an extension it knows to be binary, and on Windows text mode puts
    #a carriage return before every line feed: an address with no extension,
    #as OSF's have, came down 0.4% longer and unreadable.
    status <- tryCatch(download.file(file, destfile=tmp, method="libcurl", mode="wb"),
                       error=function(e) -1)
    return(identical(as.integer(status), 0L))
  }
  status <- runCommand(paste("wget --retry-connrefused --waitretry=0 --tries=1000 -O",
                             shQuote(tmp), shQuote(file)))
  return(downloadFinished(status))
}

#Whether wget, ending with the given status, finished what it was asked to
#fetch. Only 0 is.
#
#8 -- a server that answered with an error -- was taken for a finished download
#too, on the reasoning that the error was the whole of what there was to be had
#and would be measured as such. It is not. wget's -O creates its output file
#before it makes the request, and throws away the body of an error response
#unless it is asked to keep it with --content-on-error, so a 404 or a 500 left
#an empty file, which was then renamed into the cache under the recording's
#name. 394 of the 1,131,714 files cached from xeno-canto held nothing at all,
#and because an empty file is a file, every one of them was read as the
#recording ever after; 90 of those recordings the source serves perfectly today.
#
#What the old reasoning wanted still happens. A recording that cannot be
#downloaded however often it is asked for is measured at its address, where
#there is no file, and so is recorded as missing with its address in the error
#(see analyse() and downloadAttempts()) -- which is how a source of recordings
#that have moved is found. It is recorded without a file in the cache that
#reads as silence, and where the source has said outright that it has not got
#the recording that is written down beside it (see recordGone()).
#
#Anything else -- a network that failed, a file that could not be written, a
#wget that was stopped -- leaves a file that is only part of something.
downloadFinished <- function(status) {
  status <- suppressWarnings(as.integer(status))
  return(length(status) == 1 && !is.na(status) && status == 0L)
}

#Runs a command, giving its exit status, as its own function so that tests
#need not
runCommand <- function(command) {
  return(system(command))
}

#The address a recording can actually be fetched from.
#
#xeno-canto is asked for its recordings at https://xeno-canto.org/<id>/download.
#Of the 394 recordings whose cached files were empty, 81 answer 500 at that
#address on every attempt while the file itself is sitting on the server under
#its own name, where all 81 answer 200 with correct audio: 77 mp3 and 4 wav,
#162 MB in all, every one of them reading without a warning. The endpoint is
#broken for those recordings; the recordings are not gone. So where the address
#is one of those, the record page is read and the address of the file taken from
#it.
#
#The page is read rather than the API asked because API v2 is retired -- it
#answers that it is no longer available and to see v3 -- and v3 wants a key,
#which belongs to the harvesting in audioBlastIngest and not here. The page
#needs none.
#
#Any other address is given back as it came. This is one source's broken
#endpoint, not something to do to every source.
audioAddress <- function(url, type=NA_character_, verbose=FALSE) {
  id <- xenoCantoDownloadId(url)
  if (is.na(id)) return(url)
  page <- fetchText(paste0("https://xeno-canto.org/", id))
  if (is.na(page)) return(url)
  files <- uploadedFiles(page)
  #A record page that links no audio file at all: 40 of the 394 are like this,
  #and there is nothing to be fetched from anywhere. Asked at the address it
  #always had, the recording will be found to be absent and written down as
  #absent, which is the most that can be said of it.
  if (length(files) == 0) return(url)
  chosen <- chooseUpload(files, type)
  if (verbose) print(paste("Fetching", url, "as", chosen))
  return(chosen)
}

#The id of the recording a xeno-canto /download address asks for, or NA for an
#address that is not one. Only that exact shape, so that nothing else is sent
#to xeno-canto to be looked up.
xenoCantoDownloadId <- function(url) {
  url <- as.character(url)[1]
  if (is.na(url)) return(NA_character_)
  found <- regmatches(url, regexec("^https?://(?:www[.])?xeno-canto[.]org/([0-9]+)/download/?$",
                                   url, perl=TRUE))[[1]]
  if (length(found) < 2) return(NA_character_)
  return(found[2])
}

#The addresses of the audio files a xeno-canto record page links, in the order
#the page gives them.
#
#A page carries other things served from the same place -- the spectrograms of
#the recording among them -- so only the formats xeno-canto holds uploads in are
#taken. This list is for reading a page and is no part of how files are named:
#mimeExtension()'s list, which audioBlastBirdNET keeps a copy of, is untouched.
uploadedFiles <- function(page) {
  page <- as.character(page)[1]
  if (is.na(page)) return(character())
  found <- unique(regmatches(page, gregexpr("/sounds/uploaded/[^\"'[:space:]<>]+", page))[[1]])
  if (length(found) == 0) return(character())
  found <- gsub("&amp;", "&", found, fixed=TRUE)
  audio <- c(".mp3", ".wav", ".flac", ".ogg", ".opus", ".m4a", ".aac", ".aiff", ".wv")
  found <- found[vapply(found, extension, "", USE.NAMES=FALSE) %in% audio]
  #Said rather than left to paste0(), which given nothing to add to an address
  #gives back the address on its own
  if (length(found) == 0) return(character())
  return(paste0("https://xeno-canto.org", found))
}

#Which of the files a record page links is the recording that was asked for.
#
#A page may link more than one: XC900000 links the WAV it was uploaded as and
#the MP3 made from that. What is wanted is the recording the broken endpoint
#should have served and not a different one, so the file whose extension matches
#the type the database holds for the recording is preferred; failing that the
#MP3, which is what /download serves and what 77 of the 81 recovered this way
#turned out to be. Only where neither is there is the first the page links
#taken.
chooseUpload <- function(files, type) {
  extensions <- vapply(files, extension, "", USE.NAMES=FALSE)
  wanted <- mimeExtension(type)
  if (nzchar(wanted) && wanted %in% extensions) return(files[match(wanted, extensions)])
  if (".mp3" %in% extensions) return(files[match(".mp3", extensions)])
  return(files[1])
}

#Reads a page as text, or NA where it could not be read, as its own function so
#that tests need not reach the network.
#
#Read as bytes and made into characters afterwards, with whatever is not UTF-8
#dropped: a page with a byte in it that is not the character set it claims must
#not stop the agent. The same care is taken over ids, for the same reason (see
#safeName()).
fetchText <- function(address, bytes=2e6) {
  timeout <- getOption("timeout")
  on.exit(options(timeout=timeout), add=TRUE)
  options(timeout=60)
  connection <- tryCatch(url(address, "rb"), error=function(e) NULL)
  if (is.null(connection)) return(NA_character_)
  on.exit(close(connection), add=TRUE)
  raw <- tryCatch(readBin(connection, "raw", n=bytes), error=function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(NA_character_)
  raw <- raw[raw != as.raw(0)]
  return(iconv(rawToChar(raw), "UTF-8", "UTF-8", sub=""))
}

#What a source says about a recording before it is fetched: the status it
#answers with, the type it calls the recording, and the length it says the
#recording is. NA for whatever it does not say, and NA throughout where it
#cannot be asked at all.
#
#One question ahead of the download, against the hundreds of round trips a
#recording of any size takes, and only ever asked about a recording that is not
#already in the cache. It buys two things. The type of a recording the database
#holds none for, which is 324 files in the cache named by a bare id for want of
#it. And the length to measure the download against: /download sends no
#Content-Length, but the file's own address does, which is one more reason to
#fetch recordings by their own addresses (see audioAddress()).
#
#The answer is read for what it says and is not trusted to say no. A source that
#refuses HEAD while serving GET perfectly well would otherwise lose every one of
#its recordings, so whether a recording can be had is settled by fetching it and
#by nothing else.
responseHeaders <- function(address) {
  unknown <- list(status=NA_integer_, type=NA_character_, length=NA_real_)
  if (!capabilities("libcurl")) return(unknown)
  headers <- tryCatch(suppressWarnings(curlGetHeaders(address, redirect=TRUE, timeout=60L)),
                      error=function(e) NULL)
  if (is.null(headers)) return(unknown)
  return(answerAbout(headers))
}

#What a source's answer says about the recording that was asked for: the status,
#and the type and length where the answer was about the recording at all.
#
#Only an answer about the recording describes the recording. A source that
#refuses the question -- 403 or 405 to a HEAD whose GET it serves perfectly well
#-- describes the page it refused with, and a recording in a format with no mark
#this package knows would then be thrown away for being text/html. The status of
#such an answer is still worth having; nothing else of it is.
answerAbout <- function(headers) {
  status <- suppressWarnings(as.integer(attr(headers, "status")))
  if (is.na(status) || status < 200 || status > 299) {
    return(list(status=status, type=NA_character_, length=NA_real_))
  }
  return(list(status=status,
              type=headerValue(headers, "content-type"),
              length=suppressWarnings(as.numeric(headerValue(headers, "content-length")))))
}

#The value a named header was given, or NA where it was not given at all. The
#last one it was given: a redirect is followed, and the headers of every answer
#along the way come back together, of which only the last describes what would
#be fetched.
headerValue <- function(headers, name) {
  lines <- grep(paste0("^", name, ":"), headers, ignore.case=TRUE, value=TRUE)
  if (length(lines) == 0) return(NA_character_)
  return(trimws(sub("^[^:]*:", "", lines[length(lines)])))
}

#Why what was downloaded to path is not the recording that was asked for, or ""
#where nothing says it is not.
#
#A server with nothing to give answers all the same, and its answer lands in the
#file the download was writing. The answers that left 394 empty files in the
#cache were 15 kB pages of HTML saying the recording was not there. So what has
#been written is looked at before it is moved into the cache under a recording's
#name, because that name is what everything downstream reads it by.
#
#What it is looked at for wants care in both directions. Audio cannot be
#recognised from its first bytes alone: ID3, RIFF, fLaC and OggS say what they
#are, but an AAC stream, an m4a and a WavPack file do not all announce
#themselves in a list this short, and a file is still a recording for want of a
#mark this package knows. Nor can a recording be required to call itself audio:
#xeno-canto serves its WAV files as application/octet-stream, measured on the
#live server, so insisting on audio/* would throw away good recordings. So the
#marks of audio are taken as proof that a file is a recording, the marks of a
#document as proof that it is not, and a file that says nothing either way is
#let through on its length -- which is the thing that was actually wrong with
#all 394 of them.
notRecording <- function(path, type=NA_character_, length=NA_real_) {
  if (!file.exists(path)) return("nothing was written")
  size <- file.size(path)
  if (is.na(size) || size == 0) return("nothing was sent")
  #Before anything else, because a recording cut off part way through still
  #begins with everything that says it is a recording
  if (shortOfLength(path, length)) {
    return(paste(size, "bytes came of the",
                 trimws(format(length, scientific=FALSE)),
                 "the source said it would send"))
  }
  first <- tryCatch(readBin(path, "raw", n=16), error=function(e) raw())
  if (audioMark(first)) return("")
  if (documentType(type)) return(paste("the source sent", type))
  if (documentMark(first)) return("what was sent begins as a document, not a recording")
  return("")
}

#Whether what came down is not the length the source said it would be.
#
#Kept apart from the rest of what makes a file not a recording because it calls
#for something else to be done about it: a recording that came down short is a
#connection that gave out part way through, which may not next time, and says
#nothing at all about whether the source has the recording.
shortOfLength <- function(path, length) {
  size <- file.size(path)
  if (is.na(size) || size == 0) return(FALSE)
  wanted <- suppressWarnings(as.numeric(length)[1])
  return(!is.na(wanted) && wanted > 0 && size != wanted)
}

#Whether the first bytes of a file are a mark that says it holds audio. A file
#without one is not thereby something else: the formats that have no mark here
#are still fetched and kept, on the strength of their length and of not looking
#like a document.
audioMark <- function(first) {
  if (length(first) < 4) return(FALSE)
  starts <- function(mark) identical(first[seq_len(nchar(mark))], charToRaw(mark))
  #MP3 with a tag in front of it, WAV, FLAC, Ogg, AIFF and WavPack each say what
  #they are in their first four bytes
  if (starts("ID3") || starts("RIFF") || starts("fLaC") || starts("OggS") ||
      starts("FORM") || starts("wvpk")) return(TRUE)
  #An MP3 or an AAC carrying no tag begins with a frame sync instead: eleven
  #bits set
  if (first[1] == as.raw(0xFF) && bitwAnd(as.integer(first[2]), 0xE0) == 0xE0) return(TRUE)
  #An MP4, an m4a and a MOV say so in the four bytes after their length
  if (length(first) >= 8 && identical(first[5:8], charToRaw("ftyp"))) return(TRUE)
  #Matroska, and WebM with it
  if (identical(first[1:4], as.raw(c(0x1A, 0x45, 0xDF, 0xA3)))) return(TRUE)
  return(FALSE)
}

#Whether the first bytes of a file begin a document rather than a recording. An
#HTML page, an XML document and a JSON object all begin with a character no
#audio format begins with, whatever whitespace or byte order mark is in front of
#it. Read as bytes throughout: an error page is not to be made into characters
#before it can be recognised as one.
documentMark <- function(first) {
  if (length(first) >= 3 && identical(first[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    first <- first[-(1:3)]
  }
  #What is in front of it and nothing more. A 0x20 further in is a byte of
  #whatever the file holds, not the space before a document, and skipping over
  #it could carry this as far as a < that was never the beginning of anything.
  begins <- which(!(first %in% as.raw(c(0x09, 0x0A, 0x0D, 0x20))))
  if (length(begins) == 0) return(FALSE)
  return(first[begins[1]] %in% as.raw(c(0x3C, 0x7B)))
}

#Whether a type is one a document is served as rather than a recording. A type
#that is not audio/* is not thereby a document -- xeno-canto's WAV files come as
#application/octet-stream -- so only the types a page or a message is sent as
#are counted. Parameters, such as a character set, are no part of the type.
documentType <- function(type) {
  type <- as.character(type)[1]
  if (is.na(type)) return(FALSE)
  type <- tolower(trimws(sub(";.*$", "", type)))
  if (!nzchar(type)) return(FALSE)
  return(grepl("^text/", type) ||
         type %in% c("application/json", "application/problem+json",
                     "application/xml", "application/xhtml+xml", "application/rss+xml"))
}

#Whether a status says the source has not got the recording, rather than that
#something went wrong on the way to it.
#
#Only the 4xx answers say it. /download answers 500 for 81 recordings whose
#files are sitting on the server and can be fetched by name, so a 5xx is a
#server having trouble and says nothing at all about whether a recording exists.
sourceHasNot <- function(status) {
  status <- suppressWarnings(as.integer(status))
  return(length(status) == 1 && !is.na(status) && status >= 400 && status < 500)
}
