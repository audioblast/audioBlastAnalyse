#What measuring an audio file found, as one row of the columns that
#recordings-calculated holds. Every column is NA until something measures it,
#and status says what came of the measuring, so that a file that cannot be read
#is recorded as unreadable rather than left looking as though nothing had tried:
#
#* ok: the file was read, and what could be measured of it was.
#* missing: there is no file to measure at the path given.
#* unreadable: there are bytes, but no audio could be read from them. Their
#  size and hash are measured all the same, as a file of no bytes, or one whose
#  hash is that of a page saying the recording has moved, says why it could not
#  be read.
#
#error says what went wrong, and is NA where nothing did.
#
#Measurements are always one row, however many paths or messages are handed in:
#a caller that measured one file must not be given two rows to write.
noMeasurements <- function(status, error=NA_character_) {
  status <- as.character(status)[1]
  error <- as.character(error)[1]
  return(data.frame(
    hash=NA_character_,
    duration=NA_real_,
    channels=NA_integer_,
    sample_rate=NA_integer_,
    bit_depth=NA_integer_,
    bit_rate=NA_integer_,
    codec=NA_character_,
    size_raw=NA_real_,
    status=status,
    error=error,
    stringsAsFactors=FALSE))
}

#Measurements of a file that turned out not to be readable as audio, keeping
#whatever was measured of its bytes before that was known
unreadableFile <- function(measurements, error) {
  measurements$duration <- NA_real_
  measurements$status <- "unreadable"
  measurements$error <- as.character(error)[1]
  return(measurements)
}

#Measures an audio file, giving one row of measurements (see noMeasurements()).
#The file is never decoded: its bytes are read once for the hash, and the rest
#is what the container says of itself, so that measuring a recording costs the
#same whether it is a second long or an hour.
#
#A file of more than one audio stream is measured by its first, as a recording
#is one sound however many ways it was stored.
#' @importFrom cli hash_file_sha256
#' @importFrom av av_media_info
measureFile <- function(path) {
  if (length(path) != 1 || is.na(path) || !file.exists(path) || dir.exists(path)) {
    return(noMeasurements("missing", paste0("No file at ", pathText(path))))
  }

  #The bytes are measured whether or not they turn out to be audio
  measurements <- noMeasurements("ok")
  measurements$size_raw <- as.numeric(file.size(path))
  measurements$hash <- tryCatch(as.character(hash_file_sha256(path)),
                                error=function(e) NA_character_)

  info <- tryCatch(av_media_info(path), error=function(e) conditionMessage(e))
  if (is.character(info)) return(unreadableFile(measurements, info))
  audio <- info$audio
  if (is.null(audio) || nrow(audio) == 0) {
    return(unreadableFile(measurements, "The file holds no audio"))
  }

  measurements$channels <- wholeNumber(audio$channels[1])
  measurements$sample_rate <- wholeNumber(audio$sample_rate[1])
  measurements$bit_rate <- wholeNumber(audio$bitrate[1])
  measurements$codec <- codecName(audio$codec[1])
  measurements$bit_depth <- bitDepth(measurements$codec, audio$sample_fmt[1])
  measurements$duration <- positiveNumber(info$duration)

  #A container that cannot say how long it is cannot be analysed in chunks, so
  #it is unreadable however well it described itself otherwise
  if (is.na(measurements$duration)) {
    return(unreadableFile(measurements, "The file does not say how long it is"))
  }
  return(measurements)
}

#A path as it is written in a message, however little of a path it is
pathText <- function(path) {
  if (length(path) == 0) return("no path")
  return(paste(as.character(path), collapse=", "))
}

#The name of the format a stream is in, rather than of the decoder that read
#it: ffmpeg names some decoders after the arithmetic they use, so an MP3 is
#read by mp3float and would otherwise be recorded as being in that format.
codecName <- function(codec) {
  if (length(codec) != 1 || is.na(codec) || codec == "") return(NA_character_)
  return(sub("(float|_fixed)$", "", as.character(codec)))
}

#The bits a sample of the file is held in, read from the sample format the
#decoder gives, or NA where the file has no such thing.
#
#Only a lossless format has one: a lossy format holds no samples to have a
#width, and the format its decoder emits (commonly fltp, planar floating point)
#describes the decoder rather than the recording. A lossless decoder emits the
#samples the file holds, so its sample format is the file's own.
bitDepth <- function(codec, sample_fmt) {
  if (length(sample_fmt) != 1 || is.na(sample_fmt) || is.na(codec)) return(NA_integer_)
  lossless <- startsWith(codec, "pcm_") || codec %in% c("flac", "alac", "wavpack", "tta")
  if (!lossless) return(NA_integer_)
  #Planar formats hold each channel apart, and are as wide as the same format
  #interleaved
  bits <- c(u8=8L, s16=16L, s24=24L, s32=32L, s64=64L)[sub("p$", "", sample_fmt)]
  return(unname(bits))
}

#A number greater than zero, or NA for anything else, as a recording of no
#length was not measured but failed to be
positiveNumber <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || is.na(x) || !is.finite(x) || x <= 0) return(NA_real_)
  return(x)
}

#A whole number greater than zero, or NA for anything else
wholeNumber <- function(x) {
  x <- positiveNumber(x)
  if (is.na(x)) return(NA_integer_)
  return(as.integer(round(x)))
}
