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
  #Under a strict SQL mode a value too long for its column is an error rather
  #than a truncation, so a long message would fail the write that was recording
  #it and lose the failure itself. A message is cut to a length any column wide
  #enough to be worth writing to will take.
  if (!is.na(error) && nchar(error) > 255) {
    error <- paste0(substr(error, 1, 252), "...")
  }
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
  measurements$bit_depth <- fileBitDepth(path, measurements$codec, audio$sample_fmt[1])
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

#The bit depth of a recording: how many bits of each sample the file says
#mean something, or NA where it has no such thing or cannot be read for it.
#
#Only a lossless format has one: a lossy format holds no samples to have a
#depth, and the format its decoder emits (commonly fltp, planar floating point)
#describes the decoder rather than the recording.
#
#Nor does a lossless decoder say what the file holds. ffmpeg has no 24-bit
#sample format, so 24-bit samples are decoded into 32 bits: a 24-bit WAV,
#FLAC, ALAC, WavPack or TTA file is s32 to its decoder, just as a 32-bit one
#is, and av gives nothing more (it has no bits_per_raw_sample). So the depth
#is read from the file's own header, which every lossless format has. Only
#PCM can be given a depth without it, from the name of its codec (see
#bitDepth()); any other format whose header cannot be read, such as FLAC in
#Matroska, has no depth recorded rather than a wrong one. A depth that is not
#a whole number of bytes, such as 20 bits, can only be had from the header.
#
#The header gives the depth the recording was made at, where a sample can be
#stored in more bits than that: a 20-bit recording in a WAV of 24-bit samples
#is 20 bits, as the file itself says. PCM is never taken to be deeper than its
#codec says its samples are.
fileBitDepth <- function(path, codec, sample_fmt) {
  decoded <- bitDepth(codec, sample_fmt)
  said <- tryCatch(headerBitDepth(path, codec), error=function(e) NA_integer_)
  if (is.na(said) || said < 1 || said > 64) return(decoded)
  if (!is.na(codec) && startsWith(codec, "pcm_") && !is.na(decoded) && said > decoded) {
    return(decoded)
  }
  return(said)
}

#The bit depth a lossless file's header says it has, or NA where the file is
#not one whose header is read, or does not begin as one does
headerBitDepth <- function(path, codec) {
  if (length(codec) != 1 || is.na(codec)) return(NA_integer_)
  if (startsWith(codec, "pcm_")) {
    said <- wavBitDepth(path)
    if (is.na(said)) said <- aiffBitDepth(path)
    return(said)
  }
  reader <- switch(codec, flac=flacBitDepth, alac=alacBitDepth,
                   wavpack=wavpackBitDepth, tta=ttaBitDepth, NULL)
  if (is.null(reader)) return(NA_integer_)
  return(reader(path))
}

#The bits a sample of a PCM file is held in, as far as its codec and decoder
#can say without the file being read, or NA where they cannot.
#
#PCM says how wide its samples are in the name of its codec (pcm_s24le,
#pcm_f32be, pcm_u8). Where it does not, its decoder can say so only of samples
#it decodes as they are held: u8 and s16. s32 could be 20 bits, 24 or 32.
#
#Nothing else is taken from a decoder. The other lossless formats' decoders
#widen what they are given, and not only into s32: ffmpeg decodes 8-bit
#WavPack, and 8-bit and 12-bit FLAC, as s16. Their depth is in their headers
#(see headerBitDepth()), or is not known.
bitDepth <- function(codec, sample_fmt) {
  if (length(codec) != 1 || is.na(codec) || !startsWith(codec, "pcm_")) return(NA_integer_)
  #A-law and mu-law are 8 bits a sample, expanded to 16 when decoded
  if (codec %in% c("pcm_alaw", "pcm_mulaw")) return(8L)
  named <- regmatches(codec, regexec("^pcm_[suf]([0-9]+)", codec))[[1]]
  if (length(named) == 2) return(as.integer(named[2]))
  if (length(sample_fmt) != 1 || is.na(sample_fmt)) return(NA_integer_)
  #Planar formats hold each channel apart, and are as wide as the same format
  #interleaved
  bits <- c(u8=8L, s16=16L)[sub("p$", "", sample_fmt)]
  return(unname(bits))
}

#The bit depth a WAV file's fmt chunk says it has. WAVE_FORMAT_EXTENSIBLE
#says how many bits of each sample are valid, where they are fewer than the
#bits it is stored in. RF64 is laid out as WAV is.
wavBitDepth <- function(path) {
  header <- fileBytes(path, 0, 12)
  if (!(hasBytes(header, 1, "RIFF") || hasBytes(header, 1, "RF64")) ||
      !hasBytes(header, 9, "WAVE")) {
    return(NA_integer_)
  }
  fmt <- chunkOf(path, 12, "fmt ", msbFirst=FALSE, n=40)
  if (length(fmt) < 16) return(NA_integer_)
  bits <- littleEndian(fmt[15:16])
  if (littleEndian(fmt[1:2]) == 0xFFFE && length(fmt) >= 20) {
    valid <- littleEndian(fmt[19:20])
    if (valid > 0) bits <- valid
  }
  return(as.integer(bits))
}

#The bit depth an AIFF or AIFF-C file's COMM chunk says it has
aiffBitDepth <- function(path) {
  header <- fileBytes(path, 0, 12)
  if (!hasBytes(header, 1, "FORM") ||
      !(hasBytes(header, 9, "AIFF") || hasBytes(header, 9, "AIFC"))) {
    return(NA_integer_)
  }
  comm <- chunkOf(path, 12, "COMM", msbFirst=TRUE, n=8)
  if (length(comm) < 8) return(NA_integer_)
  return(as.integer(bigEndian(comm[7:8])))
}

#The bit depth a FLAC file's STREAMINFO says it has. STREAMINFO is always
#the first block, whether the file is FLAC as it is or FLAC in Ogg, where it
#follows a packet header of its own.
flacBitDepth <- function(path) {
  start <- id3Length(path)
  bytes <- fileBytes(path, start, 42)
  if (hasBytes(bytes, 1, "fLaC")) return(streaminfoBitDepth(bytes[5:42]))
  if (hasBytes(bytes, 1, "OggS") && length(bytes) >= 27) {
    #An Ogg page is 27 bytes and a table of how long its segments are
    packet <- fileBytes(path, start + 27 + as.integer(bytes[27]), 51)
    if (length(packet) == 51 && packet[1] == as.raw(0x7F) &&
        hasBytes(packet, 2, "FLAC") && hasBytes(packet, 10, "fLaC")) {
      return(streaminfoBitDepth(packet[14:51]))
    }
  }
  return(NA_integer_)
}

#The bits per sample of a STREAMINFO block, given with the header of the block.
#They are held less one, in the five bits that follow the sample rate and
#number of channels: the last bit of its 13th byte and the first four of its
#14th.
streaminfoBitDepth <- function(block) {
  if (length(block) < 38 || bitwAnd(as.integer(block[1]), 0x7F) != 0) return(NA_integer_)
  streaminfo <- as.integer(block[5:38])
  return(as.integer(bitwAnd(streaminfo[13], 0x01) * 16 + bitwShiftR(streaminfo[14], 4) + 1))
}

#The bit depth an ALAC file's magic cookie says it has, the cookie being in
#the moov atom of an MP4 (M4A) file or the kuki chunk of a CAF file
alacBitDepth <- function(path) {
  header <- fileBytes(path, 0, 8)
  if (hasBytes(header, 1, "caff")) return(cafAlacBitDepth(path))
  if (!hasBytes(header, 5, "ftyp")) return(NA_integer_)
  size <- file.size(path)
  at <- 0
  while (at + 8 <= size) {
    atom <- fileBytes(path, at, 16)
    if (length(atom) < 8) return(NA_integer_)
    span <- bigEndian(atom[1:4])
    if (span == 1 && length(atom) == 16) span <- bigEndian(atom[9:16])
    if (span == 0) span <- size - at
    if (span < 8) return(NA_integer_)
    if (hasBytes(atom, 5, "moov")) {
      #moov describes the file rather than holding its audio, and is small
      #beside it; one too large to be that is not read
      if (span > 64 * 2^20) return(NA_integer_)
      return(alacCookieBitDepth(fileBytes(path, at, span)))
    }
    at <- at + span
  }
  return(NA_integer_)
}

#The bit depth an ALAC magic cookie in a CAF file's kuki chunk says it has
cafAlacBitDepth <- function(path) {
  size <- file.size(path)
  at <- 8
  while (at + 12 <= size) {
    chunk <- fileBytes(path, at, 12)
    if (length(chunk) < 12) return(NA_integer_)
    span <- bigEndian(chunk[5:12])
    if (hasBytes(chunk, 1, "kuki")) {
      cookie <- fileBytes(path, at + 12, min(span, 4096))
      said <- alacCookieBitDepth(cookie)
      #A cookie may be the configuration itself, rather than in an atom
      if (is.na(said) && length(cookie) >= 24) said <- as.integer(cookie[6])
      return(said)
    }
    #A chunk of unknown length runs to the end of the file
    if (span >= 2^63 - 2^11) return(NA_integer_)
    at <- at + 12 + span
  }
  return(NA_integer_)
}

#The bit depth in an ALAC magic cookie found among the given bytes: an alac
#atom of 36 bytes, holding its version and flags and then the configuration,
#whose sixth byte is the bit depth
alacCookieBitDepth <- function(bytes) {
  for (at in grepRaw("alac", bytes, fixed=TRUE, all=TRUE)) {
    if (at > 4 && at + 13 <= length(bytes) && bigEndian(bytes[(at - 4):(at - 1)]) == 36) {
      return(as.integer(bytes[at + 13]))
    }
  }
  return(NA_integer_)
}

#The bit depth a WavPack file's block header says it has. It says how many
#bytes a sample is stored in, and how many bits they are shifted by, the
#difference being the bits a sample has. A block of no samples may lead a
#file, holding only metadata, and is passed over. DSD is one bit a sample.
wavpackBitDepth <- function(path) {
  at <- id3Length(path)
  for (block in 1:16) {
    header <- fileBytes(path, at, 32)
    if (length(header) < 32 || !hasBytes(header, 1, "wvpk")) return(NA_integer_)
    flags <- littleEndian(header[25:28])
    if (littleEndian(header[21:24]) > 0) {
      if (flags >= 2^31) return(1L)
      stored <- flags %% 4 + 1
      shift <- (flags %/% 2^13) %% 32
      return(as.integer(stored * 8 - shift))
    }
    at <- at + 8 + littleEndian(header[5:8])
  }
  return(NA_integer_)
}

#The bit depth a TTA file's header says it has
ttaBitDepth <- function(path) {
  header <- fileBytes(path, id3Length(path), 10)
  if (length(header) < 10 || !hasBytes(header, 1, "TTA1")) return(NA_integer_)
  return(as.integer(littleEndian(header[9:10])))
}

#How many bytes an ID3v2 tag in front of the audio takes up, or 0 where there
#is none. Its size is in four bytes of seven bits each, and leaves out its own
#header and footer.
id3Length <- function(path) {
  header <- fileBytes(path, 0, 10)
  if (length(header) < 10 || !hasBytes(header, 1, "ID3")) return(0)
  size <- sum(bitwAnd(as.integer(header[7:10]), 0x7F) * 128^(3:0))
  footer <- bitwAnd(as.integer(header[6]), 0x10) != 0
  return(10 + size + if (footer) 10 else 0)
}

#The first n bytes of a chunk of the given name in a RIFF or IFF file, looking
#from the given offset, or none where there is no such chunk. Chunks are
#padded to an even length.
chunkOf <- function(path, from, name, msbFirst, n) {
  at <- from
  for (chunk in 1:64) {
    header <- fileBytes(path, at, 8)
    if (length(header) < 8) return(raw(0))
    span <- if (msbFirst) bigEndian(header[5:8]) else littleEndian(header[5:8])
    if (hasBytes(header, 1, name)) return(fileBytes(path, at + 8, min(span, n)))
    at <- at + 8 + span + span %% 2
  }
  return(raw(0))
}

#n bytes of a file from the given offset, or fewer where the file is shorter,
#or none where it cannot be read
fileBytes <- function(path, from, n) {
  if (n <= 0) return(raw(0))
  connection <- tryCatch(suppressWarnings(file(path, "rb")), error=function(e) NULL)
  if (is.null(connection)) return(raw(0))
  on.exit(close(connection))
  return(tryCatch({
    if (from > 0) seek(connection, from)
    readBin(connection, "raw", n=n)
  }, error=function(e) raw(0)))
}

#Whether bytes hold the given text at the given place
hasBytes <- function(bytes, at, text) {
  expected <- charToRaw(text)
  end <- at + length(expected) - 1
  return(length(bytes) >= end && identical(bytes[at:end], expected))
}

#A number held in bytes, most significant first or last
bigEndian <- function(bytes) sum(as.integer(bytes) * 256^((length(bytes) - 1):0))
littleEndian <- function(bytes) bigEndian(rev(bytes))

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
