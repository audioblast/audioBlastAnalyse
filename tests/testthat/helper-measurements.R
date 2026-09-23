#A row of measurements (see noMeasurements()) holding the given values, and NA
#for everything that a test does not care about
measurementsOf <- function(status="ok", ...) {
  measurements <- noMeasurements(status)
  values <- list(...)
  for (column in names(values)) {
    measurements[[column]] <- values[[column]]
  }
  return(measurements)
}

#A WAV file of a tone, written where a test will clean it up after itself
aWave <- function(seconds=1, samp.rate=44100, bit=16, stereo=TRUE) {
  samples <- round(16000 * sin(2 * pi * 440 * seq(0, seconds, length.out=seconds * samp.rate)))
  wave <- tuneR::Wave(left=samples, right=if (stereo) samples else numeric(0),
                      samp.rate=samp.rate, bit=bit, pcm=TRUE)
  path <- tempfile(fileext=".wav")
  tuneR::writeWave(wave, path)
  return(path)
}

#A file of the given bytes, which are not audio
aFile <- function(contents="Not a recording") {
  path <- tempfile(fileext=".wav")
  writeBin(charToRaw(contents), path)
  return(path)
}

#A FLAC file of a tone, written by hand: av will only encode FLAC of 16 bits,
#and a test of a 24-bit one needs one to read. It is a single frame of
#samples stored as they are, which is the simplest FLAC there is.
aFlac <- function(bits=24, samples=4096, samp.rate=48000) {
  bytesOf <- function(x, n) as.raw((x %/% 256^((n - 1):0)) %% 256)
  crc <- function(bytes, width, poly) {
    top <- 2^(width - 1)
    value <- 0
    for (byte in as.integer(bytes)) {
      value <- bitwXor(value, byte * 2^(width - 8))
      for (i in 1:8) {
        value <- if (value >= top) bitwXor((value - top) * 2, poly) else value * 2
      }
    }
    return(bytesOf(value, width / 8))
  }
  #STREAMINFO: block sizes, unknown frame sizes, then the rate, channels less
  #one, bits less one and number of samples packed into 64 bits, and no MD5
  packed <- c(bytesOf(samp.rate %/% 16, 2),
              bytesOf((samp.rate %% 16) * 16 + (bits - 1) %/% 16, 1),
              bytesOf(((bits - 1) %% 16) * 16, 1),
              bytesOf(samples, 4))
  streaminfo <- c(bytesOf(samples, 2), bytesOf(samples, 2), raw(6), packed, raw(16))
  #A frame of a fixed block size, the size given at the end of the header,
  #the rate taken from STREAMINFO, one channel of the given width
  header <- c(as.raw(c(0xFF, 0xF8, 0x70)),
              bytesOf(c(`8`=1, `16`=4, `24`=6)[[as.character(bits)]] * 2, 1),
              as.raw(0), bytesOf(samples - 1, 2))
  header <- c(header, crc(header, 8, 0x07))
  tone <- round((2^(bits - 3)) * sin(2 * pi * 440 * (seq_len(samples) - 1) / samp.rate))
  tone[tone < 0] <- tone[tone < 0] + 2^bits
  #A verbatim subframe: the samples as they are, most significant byte first
  frame <- c(header, as.raw(0x02), as.vector(sapply(tone, bytesOf, n=bits / 8)))
  frame <- c(frame, crc(frame, 16, 0x8005))
  path <- tempfile(fileext=".flac")
  writeBin(c(charToRaw("fLaC"), as.raw(c(0x80, 0, 0, 34)), streaminfo, frame), path)
  return(path)
}

#A 16-bit WAV of a tone converted by av into the format an extension names,
#or a skipped test where av cannot write it. av's encoders take the first
#sample format they are able to, which is not always 16 bits: WavPack and TTA
#are written as 8.
aConverted <- function(ext) {
  wav <- aWave()
  on.exit(unlink(wav))
  path <- tempfile(fileext=ext)
  written <- tryCatch({av::av_audio_convert(wav, path, verbose=FALSE); file.exists(path)},
                      error=function(e) FALSE)
  testthat::skip_if_not(written, paste("av cannot write", ext))
  return(path)
}

#Overwrites bytes of a file, the first of them at the given place (counting
#from 1), so that a test can make a header say what it needs it to
withBytes <- function(path, at, bytes) {
  contents <- readBin(path, "raw", n=file.size(path))
  contents[at:(at + length(bytes) - 1)] <- as.raw(bytes)
  writeBin(contents, path)
  return(invisible(path))
}

#Where the given text first is in a file, counting from 1
whereIs <- function(path, text) {
  return(grepRaw(text, readBin(path, "raw", n=file.size(path)), fixed=TRUE)[1])
}

#An ALAC magic cookie: an alac atom of its version and flags, then the
#configuration, whose sixth byte is the bit depth
alacCookie <- function(bits) {
  config <- as.raw(c(0, 0, 16, 0, 0, bits, 40, 10, 14, 1, 0, 255, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 187, 128))
  return(c(as.raw(c(0, 0, 0, 36)), charToRaw("alac"), raw(4), config))
}

#The header of an M4A file of ALAC, which is all of it that a test reads: an
#ftyp atom, and a moov atom in which the cookie is inside an alac sample entry
#of its own, as it is in stsd
anAlacM4a <- function(bits=24) {
  bigEndian4 <- function(x) as.raw((x %/% 256^(3:0)) %% 256)
  entry <- c(raw(28), alacCookie(bits))
  entry <- c(bigEndian4(length(entry) + 8), charToRaw("alac"), entry)
  moov <- c(bigEndian4(length(entry) + 8), charToRaw("moov"), entry)
  ftyp <- c(bigEndian4(16), charToRaw("ftypM4A "), raw(4))
  path <- tempfile(fileext=".m4a")
  writeBin(c(ftyp, moov), path)
  return(path)
}

#The header of a CAF file of ALAC: a desc chunk, then a kuki chunk holding the
#configuration itself, or wrapped in an alac atom, then data of unknown length
aCafOfAlac <- function(bits=24, wrapped=FALSE) {
  chunk <- function(type, contents) {
    c(charToRaw(type), as.raw(c(0, 0, 0, 0, (length(contents) %/% 256^(3:0)) %% 256)), contents)
  }
  cookie <- alacCookie(bits)
  if (!wrapped) cookie <- cookie[13:36]
  path <- tempfile(fileext=".caf")
  writeBin(c(charToRaw("caff"), as.raw(c(0, 1, 0, 0)),
             chunk("desc", raw(32)), chunk("kuki", cookie),
             charToRaw("data"), as.raw(rep(255, 8))), path)
  return(path)
}
