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
