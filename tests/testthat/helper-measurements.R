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
