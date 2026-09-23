test_that("a lossless file is measured by what its container says", {
  path <- aWave()
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$error, NA_character_)
  expect_equal(measurements$duration, 1)
  expect_identical(measurements$channels, 2L)
  expect_identical(measurements$sample_rate, 44100L)
  expect_identical(measurements$codec, "pcm_s16le")
  expect_identical(measurements$bit_depth, 16L)
  expect_identical(measurements$bit_rate, 1411200L)
  #A WAV is its samples and a short header
  expect_equal(measurements$size_raw, 44100 * 2 * 2 + 80, tolerance=100)
  expect_match(measurements$hash, "^[0-9a-f]{64}$")
})

test_that("a mono file of another rate is measured as itself", {
  path <- aWave(samp.rate=22050, stereo=FALSE)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$channels, 1L)
  expect_identical(measurements$sample_rate, 22050L)
})

test_that("a lossy file is measured, but has no bit depth", {
  wav <- aWave()
  mp3 <- tempfile(fileext=".mp3")
  on.exit(unlink(c(wav, mp3)))
  skip_if_not(tryCatch({av::av_audio_convert(wav, mp3, verbose=FALSE); file.exists(mp3)},
                       error=function(e) FALSE),
              "No encoder for MP3")
  measurements <- measureFile(mp3)

  expect_identical(measurements$status, "ok")
  #The decoder is named mp3float, but the recording is in MP3
  expect_identical(measurements$codec, "mp3")
  expect_identical(measurements$bit_depth, NA_integer_)
  expect_gt(measurements$bit_rate, 0)
  #Encoders pad what they are given, so a lossy file is not exactly as long
  expect_equal(measurements$duration, 1, tolerance=0.1)
  expect_lt(measurements$size_raw, file.size(wav))
})

test_that("a file that isn't there is missing, not unreadable", {
  measurements <- measureFile(file.path(tempdir(), "no-such-recording.wav"))

  expect_identical(measurements$status, "missing")
  expect_match(measurements$error, "^No file at ")
  expect_identical(measurements$hash, NA_character_)
  expect_identical(measurements$size_raw, NA_real_)
})

test_that("a path that is no path at all is missing", {
  for (path in list(NA_character_, character(0), c("one.wav", "two.wav"), tempdir())) {
    expect_identical(measureFile(path)$status, "missing")
  }
})

test_that("bytes that are not audio are unreadable, and still measured", {
  path <- aFile()
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "unreadable")
  expect_false(is.na(measurements$error))
  #What the bytes are is worth recording even when they are not a recording
  expect_equal(measurements$size_raw, 15)
  expect_match(measurements$hash, "^[0-9a-f]{64}$")
  expect_identical(measurements$duration, NA_real_)
})

test_that("a file of no bytes is unreadable", {
  path <- aFile("")
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "unreadable")
  expect_equal(measurements$size_raw, 0)
})

test_that("a decoder is named by the format it reads", {
  expect_identical(codecName("mp3float"), "mp3")
  expect_identical(codecName("ac3_fixed"), "ac3")
  expect_identical(codecName("pcm_s16le"), "pcm_s16le")
  expect_identical(codecName("flac"), "flac")
  expect_identical(codecName(NA_character_), NA_character_)
  expect_identical(codecName(""), NA_character_)
})

test_that("only a lossless recording has a bit depth", {
  expect_identical(bitDepth("pcm_s16le", "s16"), 16L)
  expect_identical(bitDepth("pcm_u8", "u8"), 8L)
  expect_identical(bitDepth("flac", "s16"), 16L)
  expect_identical(bitDepth("alac", "s16p"), 16L)
  expect_identical(bitDepth("wavpack", "u8p"), 8L)
  #A lossy format holds no samples to have a width, whatever its decoder emits
  expect_identical(bitDepth("mp3", "fltp"), NA_integer_)
  expect_identical(bitDepth("mp3", "s16p"), NA_integer_)
  expect_identical(bitDepth("aac", "fltp"), NA_integer_)
  expect_identical(bitDepth("opus", "s16"), NA_integer_)
  expect_identical(bitDepth(NA_character_, "s16"), NA_integer_)
  expect_identical(bitDepth("flac", NA_character_), NA_integer_)
})

test_that("PCM is as deep as its codec says, not as its decoder emits", {
  #ffmpeg has no 24-bit sample format, and decodes 24-bit PCM into 32 bits
  expect_identical(bitDepth("pcm_s24le", "s32"), 24L)
  expect_identical(bitDepth("pcm_s24be", "s32"), 24L)
  expect_identical(bitDepth("pcm_u24le", "s32"), 24L)
  expect_identical(bitDepth("pcm_s32le", "s32"), 32L)
  expect_identical(bitDepth("pcm_s16be", "s16"), 16L)
  expect_identical(bitDepth("pcm_s16le_planar", "s16p"), 16L)
  expect_identical(bitDepth("pcm_s8", "u8"), 8L)
  expect_identical(bitDepth("pcm_f32le", "flt"), 32L)
  expect_identical(bitDepth("pcm_f64be", "dbl"), 64L)
  #A-law and mu-law hold 8 bits a sample, and are decoded into 16
  expect_identical(bitDepth("pcm_alaw", "s16"), 8L)
  expect_identical(bitDepth("pcm_mulaw", "s16"), 8L)
  #The codec says it, so the decoder need not
  expect_identical(bitDepth("pcm_s16le", NA_character_), 16L)
  #PCM that does not name its width is as wide as it is decoded, where that
  #cannot be wider than the file
  expect_identical(bitDepth("pcm_dvd", "s16"), 16L)
  expect_identical(bitDepth("pcm_bluray", "s32"), NA_integer_)
})

test_that("a lossless format decoded into 32 bits has no bit depth said", {
  #24 bits and 32 are both s32 to the decoder, and av gives nothing else that
  #would tell them apart, so neither is recorded as a guess
  expect_identical(bitDepth("flac", "s32"), NA_integer_)
  expect_identical(bitDepth("alac", "s32p"), NA_integer_)
  expect_identical(bitDepth("wavpack", "s32p"), NA_integer_)
  expect_identical(bitDepth("tta", "s32"), NA_integer_)
})

test_that("a 24-bit WAV is measured as 24 bits", {
  path <- aWave(samp.rate=96000, bit=24, stereo=FALSE)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$codec, "pcm_s24le")
  expect_identical(measurements$bit_depth, 24L)
  expect_identical(measurements$bit_rate, 96000L * 24L)
})

test_that("a 24-bit FLAC is measured, but has no bit depth", {
  #Should av come to say how many bits a sample is held in, this is the test
  #that will say that FLAC can be measured after all
  path <- aFlac(bits=24)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$codec, "flac")
  expect_identical(av::av_media_info(path)$audio$sample_fmt, "s32")
  expect_identical(measurements$bit_depth, NA_integer_)
})

test_that("a 16-bit FLAC is measured as 16 bits", {
  path <- aFlac(bits=16)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$bit_depth, 16L)
})

test_that("a measurement is a number, or nothing", {
  expect_equal(positiveNumber("48000"), 48000)
  expect_identical(positiveNumber(0), NA_real_)
  expect_identical(positiveNumber(-1), NA_real_)
  expect_identical(positiveNumber(Inf), NA_real_)
  expect_identical(positiveNumber("soon"), NA_real_)
  expect_identical(positiveNumber(NULL), NA_real_)

  expect_identical(wholeNumber(44100), 44100L)
  expect_identical(wholeNumber("2"), 2L)
  expect_identical(wholeNumber(0), NA_integer_)
  expect_identical(wholeNumber(NA), NA_integer_)
})

test_that("a message too long for a column is cut rather than failing the write", {
  #A recording in a deeply nested directory makes a long "No file at" message,
  #and under a strict SQL mode an over-long value is an error, not a truncation
  path <- file.path(paste(rep("a-directory", 40), collapse="/"), "recording.wav")
  measurements <- measureFile(path)

  expect_identical(measurements$status, "missing")
  expect_lte(nchar(measurements$error), 255)
  expect_true(endsWith(measurements$error, "..."))
  #A message that fits is left whole
  expect_identical(noMeasurements("unreadable", "The file holds no audio")$error,
                   "The file holds no audio")
})
