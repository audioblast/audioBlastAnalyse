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

test_that("only PCM has a bit depth without its file being read", {
  expect_identical(bitDepth("pcm_s16le", "s16"), 16L)
  expect_identical(bitDepth("pcm_u8", "u8"), 8L)
  #A lossy format holds no samples to have a width, whatever its decoder emits
  expect_identical(bitDepth("mp3", "fltp"), NA_integer_)
  expect_identical(bitDepth("mp3", "s16p"), NA_integer_)
  expect_identical(bitDepth("aac", "fltp"), NA_integer_)
  expect_identical(bitDepth("opus", "s16"), NA_integer_)
  expect_identical(bitDepth(NA_character_, "s16"), NA_integer_)
  #Other lossless decoders widen what they are given, so what they emit is
  #not the file's: 8-bit WavPack and FLAC are both s16 to their decoders
  expect_identical(bitDepth("flac", "s16"), NA_integer_)
  expect_identical(bitDepth("wavpack", "s16p"), NA_integer_)
  expect_identical(bitDepth("alac", "s32p"), NA_integer_)
  expect_identical(bitDepth("tta", "s32"), NA_integer_)
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

test_that("a 24-bit WAV is measured as 24 bits", {
  path <- aWave(samp.rate=96000, bit=24, stereo=FALSE)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$codec, "pcm_s24le")
  expect_identical(measurements$bit_depth, 24L)
  expect_identical(measurements$bit_rate, 96000L * 24L)
})

test_that("a WAV of 20-bit samples held in 24 bits is measured as 20", {
  path <- aWave(bit=24)
  on.exit(unlink(path))
  #WAVE_FORMAT_EXTENSIBLE says how many bits of each sample are valid, in the
  #19th and 20th bytes of the fmt chunk
  fmt <- whereIs(path, "fmt ") + 8
  expect_identical(littleEndian(readBin(path, "raw", n=fmt + 1)[fmt:(fmt + 1)]), 0xFFFE)
  withBytes(path, fmt + 18, c(20, 0))
  measurements <- measureFile(path)

  expect_identical(measurements$codec, "pcm_s24le")
  expect_identical(measurements$bit_depth, 20L)
})

test_that("PCM is never deeper than its codec says, whatever its header does", {
  path <- aWave(bit=24)
  on.exit(unlink(path))
  withBytes(path, whereIs(path, "fmt ") + 8 + 18, c(32, 0))

  #ffmpeg would take this file for 32-bit PCM itself, so the rule is asked of
  #directly, for a decoder that took the file for what it is
  expect_identical(wavBitDepth(path), 32L)
  expect_identical(fileBitDepth(path, "pcm_s24le", "s32"), 24L)
})

test_that("an AIFF is as deep as its COMM chunk says", {
  path <- aConverted(".aiff")
  on.exit(unlink(path))
  expect_identical(measureFile(path)$bit_depth, 16L)
  #The sample size is the 7th and 8th bytes of COMM, most significant first
  withBytes(path, whereIs(path, "COMM") + 8 + 6, c(0, 12))

  expect_identical(aiffBitDepth(path), 12L)
  expect_identical(measureFile(path)$bit_depth, 12L)
})

test_that("a 24-bit FLAC is measured as 24 bits, from its STREAMINFO", {
  path <- aFlac(bits=24)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$codec, "flac")
  #Its decoder cannot say, so the file must be read for it
  expect_identical(av::av_media_info(path)$audio$sample_fmt, "s32")
  expect_identical(measurements$bit_depth, 24L)
})

test_that("a 16-bit FLAC is measured as 16 bits", {
  path <- aFlac(bits=16)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(measurements$status, "ok")
  expect_identical(measurements$bit_depth, 16L)
})

test_that("an 8-bit FLAC is measured as 8 bits, not as its decoder emits", {
  path <- aFlac(bits=8)
  on.exit(unlink(path))
  measurements <- measureFile(path)

  expect_identical(av::av_media_info(path)$audio$sample_fmt, "s16")
  expect_identical(measurements$bit_depth, 8L)
})

test_that("a FLAC behind an ID3 tag is read past it", {
  flac <- aFlac(bits=24)
  path <- tempfile(fileext=".flac")
  on.exit(unlink(c(flac, path)))
  #An ID3v2.4 tag of one title frame, and a size in seven-bit bytes
  frame <- c(charToRaw("TIT2"), as.raw(c(0, 0, 0, 5, 0, 0, 3)), charToRaw("Song"))
  tag <- c(charToRaw("ID3"), as.raw(c(4, 0, 0, 0, 0, 0, length(frame))), frame)
  writeBin(c(tag, readBin(flac, "raw", n=file.size(flac))), path)

  expect_identical(flacBitDepth(path), 24L)
  expect_identical(measureFile(path)$bit_depth, 24L)
})

test_that("STREAMINFO is read for widths no decoder format has", {
  path <- aFlac(bits=24)
  on.exit(unlink(path))
  #Bits per sample less one, 23, becomes 19: the header of a 20-bit file
  bytes <- readBin(path, "raw", n=file.size(path))
  withBytes(path, 22, bitwOr(bitwAnd(as.integer(bytes[22]), 0x0F), 0x30))

  expect_identical(flacBitDepth(path), 20L)
})

test_that("FLAC in Ogg is read for its STREAMINFO too", {
  path <- aConverted(".oga")
  on.exit(unlink(path))
  expect_identical(av::av_media_info(path)$audio$codec, "flac")
  expect_identical(measureFile(path)$bit_depth, 16L)
  #The bits per sample less one are the last bit of the 13th byte of
  #STREAMINFO, which follows fLaC and a block header, and the first four of
  #the 14th: 15 (0 1111) becomes 23 (1 0111)
  at <- whereIs(path, "fLaC") + 8 + 12
  bytes <- as.integer(readBin(path, "raw", n=at + 1)[at:(at + 1)])
  withBytes(path, at, c(bitwOr(bytes[1], 0x01), bitwOr(bitwAnd(bytes[2], 0x0F), 0x70)))

  expect_identical(flacBitDepth(path), 24L)
})

test_that("WavPack is as deep as its block header says, not as its decoder emits", {
  path <- aConverted(".wv")
  on.exit(unlink(path))
  measurements <- measureFile(path)

  #av writes WavPack as 8 bits, which its decoder emits as s16p
  expect_identical(measurements$codec, "wavpack")
  expect_identical(av::av_media_info(path)$audio$sample_fmt, "s16p")
  expect_identical(measurements$bit_depth, 8L)

  #Flags are the 25th to 28th bytes: the bytes a sample is stored in, less one,
  #in their lowest two bits, and how far they are shifted in bits 13 to 17
  flags <- littleEndian(readBin(path, "raw", n=28)[25:28])
  setFlags <- function(value) withBytes(path, 25, (value %/% 256^(0:3)) %% 256)
  unshifted <- flags - flags %% 4 - ((flags %/% 2^13) %% 32) * 2^13
  setFlags(unshifted + 2)
  expect_identical(wavpackBitDepth(path), 24L)
  setFlags(unshifted + 2 + 4 * 2^13)
  expect_identical(wavpackBitDepth(path), 20L)
  #DSD is one bit a sample
  setFlags(unshifted + 2^31)
  expect_identical(wavpackBitDepth(path), 1L)
})

test_that("TTA is as deep as its header says", {
  path <- aConverted(".tta")
  on.exit(unlink(path))
  #av writes TTA as 8 bits
  expect_identical(measureFile(path)$bit_depth, 8L)
  withBytes(path, 9, c(24, 0))

  expect_identical(ttaBitDepth(path), 24L)
})

test_that("ALAC is as deep as its magic cookie says, in M4A or CAF", {
  m4a <- anAlacM4a(bits=24)
  caf <- aCafOfAlac(bits=20)
  wrapped <- aCafOfAlac(bits=24, wrapped=TRUE)
  on.exit(unlink(c(m4a, caf, wrapped)))

  #The sample entry is named alac too, and is not taken for the cookie
  expect_identical(alacBitDepth(m4a), 24L)
  expect_identical(alacBitDepth(caf), 20L)
  expect_identical(alacBitDepth(wrapped), 24L)
})

test_that("a lossless format whose header cannot be read has no bit depth", {
  wav <- aWave()
  notAudio <- aFile()
  empty <- aFile("")
  zeros <- tempfile()
  writeBin(raw(64), zeros)
  on.exit(unlink(c(wav, notAudio, empty, zeros)))
  missing <- file.path(tempdir(), "no-such-recording.flac")

  for (path in list(wav, notAudio, empty, zeros, missing)) {
    expect_identical(flacBitDepth(path), NA_integer_)
    expect_identical(alacBitDepth(path), NA_integer_)
    expect_identical(wavpackBitDepth(path), NA_integer_)
    expect_identical(ttaBitDepth(path), NA_integer_)
    #FLAC in a container not read, such as Matroska, is not guessed at
    expect_identical(fileBitDepth(path, "flac", "s32"), NA_integer_)
  }
  expect_identical(wavBitDepth(notAudio), NA_integer_)
  expect_identical(aiffBitDepth(wav), NA_integer_)
  #PCM needs no header, as its codec says how wide it is
  expect_identical(fileBitDepth(notAudio, "pcm_s24le", "s32"), 24L)
  #A lossy file is not read for one at all
  expect_identical(fileBitDepth(wav, "mp3", "fltp"), NA_integer_)
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
