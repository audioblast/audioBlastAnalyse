#' Analyses soundscapes in one minute chunks
#'
#' Analyses performed:
#' - ACI (Acoustic Complexity Index)
#' - ADI (Acoustic Diversity Index)
#' - Bedoya rainfall
#' - BI (Bioacoustic Index)
#' - Acoustic Evenness
#' - H (Acoustic Entropy)
#' - M (Amplitude Index)
#' - NDSI (Normalised Difference Soundscape Index)
#' - SH (Spectral Entropy)
#' - TH (Temporal Entropy)
#'
#' @param db database connector
#' @param source Source
#' @param id id (unique within source)
#' @param file url to file
#' @param type MIME type
#' @param duration audio duration
#' @param tmp Location to download temp file
#' @param force If TRUE recalculates all values
#' @param verbose If TRUE outputs debugging information
#' @export
#' @importFrom sonicscrewdriver allChannels
#' @importFrom seewave spec
soundscapes_spectro <- function(db, source, id, file, type, duration, tmp,
                                force=FALSE, verbose=FALSE, save.path=".") {
  print(tmp)
  n <- ceiling(duration/60)
  if (force==TRUE) {
    deleteAllAnalyses(db, source, id, justR=TRUE)
  }

  if (duration == 0) {
    if (verbose) print("Provided duration is zero -- skipping")
    return()
  }

  for (i in (1:n)) {
    duration <- av_media_info(tmp)$duration

    from <- (i-1)*60
    to <- i*60
    complete <- 1

    if (duration < 60 || (duration - (i-1)*60) < 60) {
      complete <- 0
    }

    if (!complete || i*60 == duration) {
        w <- readAudio(tmp, from=from, units="seconds")
    } else {
      av::av_audio_convert(tmp, output="temp.wav")
      w <- readWave("temp.wav")
    }

    if (is.logical(w)) return()

    if (verbose) print(paste("Spectro startTime:",(i-1)*60))
    s <- spec(w, wl=256, plot=FALSE)

    #Hack as seewave spec doesn't always work
    ss <- seq(from=0, to=w@samp.rate/2, along.with=s[,2])
    v <- cbind(ss, s[,2])

    path <- paste(save.path, source, id, sep="/")
    if (!dir.exists(path)) {
      dir.create(path, recursive=TRUE)
    }
    write.csv(v, paste(path, "spec.json", sep="/"))
  }
}
