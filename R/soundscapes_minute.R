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
#' @importFrom sonicscrewdriver readAudio rainfallDetection allChannels channels_se
#' @importFrom rjson toJSON
#' @importFrom seewave ACI H sh meanspec M th soundscapespec NDSI env
#' @importFrom soundecology bioacoustic_index acoustic_diversity acoustic_evenness

soundscapes_by_minute <- function(db, source, id, file, type, duration, tmp, force=FALSE, verbose=FALSE) {
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
      w <- tuneR::readWave("temp.wav")
    }

    if (is.logical(w)) return()

    if (verbose) print(paste("aci startTime:",(i-1)*60))
    v <- allChannels(w, ACI, channel.param="channel")
    insertAnalysis(db, "analysis-aci", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Bedoya startTime:",(i-1)*60))
    v <- allChannels(w, rainfallDetection, method="bedoya2017", channel.param=NULL)
    insertAnalysis(db, "analysis-bedoya", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Bioacoustic index startTime:",(i-1)*60))
    v <- allChannels(w, bioacoustic_index, channel.param=NULL, output.FUN="channels_se")
    insertAnalysis(db, "analysis-bi", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Acoustic diversity index startTime:",(i-1)*60))
    v <- allChannels(w, acoustic_diversity, channel.param = NULL, output.FUN = "channels_se")
    insertAnalysis(db, "analysis-adi", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Acoustic entropy startTime:",(i-1)*60))
    v <- allChannels(w, H, channel.param = "channel")
    insertAnalysis(db, "analysis-H", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Acoustic evenness startTime:",(i-1)*60))
    v <- allChannels(w, acoustic_evenness, channel.param = NULL, output.FUN = "channels_se")
    insertAnalysis(db, "analysis-evenness", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Spectral entropy startTime:",(i-1)*60))
    v <- allChannels(w, function(w,channel,...){m <- meanspec(w, channel=channel, plot=FALSE); return(sh(m))}, channel.param = "channel")
    insertAnalysis(db, "analysis-sh", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Amplitude index startTime:",(i-1)*60))
    v <- allChannels(w, M, channel.param = "channel")
    insertAnalysis(db, "analysis-M", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("Temporal entropy startTime:",(i-1)*60))
    v <- allChannels(w, function(w,channel,...){e <- env(w, channel=channel, plot=FALSE); return(th(e))}, channel.param = "channel")
    insertAnalysis(db, "analysis-th", source, id, (i-1)*60, v, complete)

    if (verbose) print(paste("NDSI startTime:",(i-1)*60))
    v <- allChannels(w, function(w,channel,...){m <- soundscapespec(w, channel=channel, plot=FALSE); return(NDSI(m))}, channel.param = "channel")
    insertAnalysis(db, "analysis-ndsi", source, id, (i-1)*60, v, complete)
  }
  sql = paste0("INSERT INTO `recordings-calculated` (`source`, `id`, `soundscapes_minute`) VALUES('", source, "', '", id, "', 1) ON DUPLICATE KEY UPDATE `soundscapes_minute` = 1;")
  dbeq <- abdbExecute(db, sql)
  if (verbose) print(dbeq)
}
