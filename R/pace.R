#How much an agent may download from a source, as bytes in any hour and bytes
#in any day, or NULL for a source that sets no limit.
#
#iNaturalist does. Its recommended practices for the API say that downloading
#more than 5 GB of media an hour, or 24 GB a day, may get the downloader blocked
#for good, and it means it: from 28 October 2026 an agent on unp-audio-2 that
#fetched iNaturalist's sounds as fast as it could was refused every day once it
#had fetched 20 to 27 GB, and went on refusing for hours. Each refusal was
#then kept and measured as an empty file, 1,055,846 of them. So an agent stays
#well under both: 20 GB a day, spread over the day at no more than 1 GB an hour.
#At iNaturalist's mean of about 1 MB a recording that is some 20,000 recordings
#a day.
#
#Any source can be given limits, and iNaturalist's changed, with the option
#audioBlastAnalyse.limits: a list naming each source, each a vector of hour and
#day in bytes, as list(iNaturalist=c(hour=1e9, day=20e9)). NULL for a source
#lifts its limits.
downloadLimits <- function(source) {
  limits <- getOption("audioBlastAnalyse.limits",
                      list(iNaturalist=c(hour=1e9, day=20e9)))
  source <- as.character(source)[1]
  if (is.na(source) || !(source %in% names(limits))) return(NULL)
  limit <- limits[[source]]
  if (is.null(limit)) return(NULL)
  return(c(hour=unname(limit[["hour"]]), day=unname(limit[["day"]])))
}

#Where what has been downloaded from a source is written down: a file of its
#own for each source, kept with the user's data for this package on this
#machine rather than in the cache. A source limits what is fetched from one
#address, which is one machine, so every agent the user runs on the machine
#writes to the same file and keeps within the same limits; and an agent that is
#stopped and started again picks up where it left off, rather than starting a
#fresh day's allowance.
#
#The option audioBlastAnalyse.ledger_dir puts the files somewhere else.
ledgerPath <- function(source) {
  dir <- getOption("audioBlastAnalyse.ledger_dir",
                   tools::R_user_dir("audioBlastAnalyse", which="data"))
  return(file.path(dir, paste0("downloads-", safeName(source), ".csv")))
}

#Writes down that bytes were downloaded from a source just now. Only for a
#source with limits: nothing else needs to know. Every download counts, whether
#or not it turned out to be a recording, because a source counts what it sent.
#
#One line appended for each download, which agents writing at once cannot make
#a muddle of. A ledger that cannot be written is not a reason to stop: the
#download is not counted, and a warning says so.
recordDownload <- function(source, bytes, when=Sys.time()) {
  if (is.null(downloadLimits(source))) return(invisible(FALSE))
  bytes <- suppressWarnings(as.numeric(bytes)[1])
  if (is.na(bytes) || bytes <= 0) return(invisible(FALSE))
  path <- ledgerPath(source)
  written <- tryCatch({
    dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
    cat(sprintf("%.3f,%.0f\n", as.numeric(when), bytes), file=path, append=TRUE)
    TRUE
  }, error=function(e) FALSE)
  if (!written) warning(paste("Could not write down a download from", source, "in", path))
  return(invisible(written))
}

#What has been downloaded from a source in the last day, as a data frame of
#when (seconds since 1970) and how many bytes, oldest first.
#
#Older lines are of no more use, and are cleared from the file once there are
#more of them than of the lines that are still wanted, so that the file stays
#about a day long. Cleared through a file beside it, renamed over it, so that a
#reader never finds it half written; a line another agent appends between the
#reading and the renaming is lost, which costs one download's worth of
#allowance at most.
downloadsSince <- function(source, now=Sys.time()) {
  path <- ledgerPath(source)
  none <- data.frame(when=numeric(), bytes=numeric())
  if (!file.exists(path)) return(none)
  lines <- tryCatch(readLines(path, warn=FALSE), error=function(e) character())
  if (length(lines) == 0) return(none)
  fields <- strsplit(lines, ",", fixed=TRUE)
  whole <- lengths(fields) == 2
  when <- suppressWarnings(as.numeric(vapply(fields[whole], `[`, "", 1)))
  bytes <- suppressWarnings(as.numeric(vapply(fields[whole], `[`, "", 2)))
  read <- data.frame(when=when, bytes=bytes)
  read <- read[!is.na(read$when) & !is.na(read$bytes), , drop=FALSE]
  recent <- read$when > as.numeric(now) - 86400
  if (sum(!recent) > max(1000, sum(recent))) {
    part <- paste0(path, ".", Sys.getpid(), ".tmp")
    kept <- tryCatch({
      writeLines(lines[whole][!is.na(when) & !is.na(bytes)][recent], part)
      file.rename(part, path)
    }, error=function(e) FALSE)
    if (!isTRUE(kept)) unlink(part)
  }
  read <- read[recent, , drop=FALSE]
  return(read[order(read$when), , drop=FALSE])
}

#How many seconds from now until a source's limits allow another download, or 0
#where they allow one now.
#
#For each limit, the downloads within its window are added up; where they come
#to the limit or more, the wait is until enough of the oldest have passed out of
#the window to bring them under it. A download is let through whenever the
#total is under a limit, however large it turns out to be, so a limit can be
#passed by one recording: a recording cannot be weighed before it is fetched.
budgetWait <- function(source, now=Sys.time()) {
  limits <- downloadLimits(source)
  if (is.null(limits)) return(0)
  had <- downloadsSince(source, now)
  if (nrow(had) == 0) return(0)
  now <- as.numeric(now)
  wait <- 0
  for (window in c(hour=3600, day=86400)) {
    limit <- limits[[if (window == 3600) "hour" else "day"]]
    if (is.na(limit)) next
    inside <- had[had$when > now - window, , drop=FALSE]
    total <- sum(inside$bytes)
    if (total < limit) next
    #The oldest downloads leave the window first. The wait is until the one
    #whose leaving brings the rest under the limit has left.
    after <- total - cumsum(inside$bytes)
    leaving <- which(after < limit)[1]
    wait <- max(wait, inside$when[leaving] + window - now)
  }
  return(max(0, wait))
}

#Waits until a source's limits allow another download, saying so where asked.
#Waited out an hour at a time at most, so that a limit that is changed, or
#downloads that another agent writes down, are seen within the hour.
waitForBudget <- function(source, verbose=FALSE) {
  repeat {
    wait <- budgetWait(source)
    if (wait <= 0) return(invisible(TRUE))
    wait <- min(wait, 3600)
    if (verbose) {
      print(paste0("Download limits for ", source, " reached; waiting ", round(wait), " s"))
    }
    pause(wait + 1)
  }
}

#Whether a source is serving a recording it served before: asked of the address
#of the last recording this agent downloaded from it, with HEAD, which sends no
#recording and so costs nothing against its limits.
#
#This is what tells a recording that is not there from a source that has
#stopped serving this agent. Both look the same to a download -- iNaturalist
#answers 403 to a file it has not got -- but only one of them leaves a recording
#the source was serving a moment ago unserved.
#
#Asked only of a source with limits (see analyse()). A source that refuses HEAD
#while serving GET perfectly well would otherwise look as though it were
#refusing the agent for ever, and a recording it has really lost would never be
#recorded as missing. iNaturalist answers HEAD as it answers GET.
sourceServes <- function(address) {
  status <- responseHeaders(address)$status
  return(!is.na(status) && status >= 200 && status < 300)
}

#Whether a source is refusing this agent: a source with limits that will not
#serve the last recording the agent downloaded from it. lastGood holds those
#addresses, named by source. A source the agent has downloaded nothing from yet
#cannot be told to be refusing, and is taken not to be.
refusing <- function(source, lastGood) {
  if (is.null(downloadLimits(source))) return(FALSE)
  address <- unname(lastGood[source])
  if (length(address) != 1 || is.na(address)) return(FALSE)
  return(!sourceServes(address))
}

#How long an agent waits, after the given number of waits in a row, for a source
#that is refusing it to serve it again: 5 minutes, doubling each time, to 2
#hours at most, spread so that agents on one machine do not all ask again at once
refusalWait <- function(refusals) {
  return(spread(min(7200, 300 * 2^(max(1, refusals) - 1))))
}
