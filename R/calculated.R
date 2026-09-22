#The measurements recordings-calculated holds of a recording, in the order they
#are written (see measureFile()). source and id say which recording they are
#of, and are its key, so they are written but never updated.
measurementColumns <- function() {
  return(c("hash", "duration", "channels", "sample_rate", "bit_depth", "bit_rate",
           "codec", "size_raw", "status", "error"))
}

#The statement that writes the measurements of one recording, adding a row for
#a recording that has not been measured before and replacing the measurements
#of one that has. A measurement that failed is written as readily as one that
#did not, so that a recording that cannot be read is recorded as such rather
#than left looking as though it were still waiting to be measured.
#
#The time is the database's own, not the agent's: agents measure on whatever
#machine they are given, and clocks that disagree would date the rows wrongly.
calculatedSQL <- function() {
  columns <- c("source", "id", measurementColumns(), "calculated_at", "calculated_by")
  placeholders <- ifelse(columns == "calculated_at", "NOW()", "?")
  updated <- setdiff(columns, c("source", "id"))
  return(paste(
    "INSERT INTO `recordings-calculated`",
    paste0("(", paste0("`", columns, "`", collapse=", "), ")"),
    "VALUES", paste0("(", paste(placeholders, collapse=", "), ")"),
    "ON DUPLICATE KEY UPDATE",
    paste0("`", updated, "` = VALUES(`", updated, "`)", collapse=", ")))
}

#Writes the measurements of a recording, returning whether the database kept
#them (see abdbExecute()).
writeMeasurements <- function(db, source, id, measurements) {
  params <- c(
    list(as.character(source), as.character(id)),
    unname(as.list(measurements[1, measurementColumns()])),
    list(analyserVersion()))
  return(abdbExecute(db, calculatedSQL(), params=params))
}

#What a recording has been measured as before, as a list of its status and what
#measured it, or NULL where nothing has measured it yet. A recording that is
#not in the table has never been measured; one that is has been, however it
#went.
calculatedStatus <- function(db, source, id) {
  sql <- paste("SELECT `status`, `calculated_by` FROM `recordings-calculated`",
               "WHERE `source` = ? AND `id` = ? LIMIT 1;")
  rows <- abdbGetQuery(db, sql, params=list(as.character(source), as.character(id)))
  if (!is.data.frame(rows) || nrow(rows) == 0) return(NULL)
  return(as.list(rows[1, , drop=FALSE]))
}

#What measured a recording, as the package and its version, so that
#measurements made before a fix can be found and made again
analyserVersion <- function() {
  version <- tryCatch(as.character(utils::packageVersion("audioBlastAnalyse")),
                      error=function(e) "unknown")
  return(paste("audioBlastAnalyse", version))
}
