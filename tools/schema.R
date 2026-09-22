#Reads out the part of the audioBlast! schema that the analysis service works
#against: the tables an agent claims its work from, the routines that hand the
#work out, and the table it writes its measurements to. None of this is held in
#any repository, so it can only be read from a database that is running.
#
#Nothing here writes to the database, and nothing here handles a password: it
#is given a connection that has already been made.
#
#Usage:
#  db <- DBI::dbConnect(RMariaDB::MariaDB(), user=..., password=..., dbname="audioblast", host=..., port=3306)
#  source("tools/schema.R")
#  schemaReport(db)
#
#It writes audioblast-schema.txt beside the working directory and returns where
#it put it. DEFINER clauses, which name the user a routine was defined by, are
#taken out.

#The objects the analysis service reads and writes
schemaObjects <- function() {
  return(c("tasks", "tasks-progress", "tasks-agents", "tasks-data",
           "recordings-calculated", "v-recordings-calculated"))
}

#The routines that hand work to an agent and take it back again
schemaRoutines <- function() {
  return(c("get-tasks", "get-tasks-by-file", "delete-task"))
}

#What the database answered, as text, whatever shape it answered in. A
#definition is given as itself; anything else is printed as a table.
answerText <- function(answer) {
  if (is.character(answer)) return(paste("  !", answer))
  if (!is.data.frame(answer) || nrow(answer) == 0) return("  (nothing)")
  #SHOW CREATE ... answers with the definition in a column named for what it
  #made, e.g. "Create Table", "Create View", "Create Procedure"
  created <- grep("^Create ", names(answer), value=TRUE)
  if (length(created) > 0) {
    return(paste(withoutDefiner(answer[[created[1]]]), collapse="\n\n"))
  }
  return(paste(utils::capture.output(print(answer, row.names=FALSE)), collapse="\n"))
}

#A definition without the user it was defined by, which is nobody's business
#but the database's
withoutDefiner <- function(definition) {
  return(gsub("DEFINER=[^ ]+ ", "", definition))
}

#Asks the database, giving what it answered, or the complaint it made instead:
#a routine the connected user may not read should not stop the rest being read
ask <- function(db, sql) {
  return(tryCatch(DBI::dbGetQuery(db, sql), error=function(e) conditionMessage(e)))
}

#One part of the report: what was asked, and what came back
part <- function(db, title, sql) {
  return(c(paste("##", title), paste("#", sql), answerText(ask(db, sql)), ""))
}

#' Read out the schema the analysis service works against
schemaReport <- function(db, file="audioblast-schema.txt", schema="audioblast") {
  quoted <- DBI::dbQuoteString(db, schema)
  names <- paste(vapply(schemaObjects(), function(x) DBI::dbQuoteString(db, x), character(1)),
                 collapse=", ")

  report <- c(
    paste("# audioBlast! schema, read", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    part(db, "Server",
         "SELECT VERSION() AS `version`, DATABASE() AS `database`;"),
    part(db, "What each object is",
         paste0("SELECT `TABLE_NAME`, `TABLE_TYPE`, `ENGINE`, `TABLE_ROWS` ",
                "FROM `information_schema`.`TABLES` WHERE `TABLE_SCHEMA` = ", quoted,
                " AND `TABLE_NAME` IN (", names, ");")),
    part(db, "Routines this database holds",
         paste0("SELECT `ROUTINE_NAME`, `ROUTINE_TYPE` FROM `information_schema`.`ROUTINES` ",
                "WHERE `ROUTINE_SCHEMA` = ", quoted, ";")))

  for (object in schemaObjects()) {
    report <- c(report, part(db, object, paste0("SHOW CREATE TABLE `", object, "`;")))
  }
  for (routine in schemaRoutines()) {
    #A routine the user may not read with SHOW CREATE can often still be read
    #from information_schema, so both are asked
    report <- c(report, part(db, routine, paste0("SHOW CREATE PROCEDURE `", routine, "`;")))
    report <- c(report, part(db, paste(routine, "(from information_schema)"),
                             paste0("SELECT `ROUTINE_DEFINITION` FROM `information_schema`.`ROUTINES` ",
                                    "WHERE `ROUTINE_SCHEMA` = ", quoted,
                                    " AND `ROUTINE_NAME` = ", DBI::dbQuoteString(db, routine), ";")))
  }

  report <- c(
    report,
    #Not schema, but it says how much work is held by agents that may be long
    #gone: a claim is never given up, so a task an agent died holding is
    #counted as being done and is offered to nobody
    part(db, "Work in hand",
         paste("SELECT COUNT(*) AS `claimed`, COUNT(DISTINCT `process`) AS `processes`,",
               "MIN(`started`) AS `oldest_claim`,",
               "SUM(`started` < NOW() - INTERVAL 1 DAY) AS `held_over_a_day`,",
               "SUM(`started` < NOW() - INTERVAL 7 DAY) AS `held_over_a_week`",
               "FROM `tasks-progress`;")),
    part(db, "Work waiting, by task",
         "SELECT `task`, COUNT(*) AS `tasks` FROM `tasks` GROUP BY `task` ORDER BY `tasks` DESC;"),
    part(db, "Which agent does which task",
         "SELECT * FROM `tasks-agents`;"))

  writeLines(report, file)
  cat(paste(report, collapse="\n"), "\n")
  return(invisible(normalizePath(file)))
}
