#' Run all analyses
#'
#' @param db database connector
#' @param force If TRUE recalculates all values
#' @export
analyse <- function(db, force=FALSE) {
  a_tdsc(db, force)
}
