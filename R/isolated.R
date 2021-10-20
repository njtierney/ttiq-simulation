#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param infections
#' @param day
#' @return
#' @author Nick Golding
#' @export
isolated <- function(infections) {
  (infections$isolation_day - .abm_globals$day) < 14
}