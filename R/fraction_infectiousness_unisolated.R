#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param date_onset
#' @param date_isolation
#' @return
#' @author Nick Golding
#' @export
fraction_infectiousness_unisolated <- function(date_onset, date_isolation, infection_to_onset = 5) {
  date_infection <- date_onset - infection_to_onset
  delay <- as.numeric(date_isolation - date_infection)
  gi_cdf_discrete(delay)
}
