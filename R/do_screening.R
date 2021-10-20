#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param infections
#' @return
#' @author Nick Golding
#' @export
do_screening <- function(infections) {

  # apply passive detection to infections, and put them in isolation as soon as
  # they are detected
  
  # only detectable if they are not in isolation, are symptomatic, and are
  # exactly 5 days post infection
  detectable <- infections$symptomatic &
    !is.finite(infections$isolation_day) &
    (.abm_globals$day - infections$infection_day) == 5
  
  # then they have a 50% probability of detection
  p_detection <- ifelse(
    detectable,
    .abm_parameters$passive_detection_given_symptoms,
    0
  )
  
  detected <- rbinom(nrow(infections), 1, p_detection)
  
  # put them in isolation on this day if detected
  infections$isolation_day <- ifelse(
    detected,
    .abm_globals$day,
    infections$isolation_day
  )
  
  infections$case_found_by <- ifelse(
    detected,
    "screening",
    infections$case_found_by
  )
  
  infections

}
