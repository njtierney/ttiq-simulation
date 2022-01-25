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

  # rename this one to make it clear that its about the symptomatic presentation (passive surveillance)
  
 # make another one that's actually about he workplace kind of screening to pick up undetected cases
  
   #subset of new infections that are screenable
  # can't be in isolation
  # some prob of being screened (0 in isolation else the prob e.g 5/7 work days) 
  # test positive on days 4-7 post infection (assuming RATs)
  # new type of case_found_by
  
  # apply passive detection to infections, and put them in isolation as soon as
  # they are detected
  
  # only detectable if they are not in isolation and are symptomatic
  detectable <- infections$symptomatic &
    !is.finite(infections$isolation_day)
    
    
  # is the day of test seeking
  days_since_infection <- .abm_globals$day - infections$infection_day 
  
  # a multiplier on detection for the vaccinated
  vaccination_multiplier <- ifelse(
    infections$vaccinated,
    .abm_parameters$vaccination_test_seeking_multiplier,
    1
  )
  
  # they have a 50% probability of detection overall, or possibly lower if
  # vaccinated, scaled by the probability that this is the day of detection
  p_detection <- .abm_parameters$passive_detection_given_symptoms *
    time_to_symptomatic_test_pmf(days_since_infection) *
    vaccination_multiplier *
    detectable
  
  detected <- rbinom(nrow(infections), 1, p_detection)
  
  # put them in isolation on this day if detected
  infections$isolation_day <- ifelse(
    detected,
    .abm_globals$day,
    infections$isolation_day
  )
  
  infections$isolated <- ifelse(
    detected,
    TRUE,
    infections$isolated
  )
  
  
  infections$case_found_by <- ifelse(
    detected,
    "screening",
    infections$case_found_by
  )
  
  infections

}
