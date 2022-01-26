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

# this represents routine workplace screening
  
# assumptions:
   # there is a subset of new infections that are screenable in a workplace context
  # can't already be in isolation
  # some prob of being screened (0 in isolation else the prob e.g 5/7 if they work 5 days a week) 
  # they test positive on days 4-7 post infection (assuming RATs)
  # put them in isolation as soon as they are detected
  
  # only detectable if they are not in isolation
  detectable <- !is.finite(infections$isolation_day)
  
  # in kind of workforce subject to routine screening
  p_essential <- 0.30  # how to parameterise this - different estimates and definitions. this link is to a high one https://www.sgsep.com.au/publications/insights/closing-the-divide-essential-workers-australian-cities-and-covid-19, i've gone arbitrarily lower
  
  p_screened <- 5/7  # assuming works 5 of 7 days
 
  days_since_infection <- .abm_globals$day - infections$infection_day 
   
  p_positive <- ifelse(
    days_since_infection >= 4 & days_since_infection <= 7, 
    1, # screened between days 4-7 post infection
    0
    ) 
  
  p_detection = ifelse(
    detectable,
    p_essential * p_screened * p_positive,
    0)
 
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
    "workplace_screening",
    infections$case_found_by
  )
  
  infections

}
