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
   
  start_day <- switch(.abm_parameters$isolation_start_day, 
                      infection=infections$infection_day,
                      isolation=infections$isolation_day)
  
  isolation_days <- ifelse(infections$vaccinated, .abm_parameters$isolation_days_vax, 14)
  
 infections$isolated & 
    .abm_globals$day > start_day &
    ((.abm_globals$day - start_day) <= isolation_days)
  
 }


