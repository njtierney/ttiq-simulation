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
infectiousness <- function(infections, parameters, day) {
  # compute infectiousness from GI distribution, isolation status, vaccination_status etc.
  
  day_diff <- day - infections$infection_day
  isolation_multiplier <- ifelse(isolated(infections, day), 0, 1)
  vaccination_multiplier <- ifelse(infections$vaccinated, parameters$ve_onward, 1)
  parameters$R * gi_pmf_discrete(day_diff) * isolation_multiplier * vaccination_multiplier
  
}
