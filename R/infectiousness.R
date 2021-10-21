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
infectiousness <- function(infections) {
  # compute infectiousness from GI distribution, isolation status, vaccination_status etc.

  day_diff <- .abm_globals$day - infections$infection_day
  
  isolation_multiplier <- ifelse(
    isolated(infections),
    0,
    1
  )
  
  vaccination_multiplier <- ifelse(
    infections$vaccinated,
    1 - .abm_parameters$ve_onward,
    1
  )
  
  symptomatic_multiplier <- ifelse(
    infections$symptomatic,
    1,
    .abm_parameters$asymptomatic_relative_infectiousness
  )
  
  .abm_parameters$R_star *
    gi_pmf_discrete(day_diff) *
    isolation_multiplier *
    vaccination_multiplier *
    symptomatic_multiplier
  
}
