#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param efficacy_susceptibility
#' @param efficacy_onward
#' @param coverage_any_vaccine
#' @return
#' @author Nicholas Tierney
#' @export
fraction_cases_unvaccinated <- function(ve_susceptibility,
                                        ve_onward_transmission,
                                        vaccination_coverage,
                                        baseline_matrix) {

  stable_state <- get_stable_state(
    efficacy_susceptibility = ve_susceptibility,
    efficacy_onward = ve_onward_transmission,
    coverage_any_vaccine = vaccination_coverage,
    baseline_matrix = baseline_matrix
  )
  
  fraction_cases_unvaccinated <- sum(stable_state[1:17]) / sum(stable_state[1:34])
  
  return(fraction_cases_unvaccinated)
  
}
