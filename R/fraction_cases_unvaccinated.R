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
fraction_cases_unvaccinated <- function(efficacy_susceptibility,
                                        efficacy_onward,
                                        coverage_any_vaccine) {
  # transmission between unvaccinated people, no effect of vaccines and scale down
  # to vaccinated population
  unvax_unvax <- baseline_matrix() %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  # transmission between vaccinated people, susceptibility and onward transmission
  # effects and scale down to vaccinated population
  vax_vax <- baseline_matrix() %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from unvaccinated to vaccinated people (account for
  # susceptibility effects on rows) and scale down to vaccinated population
  # fraction
  unvax_vax <- baseline_matrix() %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from vaccinated to unvaccinated people (account for transmission
  # effects) and scale down to unvaccinated population
  vax_unvax <- baseline_matrix() %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  vax_structured_matrix <- rbind(cbind(unvax_unvax, vax_unvax),
                                 cbind(unvax_vax, vax_vax))
  
  stable_state <- Re(eigen(vax_structured_matrix)$vectors[, 1])
  fraction_cases_unvaccinated <-
    stable_state[1:17] / (stable_state[1:17] + stable_state[18:34])
  
  fraction_cases_unvaccinated
  
}
