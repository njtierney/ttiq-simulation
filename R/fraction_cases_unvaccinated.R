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
                                        coverage_any_vaccine,
                                        baseline_matrix = baseline_matrix()) {
  # transmission between unvaccinated people, no effect of vaccines and scale down
  # to vaccinated population
  unvax_unvax <- baseline_matrix %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  # transmission between vaccinated people, susceptibility and onward transmission
  # effects and scale down to vaccinated population
  vax_vax <- baseline_matrix %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from unvaccinated to vaccinated people (account for
  # susceptibility effects on rows) and scale down to vaccinated population
  # fraction
  unvax_vax <- baseline_matrix %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from vaccinated to unvaccinated people (account for transmission
  # effects) and scale down to unvaccinated population
  vax_unvax <- baseline_matrix %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  vax_structured_matrix <- rbind(cbind(unvax_unvax, vax_unvax),
                                 cbind(unvax_vax, vax_vax))
  
  stable_state <- Re(eigen(vax_structured_matrix)$vectors[, 1])
  
  fraction_cases_unvaccinated <- 
    sum(stable_state[1:17]) / sum(stable_state[1:34])
  
  # normalise age-weighted sum - 
  
  age_weight <- stable_state/sum(stable_state[1:34])
# sum((n_cases_per_age/total_N_cases * pr_symptomatic_per_age)...)
  
  davies_clinical_fraction <- read_susceptibility_clinical_fraction_age() %>% 
    select(age_group, clinical_fraction_mean) %>% 
    left_join(age_group_10y_5y(), 
              by=c("age_group" = "age_group_10y")) %>% 
    pull(clinical_fraction_mean)
  
  clinical_fractions <- rep(davies_clinical_fraction, times = 2)
  
  age_weighted_clinical_fractions <- age_weight * clinical_fractions
  
  # normalise the result so that it sums to one - Wait - this goes earlier I think?
  
  # multiply the RHS vaccinated cases by 1 - vaccine effectiveness (around 0.76? - taking midpoint of AZ and mRNA) so 0.24 to reduce the age-weighted clinical fraction for vaccinated
  
  ve_symptomatic <- rep(c(1, 0.24), each = 17)
  
  # return the age-weighted clinical fraction of cases
  vaccine_age_adjusted_clinical_fraction <- age_weighted_clinical_fractions * ve_symptomatic
  
  vaccine_age_adjusted_clinical_fraction
  
  fraction_cases_unvaccinated
  
}
