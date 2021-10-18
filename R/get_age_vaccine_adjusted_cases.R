#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ve_symptoms
#' @param ve_susceptibility
#' @param ve_onward_transmission
#' @param vaccination_coverage
#' @param baseline_matrix
#' @return
#' @author dhduncan
#' @export
get_age_vaccine_adjusted_cases <- function(scenario_infections_clinical_fraction,
                                           baseline_matrix) {

# baseline matrix or oz_baseline_matrix?  
  results <- scenario_infections_clinical_fraction %>% 
    rowwise %>% 
    fraction_cases_vaccinated = get_vaccinated(age_vacc_adjusted_cases)
    stable_state = get_stable_state(
    efficacy_susceptibility = ve_susceptibility,
    efficacy_onward = ve_onward_transmission,
    coverage_any_vaccine = vaccination_coverage,
    baseline_matrix = oz_baseline_matrix
    )
  
  # different levels of vaccine coverage per age_group?
  # different susceptibility - is that already hardwired?
  
  # aim is to get to sum((n_cases_per_age/total_N_cases * pr_symptomatic_per_age)...)
  
  age_weight_unvax <- stable_state[1:17]
  age_weight_vax <- stable_state[18:34]
  
  clinical_fraction_unvax <- read_susceptibility_clinical_fraction_age() %>% 
    select(age_group, clinical_fraction_mean) %>% 
    left_join(age_group_10y_5y(), 
              by=c("age_group" = "age_group_10y")) %>% 
    pull(clinical_fraction_mean)
  
  # applying one 
  clinical_fraction_vax <- clinical_fraction_unvax * (1- ve_symptoms)
  
  unvax_symptomatic <- sum(age_weight_unvax * clinical_fraction_unvax)
  
  unvax_asymptomatic <- sum(age_weight_unvax * (1 - clinical_fraction_unvax))
  
  vax_symptomatic <- sum(age_weight_vax * clinical_fraction_vax)
  
  vax_asymptomatic <- sum(age_weight_vax * (1 - clinical_fraction_vax))
  
  all_cases <- unvax_symptomatic + unvax_asymptomatic + vax_symptomatic + vax_asymptomatic
  
  frac_unvax_symptomatic <- unvax_symptomatic/all_cases
  frac_unvax_asymptomatic <- unvax_asymptomatic/all_cases
  frac_vax_symptomatic <- vax_symptomatic/all_cases
  frac_vax_asymptomatic <- vax_asymptomatic/all_cases
  
  age_vaccine_adjusted_cases <- tibble(
    status = c("Unvaccinated & symptomatic",
               "Unvaccinated & asymptomatic",
               "Vaccinated & symptomatic",
               "Vaccinated & asymptomatic"),
    fraction = c(frac_unvax_symptomatic, frac_unvax_asymptomatic, frac_vax_symptomatic, frac_vax_asymptomatic)
  )
  
  return(age_vaccine_adjusted_cases)

}
