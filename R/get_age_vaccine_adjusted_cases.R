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
get_age_vaccine_adjusted_cases <- function(scenario_clinical_fraction,
# c(0.5, 0.7, 0.8, 0.9, 0.95, 1), # but Chris only needs one for now - 0.8
                                           baseline_matrix) {

# baseline matrix or oz_baseline_matrix?
  stable_state_df <- scenario_clinical_fraction %>% 
    rowwise() %>% 
    mutate(stable_state = list(get_stable_state(
        efficacy_susceptibility = ve_susceptibility,
        efficacy_onward = ve_onward_transmission,
        coverage_any_vaccine = vaccination_coverage,
        baseline_matrix = baseline_matrix
    ))) %>% 
    # age_weight_unvax <- stable_state[1:17]
    # age_weight_vax <- stable_state[18:34]
    mutate(age_weight_unvax = list(vec_slice(stable_state, 1:17)),
           age_weight_vax = list(vec_slice(stable_state, 18:34)),
           .before = everything()) %>% 
    ungroup() %>% 
    select(-stable_state) %>% 
    # temporary ID to assign to these different scenarios, since
    # one row is a separate scenario, that is how we are assigning these
    # these don't (yet) have any meaning
    rowid_to_column(var = "scenario_id") %>% 
    unnest(
      cols = c(
        age_weight_unvax,
        age_weight_vax
      )
    )
  
  # different levels of vaccine coverage per age_group?
  # different susceptibility - is that already hardwired?
  
  clinical_fraction_unvax <- read_susceptibility_clinical_fraction_age() %>% 
    select(age_group, clinical_fraction_mean) %>% 
    left_join(age_group_10y_5y(), 
              by=c("age_group" = "age_group_10y")) %>% 
    pull(clinical_fraction_mean)
  
  # applying one 
  frac_vax_symptomatic_df <- stable_state_df %>% 
    group_by(scenario_id) %>% 
    mutate(
      clinical_fraction_vax = clinical_fraction_unvax * (1- ve_symptoms),
      unvax_symptomatic = sum(age_weight_unvax * clinical_fraction_unvax),
      unvax_asymptomatic = sum(age_weight_unvax * (1 - clinical_fraction_unvax)),
      vax_symptomatic = sum(age_weight_vax * clinical_fraction_vax),
      vax_asymptomatic = sum(age_weight_vax * (1 - clinical_fraction_vax)),
      all_cases = unvax_symptomatic + unvax_asymptomatic + vax_symptomatic + vax_asymptomatic,
      frac_unvax_symptomatic = unvax_symptomatic / all_cases,
      frac_unvax_asymptomatic = unvax_asymptomatic / all_cases,
      frac_vax_symptomatic = vax_symptomatic / all_cases,
      frac_vax_asymptomatic = vax_asymptomatic / all_cases
    ) %>% 
    ungroup()
  
  df_age_vaccine_adjusted_cases <- frac_vax_symptomatic_df %>% 
    pivot_longer(
      cols = starts_with("frac_"),
      names_to = "status",
      values_to = "fraction"
    ) 
  
  # age_vaccine_adjusted_cases <- tibble(
  #   status = c("Unvaccinated & symptomatic",
  #              "Unvaccinated & asymptomatic",
  #              "Vaccinated & symptomatic",
  #              "Vaccinated & asymptomatic"),
  #   fraction = c(frac_unvax_symptomatic, frac_unvax_asymptomatic, frac_vax_symptomatic, frac_vax_asymptomatic)
  # )
  
  return(age_vaccine_adjusted_cases)

}
