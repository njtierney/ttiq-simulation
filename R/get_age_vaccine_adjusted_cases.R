#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ve_symptoms
#' @param ve_susceptibility
#' @param ve_onward
#' @param vaccination_coverage
#' @param oz_baseline_matrix
#' @return
#' @author dhduncan
#' @export
get_age_vaccine_adjusted_cases <- function(
  scenario_clinical_fraction,
  oz_baseline_matrix,
  detection_vaccinated_asymptomatic = 1,
  detection_vaccinated_symptomatic = 1,
  detection_unvaccinated_asymptomatic = 1,
  detection_unvaccinated_symptomatic = 1
) {
  
  stable_state_df <- scenario_clinical_fraction %>% 
    nest(data = everything()) %>%
    rowwise() %>%
    mutate(
      stable_state = list(
        get_stable_state(
          efficacy_susceptibility = data$ve_susceptibility,
          efficacy_onward = data$ve_onward,
          coverage_any_vaccine = data$vaccination_coverage,
          oz_baseline_matrix = oz_baseline_matrix
        )
      )
    ) %>% 
    # age_weight_unvax <- stable_state[1:17]
    # age_weight_vax <- stable_state[18:34]
    mutate(
      age_weight_unvax = list(vec_slice(stable_state, 1:17)),
      age_weight_vax = list(vec_slice(stable_state, 18:34)),
      .before = everything()
    ) %>% 
    ungroup() %>% 
    select(-stable_state) %>% 
    unnest(
      cols = c(
        age_weight_unvax,
        age_weight_vax,
        data
      )
    )
  
  # different levels of vaccine coverage per age_group?
  # different susceptibility - is that already hardwired?
  
  clinical_fraction_unvax <- read_susceptibility_clinical_fraction_age() %>% 
    select(
      age_group,
      clinical_fraction_mean
    ) %>% 
    left_join(
      age_group_10y_5y(), 
      by = c("age_group" = "age_group_10y")
    ) %>% 
    select(
      age_5_year = age_group_5y,
      clinical_fraction_unvax = clinical_fraction_mean
    )
  
  # applying one 
  frac_vax_symptomatic_df <- stable_state_df %>% 
    # mutate(
    #   across(
    #     starts_with("age_weight"),
    #     ~.x * sign(.x[1])
    #   )
    # ) %>%
    left_join(
      clinical_fraction_unvax,
      by = "age_5_year"
    ) %>%
    mutate(
      clinical_fraction_vax = clinical_fraction_unvax * (1- ve_symptoms)
    ) %>% 
    summarise(
      unvax_symptomatic = sum(age_weight_unvax * clinical_fraction_unvax),
      unvax_asymptomatic = sum(age_weight_unvax * (1 - clinical_fraction_unvax)),
      vax_symptomatic = sum(age_weight_vax * clinical_fraction_vax),
      vax_asymptomatic = sum(age_weight_vax * (1 - clinical_fraction_vax)),
    ) %>% 
    mutate(
      unvax_symptomatic = unvax_symptomatic * detection_unvaccinated_symptomatic,
      vax_symptomatic = vax_symptomatic * detection_vaccinated_symptomatic,
      unvax_asymptomatic = unvax_asymptomatic * detection_unvaccinated_asymptomatic,
      vax_asymptomatic = vax_asymptomatic * detection_vaccinated_asymptomatic,
      
      all_cases = unvax_symptomatic + unvax_asymptomatic + vax_symptomatic + vax_asymptomatic,
      frac_unvax_symptomatic = unvax_symptomatic / all_cases,
      frac_unvax_asymptomatic = unvax_asymptomatic / all_cases,
      frac_vax_symptomatic = vax_symptomatic / all_cases,
      frac_vax_asymptomatic = vax_asymptomatic / all_cases
    ) %>% 
    select(
      frac_unvax_symptomatic,
      frac_unvax_asymptomatic,
      frac_vax_symptomatic,
      frac_vax_asymptomatic
    ) %>% 
    ungroup()  %>% 
    rowwise() %>% 
    mutate(
      sum_check = sum(across(starts_with("frac_"))),
      .before = everything()
    )
  
  df_age_vaccine_adjusted_cases <- frac_vax_symptomatic_df %>% 
    pivot_longer(
      cols = starts_with("frac_"),
      names_to = "status",
      values_to = "fraction"
    ) 
  
  df_age_vaccine_adjusted_cases

}
