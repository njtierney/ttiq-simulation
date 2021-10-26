#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param oz_baseline_matrix
#' @param ve_onward_transmission
#' @param ve_susceptibility
#' @param ve_symptoms
#' @param vaccination_coverage
#' @param vaccination_age_min
#' @return
#' @author Nicholas Tierney
#' @export
prepare_infections_vax_symp <- function(oz_baseline_matrix,
                                        average_vaccine_efficacy,
                                        vaccination_coverage_milestones) {

  # average_vaccine_efficacy
  scenarios_average_vaccine_efficacy <- average_vaccine_efficacy %>% 
    rename(vaccination_coverage = any_vaccine)

  # assume a 50% detection probability for symptomatics due to passive
  # and a 0% detection of asymptommatics due to passive
  # assume a 20% detection probability for all due to active
  test_seeking <- 0.5
  active_prob <- 0.2
  
  # nested within milestone are the parameters for each age bracket, 0-4, etc.)
  nested_scenarios_average_vaccine_efficacy <- 
    scenarios_average_vaccine_efficacy %>% 
    group_by(milestone) %>%
    nest() %>%
    filter(
      milestone %in% c(
        "over_16_over_20_pct",
        "over_16_over_30_pct",
        "over_16_over_40_pct",
        "over_16_over_50_pct",
        "over_16_over_60_pct",
        "over_16_over_70_pct",
        "over_16_over_80_pct",
        "terminal"
      )
    )
  
  scenario_cases <- nested_scenarios_average_vaccine_efficacy %>%
    summarise(
      get_age_vaccine_adjusted_cases(
        scenario_clinical_fraction = data[[1]],
        oz_baseline_matrix = oz_baseline_matrix,
        detection_unvaccinated_asymptomatic = active_prob,
        detection_vaccinated_asymptomatic = active_prob,
        detection_unvaccinated_symptomatic = 1 - ((1- test_seeking) * (1 - active_prob)),
        detection_vaccinated_symptomatic = 1 - ((1- test_seeking) * (1 - active_prob))
      ),
      .groups = "drop"
    )
  
  scenario_infections <- nested_scenarios_average_vaccine_efficacy %>%
    summarise(
      get_age_vaccine_adjusted_cases(
        scenario_clinical_fraction = data[[1]],
        oz_baseline_matrix = oz_baseline_matrix
      ),
      .groups = "drop"
    )
  
  labels <- tibble::tribble(
    ~status, ~label, ~colour, ~vaccinated, ~symptomatic,
    "frac_unvax_symptomatic", "unvaccinated & symptomatic", "red", FALSE, TRUE,
    "frac_unvax_asymptomatic", "unvaccinated & asymptomatic", "pink", FALSE, FALSE,
    "frac_vax_symptomatic", "vaccinated & symptomatic", "blue", TRUE, TRUE,
    "frac_vax_asymptomatic", "vaccinated & asymptomatic", "light blue", TRUE, FALSE
  )
  
  df_vac_coverage_scenario <- bind_rows(
    `All infections` = scenario_infections,
    `Cases` = scenario_cases,
    .id = "which"
  ) %>% 
    left_join(
      select(
        vaccination_coverage_milestones,
        vaccination_coverage = coverage_over_16,
        milestone
      ),
      by = "milestone"
    ) %>%
    left_join(
      labels,
      by = "status"
    ) %>%
    mutate(
      vaccination_coverage_percent = paste0(100 * vaccination_coverage, "%"),
      vaccination_coverage_percent = factor(
        vaccination_coverage_percent,
        levels = str_sort(
          unique(vaccination_coverage_percent),
          numeric = TRUE
        )
      ),
      status = case_when(
        vaccinated & symptomatic ~ "Vaccinated & Symptomatic",
        !vaccinated & symptomatic ~ "Unvaccinated & Symptomatic",
        vaccinated & !symptomatic ~ "Vaccinated & Asymptomatic",
        !vaccinated & !symptomatic ~ "Unvaccinated & Asymptomatic",
      ),
      status = factor(status, 
                      levels = c("Unvaccinated & Symptomatic",
                                 "Unvaccinated & Asymptomatic",
                                 "Vaccinated & Symptomatic",
                                 "Vaccinated & Asymptomatic"),
                      ordered = TRUE)
    )
    
  df_vac_coverage_scenario
  
}
