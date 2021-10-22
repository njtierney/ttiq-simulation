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
                                        ve_onward_transmission = 0.5,
                                        ve_susceptibility = 0.73, ve_symptoms =
                                        0.78, vaccination_coverage = c(0.5,
                                        0.6, 0.7, 0.8, 0.9, 0.95, 1),
                                        vaccination_age_min = 12) {

  scenarios <- get_clinical_fraction_scenarios(
    ve_onward_transmission = ve_onward_transmission,
    ve_susceptibility = ve_susceptibility,
    ve_symptoms = ve_symptoms,
    vaccination_coverage = vaccination_coverage,
    vaccination_age_min = 12
  ) %>%
    ungroup() %>%
    mutate(
      id = row_number()
    )
  
  # assume a 50% detection probability for symptomatics due to passive
  # and a 0% detection of asymptommatics due to passive
  # assume a 20% detection probability for all due to active
  test_seeking <- 0.5
  active_prob <- 0.2
  
  cases <- get_age_vaccine_adjusted_cases(
    scenarios,
    baseline_matrix = oz_baseline_matrix,
    detection_unvaccinated_asymptomatic = active_prob,
    detection_vaccinated_asymptomatic = active_prob,
    detection_unvaccinated_symptomatic = 1 - ((1- test_seeking) * (1 - active_prob)),
    detection_vaccinated_symptomatic = 1 - ((1- test_seeking) * (1 - active_prob))
  ) %>%
    left_join(
      select(
        scenarios,
        id,
        vaccination_coverage
      ),
      by = c(scenario_id = "id")
    )
  
  infections <- get_age_vaccine_adjusted_cases(
    scenarios,
    baseline_matrix = oz_baseline_matrix
  ) %>%
    left_join(
      select(
        scenarios,
        id,
        vaccination_coverage
      ),
      by = c(scenario_id = "id")
    )
  
  labels <- tibble::tribble(
    ~status, ~label, ~colour, ~vaccinated, ~symptomatic,
    "frac_unvax_symptomatic", "unvaccinated & symptomatic", "red", FALSE, TRUE,
    "frac_unvax_asymptomatic", "unvaccinated & asymptomatic", "pink", FALSE, FALSE,
    "frac_vax_symptomatic", "vaccinated & symptomatic", "blue", TRUE, TRUE,
    "frac_vax_asymptomatic", "vaccinated & asymptomatic", "light blue", TRUE, FALSE
  )
  
  df_vac_coverage <- bind_rows(
    `All infections` = infections,
    `Cases` = cases,
    .id = "which"
  ) %>%
    # infections %>%
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
  
  df_vac_coverage

}
