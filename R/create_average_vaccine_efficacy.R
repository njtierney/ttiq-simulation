#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage_age_group_at_milestone_5_year
#' @return
#' @author Nicholas Tierney
#' @export
create_average_vaccine_efficacy <- function(vaccination_coverage_age_group_at_milestone_5_year) {

  replace_nan <- function(x, value){
    x[is.nan(x)] <- value
    x
  }
  
  re_weighted_vaccine_efficacy <- vaccination_coverage_age_group_at_milestone_5_year %>% 
    mutate(
      across(
        .cols = ends_with("coverage"),
        ~.x / any_vaccine
      )
    ) %>% 
    mutate(
      across(
        .cols = ends_with("coverage"),
        ~replace_nan(.x, 0)
      )
    )
  
  average_vaccine_efficacy <- re_weighted_vaccine_efficacy %>%
    rowwise() %>% 
    mutate(
      ve_susceptibility = ve(
        "susceptibility",
        fraction_az_dose_1 = AstraZeneca_1_coverage,
        fraction_az_dose_2 = AstraZeneca_2_coverage,
        fraction_pfizer_dose_1 = Pfizer_1_coverage,
        fraction_pfizer_dose_2 = Pfizer_2_coverage
      ),
      ve_onward = ve(
        "onward",
        fraction_az_dose_1 = AstraZeneca_1_coverage,
        fraction_az_dose_2 = AstraZeneca_2_coverage,
        fraction_pfizer_dose_1 = Pfizer_1_coverage,
        fraction_pfizer_dose_2 = Pfizer_2_coverage
      ),
      ve_symptoms = ve(
        "symptoms",
        fraction_az_dose_1 = AstraZeneca_1_coverage,
        fraction_az_dose_2 = AstraZeneca_2_coverage,
        fraction_pfizer_dose_1 = Pfizer_1_coverage,
        fraction_pfizer_dose_2 = Pfizer_2_coverage
      )
    ) %>% 
    select(
      - ends_with("coverage")
    )
  
  average_vaccine_efficacy

}
