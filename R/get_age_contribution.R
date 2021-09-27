#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param asymp_rel_infectious
#' @return
#' @author Nicholas Tierney
#' @export
get_age_contribution <- function(asymp_rel_infectious = 0.5) {
  
  age_disaggregation <- age_group_10y_5y()
  
  age_data_davies <- read_susceptibility_clinical_fraction_age()
  
  # calculate relative infectiousness (using relative differences in clinical
  # fraction by age and assumed relative infectiousness of asymptomatics) and
  # susceptibility by age
  age_contribution <- age_disaggregation %>%
    left_join(
      age_data_davies,
      by = c("age_group_10y" = "age_group")
    ) %>%
    mutate(
      rel_infectiousness = clinical_fraction_mean +
        asymp_rel_infectious * (1 - clinical_fraction_mean),
      rel_infectiousness = rel_infectiousness /
        max(rel_infectiousness),
      rel_susceptibility = rel_susceptibility_mean /
        max(rel_susceptibility_mean),
    ) %>%
    select(
      age_group_5y,
      rel_infectiousness,
      rel_susceptibility
    )
  
  age_contribution
  
}