#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ... extra args to expand_grid
#' @return
#' @author Nicholas Tierney
#' @export
create_scenario_vaccination_isolation <- function(...) {

  expand_grid(
    ...
  ) %>%
    mutate(
      
      # compute p_passive for the vaccinated - accounting for reduction in
      # symptoms and relative probability of a vaccinated person seeking a test
      # for symptoms
      p_passive_detection_vaccinated = p_passive_detection * ve_symptoms * rel_test_seeking_vaccinated,
      
      # turn this off altogether for that scenario
      p_passive_detection_vaccinated = case_when(
        no_passive_detection_vaccinated ~ 0,
        TRUE ~ p_passive_detection_vaccinated
      )
      
    )

}
