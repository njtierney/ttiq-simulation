#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_multiplier
#' @param p_passive_detection_vaccinated
#' @param tp_multiplier
#' @param isolation_stringency
#' @param vaccination_coverage
#' @return
#' @author Nicholas Tierney
#' @export
create_scenario_vaccination_isolation <- function(vaccination_multiplier = 0.3,
                                                  p_passive_detection_vaccinated
                                                  = 0.15, tp_multiplier = 0.46,
                                                  isolation_stringency = seq(0,
                                                  1, by = 0.2),
                                                  vaccination_coverage =
                                                  seq(0.6, 0.9, by = 0.1)) {

  expand_grid(
    vaccination_multiplier = vaccination_multiplier,
    p_passive_detection_vaccinated = p_passive_detection_vaccinated,
    # baseline - if we treated vaccinated people the same as unvaccinated ppl
    # isn't this estimated from the data?
    tp_multiplier = tp_multiplier,
    isolation_stringency = isolation_stringency,
    vaccination_coverage = vaccination_coverage
  ) 

}
