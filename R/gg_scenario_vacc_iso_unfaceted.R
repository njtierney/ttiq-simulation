#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_run_vaccination_isolation
#' @return
#' @author Nicholas Tierney
#' @export
gg_scenario_vacc_iso_unfaceted <- function(scenario_run_vaccination_isolation) {

  scenario_run_vaccination_isolation %>%
    filter(
      ve_onward_transmission == ve_onward_transmission[1],
      !no_passive_detection_vaccinated,
      fraction_vaccinated_low_risk == 1,
      vacc_setting_risk_ratio == vacc_setting_risk_ratio[1]
    ) %>%
    mutate(
      vaccination_coverage_percent = 100 * vaccination_coverage,
      vaccination_coverage_percent = as_factor(vaccination_coverage_percent),
      vaccination_coverage_percent = fct_relevel(
      .f = vaccination_coverage_percent,
      c("90", "80", "70")
      )
    ) %>% 
    ggplot(
      aes(x = isolation_stringency_vaccinated,
          y = weighted_tp_reduction_scaled,
          fill = factor(vaccination_coverage_percent))) + 
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Dark2") + 
    scale_x_reverse(breaks = seq(0, 1, by = 0.2),
                    labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Isolation stringency",
         y = "TP reduction",
         fill = "Vaccination Coverage %\n(double dose)") +
    theme_minimal()

}
