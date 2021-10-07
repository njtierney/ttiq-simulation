#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_run_vaccination_isolation
#' @return
#' @author Nicholas Tierney
#' @export
gg_scenario_vacc_iso <- function(scenario_run_vaccination_isolation) {

  # this is run for combinations of vaccination coverage and isollation stringency, so we can pllot that
  # we talked about plotting it as an onion or bullseye type thing
  # but maybe just bars of that value againts isolation stringency, faceted by vaccination cpverage
  # that would take the polly lless time to understand
  # and nest report is going to Mark, Gladys, Dan, etc.
  
  # some kind of an onion plot
  scenario_run_vaccination_isolation %>%
    filter(
      ve_onward_transmission == ve_onward_transmission[1],
      !no_passive_detection_vaccinated,
      fraction_vaccinated_low_risk == 1,
      vacc_setting_risk_ratio == vacc_setting_risk_ratio[1]
    ) %>%
    mutate(
      vaccination_coverage_percent = 100 * vaccination_coverage
    ) %>%
    ggplot(
      aes(x = isolation_stringency_vaccinated,
          y = weighted_tp_reduction_scaled)) + 
    geom_col() +
    facet_wrap( ~ vaccination_coverage_percent,
                labeller = label_glue(
                  "{vaccination_coverage_percent}% vaccination coverage"
                  ),
                nrow = 1
                ) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_reverse(breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(labels = scales::percent) +
    ylab("TP reduction") +
    xlab("Isolation stringency") +
    theme_minimal()

}


