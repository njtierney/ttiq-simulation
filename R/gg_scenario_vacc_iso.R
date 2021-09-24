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
  ggplot(scenario_run_vaccination_isolation,
         aes(x = isolation_stringency,
             y = weighted_tp_mutliplier_popn)) + 
    geom_col() +
    facet_wrap( ~ vaccination_coverage,
                labeller = label_glue(
                  "vaccination coverage = {vaccination_coverage}"
                  ),
                nrow = 1
                ) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Dark2")

}


