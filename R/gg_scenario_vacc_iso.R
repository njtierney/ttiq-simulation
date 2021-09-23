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

  # some kind of an onion plot
  ggplot(scenario_run_vaccination_isolation,
         aes(size = end_tpp_multiplier)) + 
    geom_point() + 
    facet_grid(isolation_stringency ~ vaccination_coverage)

}
