#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run_tp_multiplier
#' @return
#' @author Nicholas Tierney
#' @export
prepare_ttiq_for_csv <- function(scenario_df_run_tp_multiplier) {

  scenario_df_run_tp_multiplier %>% 
    select(-sim_tracing_fun, 
           -passive_distribution) %>% 
    relocate(scenario) %>% 
    unnest(
      cols = c(time_to_isolation_sims,
               time_to_active,
               time_to_passive)
    ) %>% 
    relocate(p_missed, 
             .after = r_start)

}
