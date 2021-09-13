#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run
#' @return
#' @author Nicholas Tierney
#' @export
add_gg_hist_tti <- function(scenario_df_run) {

  scenario_df_run %>% 
    mutate(gg = future_map(time_to_isolation_sims, gg_hist_tti))

}
