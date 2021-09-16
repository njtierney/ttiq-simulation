#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run
#' @return
#' @author Nicholas Tierney
#' @export
calculate_tp_multiplier <- function(scenario_df_run) {
  scenario_df_run %>%
    mutate(tp_multiplier = pmap_dbl(
      .l = list(
        inf_isol = time_to_isolation_sims,
        meanlog = gi_meanlog,
        sdlog = gi_sdlog
      ),
      .f = tp_reduction
    )) %>%
    relocate(tp_multiplier,
             .before = everything())
  
}
