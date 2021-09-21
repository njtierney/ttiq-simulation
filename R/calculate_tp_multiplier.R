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
    mutate(tp_multiplier_if_found = pmap_dbl(
      .l = list(
        inf_isol = time_to_isolation_sims,
        meanlog = gi_meanlog,
        sdlog = gi_sdlog
      ),
      .f = tp_reduction
    )) %>%
    # adjust tp multiplier for the probability of missing a case entirely
    mutate(
      p_missed = (1 - p_active_detection) * (1 - p_passive_detection),
      tp_multiplier = tp_multiplier_if_found * (1 - p_missed) + 1 * p_missed
    ) %>%
    relocate(
      tp_multiplier, 
      tp_multiplier_if_found,
      .before = everything()
    )
  
}

