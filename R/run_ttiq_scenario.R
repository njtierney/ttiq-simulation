#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df
#' @return
#' @author Nicholas Tierney
#' @export
run_ttiq_scenario <- function(scenario_df) {
  
  scenario_df %>% 
    mutate(time_to_isolation_sims = pmap(
      .l = list(
        n_chains = n_chains,
        n_iterations = n_iterations,
        gi_meanlog = gi_meanlog,
        gi_sdlog = gi_sdlog,
        sim_tracing_fun = sim_tracing_fun
      ),
      .f = time_to_isolation
    ),
    tp_multiplier = pmap_dbl(
      .l = list(
        inf_isol = time_to_isolation_sims,
        meanlog = gi_meanlog,
        sdlog = gi_sdlog
      ),
      .f = tp_reduction
    ),
  # given a starting R, get the R after contact tracing
    r_after_contact_tracing = r_start * tp_multiplier
    ) %>% 
    relocate(
      r_after_contact_tracing,
      tp_multiplier,
      .before = gi_meanlog
    )

}
