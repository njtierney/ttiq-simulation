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
    mutate(
      time_to_isolation_sims = pmap(
        .l = list(
          n_iterations = n_iterations,
          gi_meanlog = gi_meanlog,
          gi_sdlog = gi_sdlog,
          p_active_detection = p_active_detection,
          p_passive_detection = p_passive_detection,
          passive_distribution = passive_distribution,
          samples = samples
        ),
        .f = time_to_isolation
      )
    ) %>% 
    # unpack these simulations into their own list columns
    unnest(cols = time_to_isolation_sims) %>% 
    mutate(time_to_isolation_sims = map(time_to_isolation_sims, c)) %>% 
    mutate(names = names(time_to_isolation_sims)) %>% 
    pivot_wider(
      names_from = names,
      values_from = time_to_isolation_sims
    ) 
  
}
