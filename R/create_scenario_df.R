#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param trace_object
#' @param gi_meanlog
#' @param gi_sdlog
#' @param r_start
#' @return
#' @author Nicholas Tierney
#' @export
create_scenario_df <- function(n_iterations,
                               n_chains,
                               sim_tracing_funs,
                               passive_distribution,
                               max_prob_passive) {
  # parameters of naive (untruncated) generation interval / infectiousness
  # profile
  sim_tracing_funs %>%
    mutate(
      n_iterations = n_iterations,
      n_chains = n_chains,
      gi_meanlog = 1.375738,
      gi_sdlog = 0.5665299,
      passive_distribution = passive_distribution,
      max_prob_passive = max_prob_passive,
      r_start = 7.82
    )
  
}
