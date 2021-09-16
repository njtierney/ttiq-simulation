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
                               sim_tracing_funs) {
  # parameters of naive (untruncated) generation interval / infectiousness
  # profile
  tibble(
    n_iterations = n_iterations,
    n_chains = n_chains,
    scenario = c("optimal", "current"),
    gi_meanlog = 1.375738,
    gi_sdlog = 0.5665299,
    r_start = 7.82,
  ) %>% 
    left_join(
      sim_tracing_funs,
      by = "scenario"
    )
  
}
