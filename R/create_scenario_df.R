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
create_scenario_df <- function(gi_meanlog,
                               gi_sdlog,
                               r_start,
                               mu,
                               sigma,
                               n_iterations,
                               n_chains) {
  
  expand_grid(
    gi_meanlog = gi_meanlog,
    gi_sdlog = gi_sdlog,
    r_start = r_start,
    mu = mu,
    sigma = sigma
  ) %>% 
    mutate(
      sim_tracing_fun = map2(.x = mu, 
                             .y = sigma,
                             .f = ~build_sim_tracing_default(
                               mu = .x,
                               sigma = .y
                             )),
      n_iterations = n_iterations,
      n_chains = n_chains
    )
  

}
