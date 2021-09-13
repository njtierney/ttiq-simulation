#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n_chains
#' @param n_iterations
#' @param gi_meanlog
#' @param gi_sdlog
#' @param r_start
#' @param sim_tracing_fun
#' @return
#' @author Nicholas Tierney
#' @export
time_to_isolation <- function(n_chains,
                                n_iterations,
                                gi_meanlog,
                                gi_sdlog,
                                sim_tracing_fun) {
  # times from infection to isolation for the initial cases in each chain
  t_infect_isolate <- abs(rnorm(n = n_chains, 
                                mean = 14, 
                                sd = 5))
  
  trace_object <- matrix(NA, nrow = n_chains, ncol = n_iterations)
  # Gibbs sample multiple Markov chains in parallel to obtain the distribution of
  # times from infection to isolation
  for (iteration in seq_len(n_iterations)) {
    # simulate a generation interval
    t_infect_infect <- sim_gi_truncated(
      n = n_chains,
      infection_to_isolation = t_infect_isolate,
      meanlog = gi_meanlog,
      sdlog = gi_sdlog
    )
    # simulate the delay from isolation of case to isolation of (infected)
    # contact, which here is independent of the other random variables
    t_isolate_case_isolate_contact <- sim_tracing_fun(n_chains) 
    # compute the infection to isolation for the (infected) contact, based on:
      # infection to isolation for the case
      # generation interval, and
      # contact tracing time
    t_infect_isolate <- 
      t_infect_isolate + t_isolate_case_isolate_contact - t_infect_infect
    # record the time from infection to isolation in this step
    trace_object[, iteration] <- t_infect_isolate
  }
  
  trace_object
}

