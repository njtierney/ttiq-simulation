#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n_iterations
#' @param n_chains
#' @param meanlog
#' @param sdlog
#' @return
#' @author Nicholas Tierney
#' @export
time_to_isolation <- function(trace_object,
                              meanlog,
                              sdlog,
                              sim_tracing_mu,
                              sim_tracing_sigma) {
  n_chains <- nrow(trace_object)
  n_iterations <- ncol(trace_object)
  # times from infection to isolation for the initial cases in each chain
  inf_isol <- abs(rnorm(n_chains, 14, 5))

  # Gibbs sample multiple Markov chains in parallel to obtain the distribution of
  # times from infection to isolation
  for (iteration in seq_len(n_iterations)) {
    # simulate a generation interval
    inf_inf <- sim_gi_truncated(
      n = n_chains,
      infection_to_isolation = inf_isol,
      meanlog = gi_meanlog,
      sdlog = gi_sdlog
    )
    # simulate the delay from isolation of case to isolation of (infected)
    # contact, which here is independent of the other random variables
    isol_isol <- sim_tracing(
      n = n_chains,
      mu = sim_tracing_mu,
      sigma = sim_tracing_sigma
    )
    # compute the infection to isolation for the (infected) contact, based on the
    # infection to isolation for the case, the generation interval, and the
    # contact tracing time
    inf_isol <- inf_isol + isol_isol - inf_inf
    # record the time from infection to isolation in this step
    trace_object[, iteration] <- inf_isol
  }
  
  trace_object
}
