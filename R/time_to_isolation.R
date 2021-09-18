#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n_chains
#' @param n_iterations
#' @param gi_meanlog
#' @param gi_sdlog
#' @param passive_distribution
#' @param max_prob_passive
#' @param r_start
#' @param sim_tracing_fun
#' @return
#' @author Nicholas Tierney
#' @export
time_to_isolation <- function(n_chains,
                              n_iterations,
                              gi_meanlog,
                              gi_sdlog,
                              p_active_detection,
                              p_passive_detection,
                              passive_distribution,
                              sim_tracing_fun) {
  # use a Gibbs Sampler to convert distributions of time from:
      # "infector isolation" to "infectee isolation" into 
      # distributions of time from "infection" to "isolation" of cases
      # and therefore reduction in transmission potential
  ## Case 1 is infected at time 0 and isolated at time "infect_isolate"
  ## Case 2 is infected at time "infect_infect", drawn from a lognormal
  # representing the naive generation interval distribution, truncated at 
  # the date of Case 1 isolation.
  # Case 2 is isolated at time: inf_isol = inf_isol + isol_isol - inf_inf
  ## Case 2 is isolated at time: 
  ## infect_isolate = infect_isolate + isolate_isolate - infect_infect
  
  # times from infection to isolation for the initial cases in each chain
  # initial values
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
    # contact, based on contact tracing alone
    # (independent of the other random variables)
    t_isolate_case_isolate_contact <- sim_tracing_fun(n_chains)
    
    # compute the infection to isolation for the (infected) contact, *if* they
    # were found by contact tracing and not passive detection, based on:
      # infection to isolation for the case
      # generation interval, and
      # contact tracing time
    t_infect_isolate_ct_only <- 
      t_infect_isolate + t_isolate_case_isolate_contact - t_infect_infect
    
    # compute infection to isolation for the contact *if* they
    # were found by passive detection and not contact tracing
    t_infect_isolate_passive_only <- generate(passive_distribution, n_chains)[[1]]
    
    # assuming they are found by either active, passive, or both, sample which
    # would find them
    found_by <- r_if_either(n_chains,
                            p_active_detection,
                            p_passive_detection)
    
    # get the earliest of the delays for each
    time_to_active = ifelse(found_by$a, t_infect_isolate_ct_only, Inf)
    time_to_passive = ifelse(found_by$b, t_infect_isolate_passive_only, Inf)
    t_infect_isolate <- pmin(time_to_active, time_to_passive)
    
    # the case where they are not found either during the detectable period by
    # passive detection, or due to contact tracing from source (ie. ignoring
    # that they could be found later by backtracing) is handled in computing the
    # TP reduction
    
    # record the time from infection to isolation in this step
    trace_object[, iteration] <- t_infect_isolate
  }
  
  trace_object
}

