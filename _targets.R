## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  # use a Gibbs Sampler to convert distributions of time from infector isolation
  # to infectee isolation into distributions of time from infection to isolation
  # of cases, and therefore reduction in transmission potential
  
  # Case 1 is infected at time 0 and isolated at time inf_isol
  # Case 2 is infected at time inf_inf, drawn from a lognormal
  # representing the naive generation interval distribution, truncated at the date
  # of Case 1 isolation.
  # Case 2 is isolated at time: inf_isol = inf_isol + isol_isol - inf_inf
  
  # parameters of naive (untruncated) generation interval / infectiousness profile
  gi_meanlog = 1.375738,
  gi_sdlog = 0.5665299,
  
  # initial time from infection to isolation
  n_iterations = 1000,
  n_chains = 50,
  # setup an object which is the number of chains and number of iterations
  trace_object = matrix(NA, nrow = n_chains, ncol = n_iterations),
  trace_run = time_to_isolation(trace_object = trace_object,
                                meanlog = gi_meanlog,
                                sdlog = gi_sdlog,
                                sim_tracing_mu = 1.9,
                                sim_tracing_sigma = 2),
  
  # histogram of times to isolation from simulations
  hist_time_to_isolation = gg_hist_tti(trace_run),
  
  # compute the multiplier on TP for this distribution
  tp_multiplier = tp_reduction(
    inf_isol = trace_run, 
    meanlog = gi_meanlog,
    sdlog = gi_sdlog
      ),
  
  # given a starting R, get the R after contact tracing
  r_start = 7.82,
  r_after_contact_tracing = r_start * tp_multiplier,
  
  tar_render(explore, "doc/explore.Rmd")
  

)
