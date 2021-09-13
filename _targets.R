## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  
  scenario_df = create_scenario_df(
    # parameters of naive (untruncated) generation interval / infectiousness profile
    # what is this number in units? log(mean days generation interval) ?
    gi_meanlog = 1.375738, 
    gi_sdlog = 0.5665299, 
    r_start = 1:8,
    # parameters for sim_tracing
    # Is this also in terms of days?
    mu = 1.9,
    sigma = 2,
    # these terms are fixed for each simulation
    n_iterations = 1000,
    n_chains = 50
    ), 
  
  # use a Gibbs Sampler to convert distributions of time from:
    ## "infector isolation" to "infectee isolation" into 
    ## distributions of time from "infection" to "isolation" of cases
    ## and therefore reduction in transmission potential
  # Case 1 is infected at time 0 and isolated at time "inf_isol"
  # Case 2 is infected at time "inf_inf", drawn from a lognormal
  ## Case 1 is infected at time 0 and isolated at time "infect_isolate"
  ## Case 2 is infected at time "infect_infect", drawn from a lognormal
  # representing the naive generation interval distribution, truncated at 
  # the date of Case 1 isolation.
  # Case 2 is isolated at time: inf_isol = inf_isol + isol_isol - inf_inf
  ## Case 2 is isolated at time: 
    ## infect_isolate = infect_isolate + isolate_isolate - infect_infect
  
  scenario_df_run = run_ttiq_scenario(
    scenario_df
  ),
  
  scenario_df_run_plots = add_gg_hist_tti(scenario_df_run),
  
  # # histogram of times to isolation from simulations
  # hist_time_to_isolation = gg_hist_tti(trace_run),
  
  tar_render(explore, "doc/explore.Rmd")
  

)
