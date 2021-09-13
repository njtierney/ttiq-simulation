## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  
  scenario_df = create_scenario_df(
    # parameters of naive (untruncated) generation interval / infectiousness
    # profile
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
  
  scenario_df_run = run_ttiq_scenario(
    scenario_df
  ),
  
  # histogram of times to isolation from simulations
  scenario_df_run_plots = add_gg_hist_tti(scenario_df_run),
  
  tar_render(explore, "doc/explore.Rmd")
  

)
