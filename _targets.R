## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  
  tar_file(cases_nsw_path, 
           here("data/CASES_FROM_20200701_0000_TO_20210913_1115.xlsx")),
  
  cases_nsw = read_cases_nsw(cases_nsw_path),
  
  cases_nsw_delays = cases_nsw_add_delays(cases_nsw),
  
  cases_nsw_delays_long = cases_nsw_longer(cases_nsw_delays),
  
  plot_cases_nsw_delays = gg_cases_nsw_delays(cases_nsw_delays_long),
  
  derive_nsw_delay_distributions = derive_distributions(cases_nsw_delays),
  
  nsw_delay_samples = generate_delay_samples(derive_nsw_delay_distributions),
  
  nsw_delay_samples_against_data = add_data_to_delay_samples(nsw_delay_samples,
                                                             cases_nsw_delays),
  
  plot_nsw_delay_samples_against_data = gg_nsw_delay_samples_against_data(
    nsw_delay_samples_against_data
  ),
  
  # plot these against the data
  # also plot them as an ecdf
  # we wanted to know the "all" (total delay time) distribution looks the same 
  # as the expected all, which is the sum of 1 and 2
  # then plug these into the ttiq running step
  
  
  optimal_delay = dist_normal_truncated(mu = 0.5, sigma = 0.25, lower = 0),
  current_delay = dist_normal_truncated(mu = 2.5, sigma = 1.5, lower = 0),
  zero_delays = dist_uniform(min = 0, max = 0),
  
  current_case_initiated_delay = dist_mixture(current_delay, 
                                              zero_delays,
                                              weights = c(0.9, 0.1)),
  scenario_df = create_scenario_df(
    
    # these terms are fixed for each simulation
    n_iterations = 1000,
    n_chains = 50,
    # parameters for sim_tracing
    sim_tracing_funs = c(optimal_delay,
                         current_delay,
                         current_case_initiated_delay)
  ),
  
  scenario_df_run = run_ttiq_scenario(
    scenario_df
  ),
  
  # analyse NSW data to get distributions of these delays (blue + yellow graphs)
  
  # 1. Swab
  # 2. Notification
  # 3. Interview
  
  # (assuming the infector isolates on date of swab)
  # (assuming infectees isolate on date of interview)
  
  # difference of 1-2 is test turnaround time
  # simulate random draw from this distribution
  # difference of 2-3 is time to interview ()
  # simulate random draw from this distribution
  
  # sum these random draws together
  # this gives you the full contact tracing delay
  # this ^^ returns what we currently have in the sim_tracing function
  
  ## optimal is this time period from July 2020 to Feb 2021
  
  ## Current is from the last month
  
  ## current + case initiated
  # same as current, but we randomly set the notification to interview to 0
  
  # difference of 1-3 is swab to interview (full contact tracing delay)
  
  
  # histogram of times to isolation from simulations
  scenario_df_run_plots = add_gg_hist_tti(scenario_df_run),
  
  tar_render(explore, "doc/explore.Rmd")
  
)

    # change this so we can provide our own distribution
    
    ## optimal
    ## current
    ## current + case initiated contract tracing
      # some fraction of these the fraction of them is 0
      # mixture of current + fraction where time = 0
    
    ## idependent assumption
    
   
    # delay from speciman collection to notification - this is test turaround time
    # time from notification to interview
    
    # earliest confirmed or probable (notification date) - pretend this is 
    # interviewed date (date of interview)
    # difference of these is delay to interview - THIS IS sim tracing delay
    # time to interview
    
    # test turnaround time IS:
      # difference of swab date and earliest confirmed or probable
    
    # speciman collection date to earliest confirmed or probably
    
    # bootstrap sample from observed
