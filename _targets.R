## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  
  tar_file(cases_nsw_path, 
           "~/not_synced/vaccination/nsw/CASES_FROM_20200701_0000_TO_20210913_1115.xlsx"),
   
  cases_nsw = read_cases_nsw(cases_nsw_path),
  
  cases_nsw_raw_delay_long = cases_nsw_delay_raw_longer(cases_nsw),
  
  plot_cases_nsw_raw_delay_long = gg_cases_nsw_delays_raw(cases_nsw_raw_delay_long),
  
  cases_nsw_delays = case_add_delays(
    cases = cases_nsw,
    swab_date_var = earliest_detected,
    interview_date_var = interviewed_date,
    notification_date_var = earliest_confirmed_or_probable
  ),
  
  cases_nsw_interview_missings = gg_interview_missings(cases_nsw_delays),
  
  are_cases_independent = check_cases_independence(cases_nsw_delays),
  
  cases_nsw_delays_long = cases_nsw_longer(cases_nsw_delays),
  
  plot_cases_nsw_delays = gg_cases_nsw_delays(cases_nsw_delays_long),
  
  cases_scenario = bind_rows(
    optimal = keep_dates_between(cases_nsw_delays,
                                 lower_date = "2020-07-01",
                                 upper_date = "2021-02-01"),
    current = keep_dates_between(cases_nsw_delays,
                                 lower_date = "2021-08-01",
                                 upper_date = "2021-09-15"),
    .id = "scenario"
  ),
  
  derived_delay_distributions = derive_distributions(
    cases_scenario,
    prop_current_case_zero = 0.8
    ),
  
  delay_dist_funs = create_dist_sim_fun(derived_delay_distributions),
  
  delay_samples = generate_delay_samples(derived_delay_distributions,
                                             n_samples = 100000),
  
  delay_samples_against_data = add_data_to_delay_samples(delay_samples,
                                                         cases_scenario),
  
  prepared_cases_for_plots = prepare_case_samples_for_plots(
    delay_samples_against_data
    ),
  
  plot_hist_nsw_delay_samples_v_data = gg_hist_nsw_delay_samples_against_data(
    prepared_cases_for_plots
  ),
  
  scenario_df = create_scenario_df(
    # these terms are fixed for each simulation
    n_iterations = 1000,
    n_chains = 50,
    # parameters for sim_tracing
    sim_tracing_funs = delay_dist_funs,
    # the probability of ever being found via contact tracing if not by passive
    # detection
    p_active_detection = 0.9,
    # the probability of being found via passive detection (based on symptoms)
    # if not by contact tracing
    p_passive_detection = 0.15,
    # if found by passive case detection (assuming contact tracing not in
    # place), the distribution of times from infection to detection
    passive_distribution = list(get_passive_distribution())
  ),
  
  scenario_df_run = run_ttiq_scenario(
    scenario_df
  ),
  
  scenario_df_run_tp_multiplier = calculate_tp_multiplier(
    scenario_df_run
  ),
  
  # REFACTORING UP TO HERE
  
  plot_nsw_tp_reduction = gg_nsw_tp_reduction(scenario_df_run_tp_multiplier),
  
  scenario_vaccination_isolation = create_scenario_vaccination_isolation(
    vaccination_multiplier = 0.3,
    p_passive_detection_vaccinated = 0.15,
    p_passive_detection = 0.15,
    p_active_detection = 0.85,
    # baseline - if we treated vaccinated people the same as unvaccinated ppl
    # isn't this estimated from the data?
    tp_multiplier = 0.46,
    tp = 7.82,
    isolation_stringency = seq(0, 1, by = 0.2),
    vaccination_coverage = seq(0.6, 0.9, by = 0.1),
    pr_vaccination_cases = 0.72,
  ), 
  
  scenario_run_vaccination_isolation = run_ttiq_vaccination_isolation(
    scenario_vaccination_isolation
  ),
  
  plot_scenario_vaccination_isolation = gg_scenario_vacc_iso(
    scenario_run_vaccination_isolation
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
