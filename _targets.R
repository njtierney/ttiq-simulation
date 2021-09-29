## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  
  tar_file(cases_nsw_path, "data/CASES_FROM_20200701_0000_TO_20210913_1115.xlsx"),
  
  cases_nsw = read_cases_nsw(cases_nsw_path),
  cases_vic = read_cases_vic(),
  
  cases_nsw_delays = case_add_delays(
    cases = cases_nsw,
    swab_date_var = earliest_detected,
    interview_date_var = interviewed_date,
    notification_date_var = earliest_confirmed_or_probable
  ),
  cases_vic_delays = case_add_delays(
    cases = cases_vic,
    swab_date_var = earliest_detected,
    interview_date_var = interviewed_date,
    notification_date_var = earliest_confirmed_or_probable
  ),
  
  cases_nsw_raw_delay_long = cases_nsw_delay_raw_longer(cases_nsw_delays),
  cases_vic_raw_delay_long = cases_nsw_delay_raw_longer(cases_vic_delays),
  
  plot_cases_nsw_raw_delay_long = gg_cases_nsw_delays_raw(cases_nsw_raw_delay_long),
  plot_cases_vic_raw_delay_long = gg_cases_nsw_delays_raw(cases_vic_raw_delay_long),
  
  cases_nsw_interview_missings = gg_interview_missings(cases_nsw_delays),
  cases_vic_interview_missings = gg_interview_missings(cases_vic_delays),
  
  are_nsw_cases_independent = check_cases_independence(cases_nsw_delays),
  are_vic_cases_independent = check_cases_independence(cases_vic_delays),
  
  cases_nsw_delays_long = cases_nsw_longer(cases_nsw_delays),
  cases_vic_delays_long = cases_nsw_longer(cases_vic_delays),
  
  plot_cases_nsw_delays = gg_cases_nsw_delays(cases_nsw_delays_long),
  plot_cases_vic_delays = gg_cases_nsw_delays(cases_vic_delays_long),

  cases_scenario = bind_rows(
    optimal = keep_dates_between(cases_nsw_delays,
                                 lower_date = "2020-07-01",
                                 upper_date = "2021-02-01"),
    current_nsw = keep_dates_between(cases_nsw_delays,
                                     lower_date = "2021-08-01",
                                     upper_date = "2021-09-15"),
    partial = keep_dates_between(cases_vic_delays,
                                 lower_date = "2020-08-01",
                                 upper_date = "2020-08-07"),
    current_vic = keep_dates_between(cases_vic_delays,
                                     lower_date = "2021-08-01",
                                     upper_date = "2021-09-15"),
    .id = "scenario"
  ),
  
  derived_delay_distributions = derive_distributions(
    cases_scenario,
    prop_current_case_zero = 0.8
  ),
  
  derived_delay_distributions_df = dist_params_to_df(derived_delay_distributions),
  
  tar_file(derived_delay_distributions_csv, {
    write_csv_return_path(derived_delay_distributions_df, here("outputs/derived_delay_distributions.csv"))
  }),
  
  delay_dist_funs = create_dist_sim_fun(derived_delay_distributions),
  
  delay_samples = generate_delay_samples(derived_delay_distributions,
                                             n_samples = 100000),
  
  delay_samples_against_data = add_data_to_delay_samples(delay_samples,
                                                         cases_scenario),
  
  prepared_cases_for_plots = prepare_case_samples_for_plots(
    delay_samples_against_data
    ),
  
  plot_hist_delay_samples_v_data = gg_hist_delay_samples_against_data(
    prepared_cases_for_plots
  ),
  
  tar_file(plot_hist_delay_samples_v_data_path, {
    ggsave_write_path(
      plot = plot_hist_delay_samples_v_data,
      path = "figs/hist_delay_samples_v_data.png",
      width = 6,
      height = 6
    )
  }),

  p_active_detection = 0.9,
  p_passive_detection = 0.3,
  
  scenario_df = create_scenario_df(
    # these terms are fixed for each simulation
    n_iterations = 1000,
    n_chains = 50,
    # parameters for sim_tracing
    sim_tracing_funs = delay_dist_funs,
    # the probability of ever being found via contact tracing if not by passive
    # detection
    p_active_detection = p_active_detection,
    # the probability of being found via passive detection (based on symptoms)
    # if not by contact tracing
    p_passive_detection = p_passive_detection,
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
  
  plot_tp_reduction = gg_tp_reduction(scenario_df_run_tp_multiplier),
  
  tar_file(plot_tp_reduction_path, {
    ggsave_write_path(
      plot = plot_tp_reduction,
      path = "figs/nsw_ttiq_model_hist.png",
      width = 9,
      height = 3.5
    )
  }),
  
  oz_baseline_matrix = get_oz_baseline_matrix(),
  
  scenario_vaccination_isolation = create_scenario_vaccination_isolation(
    vaccination_multiplier = 0.3,
    p_passive_detection_vaccinated = 0.50 * p_passive_detection,
    p_active_detection = p_active_detection,
    p_passive_detection = p_passive_detection,
    # baseline - if we treated vaccinated people the same as unvaccinated ppl
    tp_multiplier = 0.46,
    isolation_stringency = seq(0, 1, by = 0.2),
    vaccination_coverage = seq(0.7, 0.9, by = 0.1)
  ), 
  
  scenario_run_vaccination_isolation = run_ttiq_vaccination_isolation(
    scenario_vaccination_isolation,
    oz_baseline_matrix
  ),
  
  plot_scenario_vaccination_isolation = gg_scenario_vacc_iso(
    scenario_run_vaccination_isolation
  ),
  
  nsw_delays = read_nsw_delays(cases_nsw_path),
  
  plot_nsw_delays_optimal = gg_nsw_delays_hist(nsw_delays),
  
  tar_file(delay_from_onset_cdfs_path,
           "data/delay_from_onset_cdfs.RDS"),
  
  tar_file(surveillance_matrix_path,
           "data/surveillance_matrix.RDS"),
  
  surveillance_cdfs = read_rds(delay_from_onset_cdfs_path),
  
  surveillance = read_rds(surveillance_matrix_path),
  
  isolation_cdfs = create_isolation_cdfs(
    nsw_delays,
    surveillance_cdfs,
    surveillance
  ),
  
  tti_distributions = create_tti_distributions(isolation_cdfs),

  plot_tti_ecdf_comparison = gg_tti_ecdf_comparison(tti_distributions),
  
  tar_file(plot_ecdf_path, {
    ggsave_write_path(
      plot = plot_tti_ecdf_comparison,
      path = "figs/nsw_ttiq_step.png",
    )
  }),
  
  tp_reductions = calculate_tp_reductions(tti_distributions),
  
  plot_hist_tp_reductions = gg_hist_tp_reductions(tp_reductions,
                                                  tti_distributions),
  
  tar_file(plot_hist_tp_path, {
    ggsave_write_path(
      plot = plot_hist_tp_reductions,
      path = "figs/nsw_ttiq_hist.png",
      width = 9,
      height = 3.5
      )
  }),
  
  ## @logan - code for writing parameters
  # tar_file(dist_parameters_csv_path, {
  #   write_csv_return_path(
  #     x = dist_parameters_csv,
  #     path = "output-public/dist_parameters.csv"
  #   )
  # }),
  
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
  
  tar_render(explore, "doc/explore_vic.Rmd")
  
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
