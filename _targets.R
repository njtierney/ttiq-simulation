## Loads all packages and defines how to handle NAMESPACE conflicts
source("./packages.R")

## Load all R files in R/ folder
lapply(list.files("./R", full.names = TRUE), source)
pkg_list <- extract_pkg_names("packages.R")
tar_option_set(
  packages = c("conmat"), 
  imports = c("conmat")
)

# debug(coverage_milestones)
tar_plan(
  
  # handle data ingress for delay distributions
  user = case_when(
    Sys.info()["nodename"] == "6300L-148079-M.local" ~ "Nick G",
    TRUE ~ "Sensible people"  
  ),
  
  data_path = case_when(
    user == "Nick G" ~ "~/not_synced",
    TRUE ~ "data"
  ),
  
  tar_file(
    cases_nsw_path, 
    file.path(
      data_path,
      "CASES_FROM_20200701_0000_TO_20210913_1115.xlsx"
    )
  ),
  
  tar_file(
    cases_vic_path, 
    file.path(
      data_path,
      "Linelist_Cases_20210917.xlsx"
    )
  ),
  
  cases_nsw = read_cases_nsw(cases_nsw_path),
  cases_vic = read_cases_vic(cases_vic_path),
  
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
  
  scenario_parameters = create_scenario_parameters(),
  
  derived_delay_distributions = derive_distributions(
    cases_scenario,
    # was previously just only 0.8 - now is 0.2, 0.4, 0.6, 0.8
    prop_current_case_zero = seq(from = 0.2, to = 0.8, by = 0.2)
  ),
  
  
  # output delay distributions for other analyses
  
  # independently sample from component delay distributions, for Eamon
  
  # get the probabilities of every number of days for each delay
  optimal_delay_samples = sample_optimal_delays(
    derived_delay_distributions
  ),
  
  tar_file(
    optimal_delay_samples_path, {
      write_csv_return_path(
        x = optimal_delay_samples,
        file = "outputs/optimal_delay_samples.csv"
      )
    }
  ),
  
  delay_dist_funs = create_dist_sim_fun(derived_delay_distributions),
  
  scenario_test_turnaround_time_sims = simulate_test_turnaround_time(
    derived_delay_distributions = derived_delay_distributions,
    n_samples = 10000
    ),
  
  scenario_test_turnaround_time_probs = parameters_test_turnaround_time(
    derived_delay_distributions
  ),
  
  tar_file(scenario_test_turnaround_time_probs_path,{
    write_csv_return_path(scenario_test_turnaround_time_probs,
                          "outputs/scenario_test_turnaround_time_probs.csv")
  }),
  
  tar_file(scenario_test_turnaround_time_sims_path,{
    write_csv_return_path(scenario_test_turnaround_time_sims,
                          "outputs/scenario_test_turnaround_time_sims.csv")
  }),
  
  
  # plot delay distributions
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
      width = 8,
      height = 8
    )
  }),
  
  plot_hist_delay_samples_v_data_original_scenarios = 
    gg_hist_delay_samples_against_data(
      filter(prepared_cases_for_plots,
             scenario %in% c("current_nsw",
                               "optimal",
                               "current_nsw_case_init_0.8"))
    ),
  
  tar_file(plot_hist_delay_samples_v_data_original_scenarios_path, {
    ggsave_write_path(
      plot = plot_hist_delay_samples_v_data_original_scenarios,
      path = "figs/plot_hist_delay_samples_v_data_original_scenarios.png",
      width = 8,
      height = 8
    )
  }),
  
  # set some global parameters for simulations
  
  p_active_detection = 0.95,
  passive_detection_given_symptoms = 0.5,
  
  # (this will change as a function of vaccination coverage)
  pr_symptoms = 0.6,
  p_passive_detection = passive_detection_given_symptoms * pr_symptoms,
  
  samples_df =  generate_samples_df_delays(delay_dist_funs,
                                           n_samples = 10000),
  
  
  # run scenarios on the impact of contact tracing delays on TP
  scenario_df = create_scenario_df(
    # these terms are fixed for each simulation
    n_iterations = 10000,
    # parameters for sim_tracing
    # samples = samples_df,
    sim_tracing_funs = samples_df,
    # the probability of ever being found via contact tracing if not by passive
    # detection
    p_active_detection = p_active_detection,
    passive_detection_given_symptoms = passive_detection_given_symptoms,
    pr_symptoms = pr_symptoms,
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
  
  plot_simple_tp = gg_simple_tp(scenario_df_run_tp_multiplier),
  
  ttiq_scenario_prepared = prepare_ttiq_for_csv(scenario_df_run_tp_multiplier),
  
  tar_file(scenario_df_run_tp_multiplier_csv,{
    write_csv_return_path(
      x = ttiq_scenario_prepared,
      file = "outputs/ttiq_scenario_run.csv"
    )
  }),
  
  plot_tp_reduction = gg_tp_reduction(scenario_df_run_tp_multiplier,
                                      scenario_parameters),
  
  plot_tp_reduction_over_prop_zeros = gg_tp_reduction_prop_zeros(
    scenario_df_run_tp_multiplier,
    scenario_parameters
  ),
  
  tar_file(plot_tp_reduction_path, {
    ggsave_write_path(
      plot = plot_tp_reduction,
      path = "figs/nsw_ttiq_model_hist.png",
      width = 9,
      height = 6
    )
  }),
  
  tar_file(plot_tp_reduction_zeros_path, {
    ggsave_write_path(
      plot = plot_tp_reduction_over_prop_zeros,
      path = "figs/ttiq_model_hist_zeros.png",
      width = 10,
      height = 10
    )
  }),
  
  
  # simulate prioritisation strategies
  queue_scenarios = run_queue_scenarios(derived_delay_distributions,
                                          n_samples = 100),
  
  plot_queue_scenarios = gg_queue_scenarios(queue_scenarios),
  
  samples_df_queue = tidy_queue_scenario(queue_scenarios),
  
  # pull out the tidied queueing delays corresponding to specified scenarios
  # split that into two tibbles, one for vaccination status = TRUE, one for vaccination status = FALSE
  queue_splits = split_queue_scenarios_by_vaccination(
    samples_df_queue
    ),
  
  # save those as two separate files (vaccinated/unvaccinated) for each scenario
  tar_file(
    queue_splits_path,{
      write_csv_queue_splits(
        queue_splits = queue_splits,
        file_paths = glue("outputs/{names(queue_splits)}.csv")
      )
    }
  ),
  
  scenario_df_queue = create_scenario_df(
    # these terms are fixed for each simulation
    n_iterations = 1000,
    # parameters for sim_tracing
    # samples = samples_df,
    sim_tracing_funs = samples_df_queue,
    # the probability of ever being found via contact tracing if not by passive
    # detection
    p_active_detection = p_active_detection,
    passive_detection_given_symptoms = passive_detection_given_symptoms,
    pr_symptoms = pr_symptoms,
    # the probability of being found via passive detection (based on symptoms)
    # if not by contact tracing
    p_passive_detection = p_passive_detection,
    # if found by passive case detection (assuming contact tracing not in
    # place), the distribution of times from infection to detection
    passive_distribution = list(get_passive_distribution())
  ),
  
  scenario_df_run_queue = run_ttiq_scenario(
    scenario_df_queue
  ),
  
  scenario_df_run_tp_multiplier_queue = calculate_tp_multiplier(
    scenario_df_run_queue
  ),
  
  
  plot_simple_tp_queue = gg_simple_tp(scenario_df_run_tp_multiplier_queue),
  

  
  
  
  
  # next generation matrix for australia, for static analysis
  oz_baseline_matrix = get_oz_baseline_matrix(),
  
  
  
  # What is the age- and vaccine adjusted clinical fraction of cases
  prepared_infections_vax_symp = prepare_infections_vax_symp(
    oz_baseline_matrix,
    average_vaccine_efficacy,
    vaccination_coverage_milestones
  ),
  
  mini_abm_parameters = get_mini_abm_parameters(
    prepared_infections_vax_symp,
    oz_baseline_matrix,
    vaccination_coverage_age_group_at_milestone,
    populations
  ),
  
  tar_file(mini_abm_parameters_path, {
    saveRDS_write_path(
      object = mini_abm_parameters,
      path = "outputs/mini_abm_parameters.RDS",
    )
  }),
  
  plot_infections_vax_symp = gg_infections_vax_symp(
    prepared_infections_vax_symp
  ),
  
  tar_file(plot_infections_vax_symp_path,{
    ggsave_write_path(
      plot = plot_infections_vax_symp,
      path = "figs/infections_to_cases_coverage.png",
      width = 9,
      height = 4,
      dpi = 600
    )
  }),
    
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
                                                  tti_distributions,
                                                  scenario_parameters),
  
  tar_file(plot_hist_tp_path, {
    ggsave_write_path(
      plot = plot_hist_tp_reductions,
      path = "figs/nsw_ttiq_hist.png",
      width = 9,
      height = 3.5
    )
  }),
  
  # histogram of times to isolation from simulations
  scenario_df_run_plots = add_gg_hist_tti(scenario_df_run),

  # plot some example epi curves  
  epi_curve_example_sims = sim_epi_curve_examples(),
  
  plot_epi_curve_example = gg_epi_curve_example(epi_curve_example_sims),
  
  tar_file(plot_epi_curve_example_path, {
    ggsave_write_path(
      plot = plot_epi_curve_example,
      path = "figs/epi_curve_example.png",
      width = 8,
      height = 6
    )
  }),
  
  
  
  # vaccination rollout formatting
  
  tar_file(dim_age_band_path,
           "data/rollout/dim_age_band.csv"),
  
  tar_file(sa2_lookup_path,
           "data/rollout/dim_sa2.csv"),
  
  tar_file(sa4_lookup_path,
           "data/rollout/dim_sa4.csv"),
  
  tar_file(dim_time_path,
           "data/rollout/dim_time.csv"),
  
  tar_file(dim_vaccine_path,
           "data/rollout/dim_vaccine.csv"),
  
  tar_file(populations_path,
           "data/rollout/populations_sa2.csv"),
  
  tar_file(vaccinations_path,
           "data/rollout/vaccinations.csv"),
  
  dim_age_band = read_csv(dim_age_band_path),
  sa2_lookup = read_csv(sa2_lookup_path),
  sa4_lookup = read_csv(sa4_lookup_path),
  dim_time = read_csv(dim_time_path),
  dim_vaccine = read_csv(dim_vaccine_path),
  populations_raw = read_csv(populations_path),
  vaccinations_raw = read_csv(vaccinations_path),
  
  populations = tidy_populations(populations_raw),
   
  aggregated_populations = aggregate_populations_to_vaccinations_age_band(
    populations
  ),
  
  vaccinations = tidy_vaccinations(vaccinations_raw,
                                   dim_age_band,
                                   dim_time,
                                   dim_vaccine,
                                   aggregated_populations,
                                   sa4_lookup),
  
  vaccination_age_band = create_vaccination_age_band(vaccinations),
  
  vaccination_coverage = create_vaccination_coverage(vaccinations),
  
  plot_vaccination_coverage = gg_vaccination_coverage(vaccination_coverage),
  
  vaccination_coverage_milestones = coverage_milestones(vaccination_coverage),
  
  tar_file(vaccination_coverage_milestones_path,{
    write_csv_return_path(vaccination_coverage_milestones,
                          "outputs/vaccination_coverage_milestones.csv")
  }),
  
  vaccination_coverage_age_group_at_milestone = 
    age_group_coverage_at_milestones(
      vaccinations,
      vaccination_coverage_milestones
    ),
  
  vaccination_coverage_age_group_at_milestone_5_year = aggregate_5_years(
    vaccination_coverage_age_group_at_milestone,
    populations
  ),
  
  tar_file(vaccination_coverage_age_group_at_milestone_5_year_path,{
    write_csv_return_path(vaccination_coverage_age_group_at_milestone_5_year,
                          "outputs/vaccination_coverage_age_group_at_milestone_5_year.csv")
  }),
  
  average_vaccine_efficacy = 
    create_average_vaccine_efficacy(vaccination_coverage_age_group_at_milestone_5_year),
  
  
  # output some things for Eamon
  eamon_terminal_coverage = get_eamon_terminal_coverage(vaccination_coverage_age_group_at_milestone),

  eamon_populations = get_eamon_populations(populations),
  
  tar_file(eamon_terminal_coverage_path,{
    write_csv_return_path(
      eamon_terminal_coverage,
      "outputs/eamon_terminal_coverage.csv"
    )
  }),
  
  tar_file(eamon_populations_path,{
    write_csv_return_path(
      eamon_populations,
      "outputs/eamon_populations.csv"
    )
  }),
  
  
  
  # a simple plot of TTIQ effect for cases, ascertainment, and TTIQ effect
  ttiq_ascertainment_plot = gg_ttiq_ascertainment(
    scenario_parameters
  ),
  
  tar_file(ttiq_ascertainment_plot_path, {
    ggsave_write_path(
      plot = ttiq_ascertainment_plot,
      path = "figs/ttiq_ascertainment.png",
      width = 6,
      height = 5
    )
  }),
  
  # a doc to view plots etc
  tar_render(explore, "doc/explore.Rmd", intermediates_dir="./"),
  
)
