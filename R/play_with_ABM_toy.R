# run Nick’s ABM
# key scripts
# setup_abm.R
# R and vaccination coverage is specified in
# get_valid_ABM_sample
# simulation criteria are specified
future::plan(multisession(workers = 8))
# future::plan(transparent)
sims <- expand_grid(
  vaccination_coverage = c(0.7, 0.8, 0.9),
  vaccination_test_seeking_multiplier = c(1, 0),
  passive_detection_given_symptoms = c(0.5),
#  R = 2,
  do_ttiq = c(TRUE)
) %>%
  mutate(
    # tweak starting R to get optimal reproduction number at about 1
    R = case_when(
      vaccination_coverage == 0.9 ~ 2.58, # fails <2.58
      vaccination_coverage == 0.8 ~ 2, # fails < 1.9
      vaccination_coverage == 0.7 ~ 1.99, # fails < 1.55
    )
  ) %>%
  rowwise() %>%
  mutate(
    parameters = list(
      setup_abm(
        R = R,
        vaccination_coverage = vaccination_coverage,
        vaccination_test_seeking_multiplier = vaccination_test_seeking_multiplier,
        passive_detection_given_symptoms = passive_detection_given_symptoms,
        contact_tracing = do_ttiq,
        screening = do_ttiq
      )
    )
  ) %>%
  mutate(
    simulations = list(
      get_valid_abm_samples(parameters, n_samples = 10)
    )
  )


# format the sims
sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics
  ) %>%
  # filter(
  #   statistic == "mean"
  # ) %>%
  # select(
  #   -statistic
  # ) %>%
  pivot_wider(
    names_from = vaccination_test_seeking_multiplier,
    values_from = value,
    names_prefix = "test_vacc_sympt_"
  ) %>%
  mutate(
    across(
      starts_with(
        "test_vacc_sympt"
      ),
      .fns = list(ratio = ~.x / test_vacc_sympt_1)
    )
  ) %>%
  arrange(
    statistic, metric, passive_detection_given_symptoms, vaccination_coverage
  ) %>%
  print(n = Inf)

plot_df <- sims %>%
  filter(
    vaccination_coverage == 0.8,
    #vaccination_test_seeking_multiplier == 0,
    passive_detection_given_symptoms == 0.5
  ) %>%
  select(-parameters) %>%
  unnest(simulations) %>%
  filter(simulation %in%
           paste0("sim_", 1:6)) #%>%

  ggplot(plot_df,
    aes(
      x = infection_day, 
      fill = factor(vaccination_test_seeking_multiplier)
    )
  ) +
  geom_histogram(
    binwidth = 1, 
    alpha = 0.5, 
    position = "identity"
   # colour = "white"
  ) +
  facet_wrap(~simulation) +
  ggtitle(
    paste0("R = ", plot_df$R,
                 " Vacc. cov. = ",
                 plot_df$vaccination_coverage)
          ) +
    labs(fill = "Test seeking\nmultiplier") +
  theme_minimal()


metrics_symp_vax <- sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics_symp_vax(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics)

