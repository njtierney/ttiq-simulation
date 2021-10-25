future::plan(multisession(workers = 8))
# future::plan(transparent)

sims <- expand_grid(
  vaccination_coverage = c(0.7, 0.8, 0.9),
  vaccination_test_seeking_multiplier = c(1, 0),
  passive_detection_given_symptoms = c(0.5, 0.8),
  R = 2,
  do_ttiq = c(TRUE)
) %>%
  # mutate(
  #   # tweak starting R to get optimal reproduction number at about 1
  #   R = case_when(
  #     vaccination_coverage == 0.9 ~ 4.5,
  #     vaccination_coverage == 0.8 ~ 3.5,
  #     vaccination_coverage == 0.7 ~ 2.7,
  #   )
  # ) %>%
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


sims %>%
  filter(
    vaccination_coverage == 0.9,
    vaccination_test_seeking_multiplier == 1,
    passive_detection_given_symptoms == 0.5
  ) %>%
  select(-parameters) %>%
  unnest(simulations) %>%
  filter(simulation %in% 
           paste0("sim_", 1:6)) %>%
  ggplot(
    aes(
      x = infection_day
    )
  ) +
  geom_histogram(
    binwidth = 1,
    colour = "white"
  ) +
  facet_wrap(~simulation) +
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
    cols = metrics
  )

tps <- metrics_symp_vax %>%
  mutate(
    vaccinated = ifelse(vaccinated == 1, "vax", "unvax"),
    symptomatic = ifelse(symptomatic == 1, "symp", "asymp"),
  ) %>%
  pivot_wider(
    names_from = c(symptomatic, vaccinated),
    values_from = value
  ) %>%
  filter(
    metric == "TP",
    statistic == "mean"
  ) %>%
  arrange(vaccination_test_seeking_multiplier, vaccination_coverage) %>%
  select(
    -metric,
    -statistic,
    -R,
    -do_ttiq
  ) %>%
  mutate(
    which = "TP"
  )
  
fractions <- sims %>%
  select(-parameters) %>%
  unnest(simulations) %>%
  group_by(
    vaccination_coverage,
    vaccination_test_seeking_multiplier,
    passive_detection_given_symptoms
  ) %>%
  summarise(
    symp_vax = mean(symptomatic & vaccinated),
    symp_unvax = mean(symptomatic & !vaccinated),
    asymp_vax = mean(!symptomatic & vaccinated),
    asymp_unvax = mean(!symptomatic & !vaccinated),
  ) %>%
  arrange(vaccination_test_seeking_multiplier, vaccination_coverage) %>%
  mutate(
    which = "fraction"
  )

bind_rows(
  fractions,
  tps
) %>%
  ungroup() %>%
  pivot_longer(
    cols = ends_with("vax"),
    names_to = "cohort",
    values_to = "number"
  ) %>%
  pivot_wider(
    names_from = which,
    values_from = number
  ) %>%
  mutate(
    foi = fraction * TP
  ) %>%
  pivot_longer(
    cols = c(fraction, TP, foi),
    names_to = "which",
    values_to = "number"
  ) %>%
  pivot_wider(
    names_from = cohort,
    values_from = number
  ) %>%
  arrange(
    vaccination_test_seeking_multiplier,
    passive_detection_given_symptoms,
    vaccination_coverage,
    which
  ) %>%
  mutate(
    totals = Reduce("+", across(ends_with("vax")))
  ) %>%
  print(n = Inf)
  



# 
# sims %>%
#   filter(
#     vaccination_coverage == 0.7,
#     vaccination_test_seeking_multiplier == 0
#   ) %>%
#   unnest(simulations) %>%
#   filter(
#     simulation %in%
#       paste0("sim_", 1:4)
#   ) %>%
#   group_by(
#     simulation,
#     infection_day,
#     vaccinated
#   ) %>%
#   summarise(
#     passive_detections = sum(case_found_by == "screening", na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     vaccinated = factor(vaccinated)
#   ) %>%
#   ggplot(
#     aes(
#       x = infection_day,
#       y = passive_detections,
#       fill = vaccinated
#     )
#   ) +
#   geom_col() +
#   facet_wrap(~simulation) +
#   theme_minimal()



abm_result <- sims %>%
  filter(
    vaccination_coverage == 0.7,
    vaccination_test_seeking_multiplier == 0
  ) %>%
  unnest(simulations)

compute_abm_metrics_symp_vax <- function(abm_result) {
  # analyse multiple ABM simulations and pull out relevant metrics
  
  # compute the numbers of onward transmissions for all sources
  transmissions <- abm_result %>%
    group_by(
      simulation,
      source_id
    ) %>%
    summarise(
      transmissions = n(),
      .groups = "drop"
    )
  
  # find source infections that we can trust onward infection for, and join on
  # their number of transmissions 
  sources <- abm_result %>%
    filter(
      # find sources to consider (exclude those during burn in and last two weeks
      # due to truncation of onward infection)
      !is.na(source_id) &
        infection_day > 14 &
        infection_day < (max(infection_day) - 14)
      # find infections to keep - the sources and those infected by these sources
    ) %>%
    select(
      -source_id
    ) %>%
    left_join(
      transmissions,
      by = c(id = "source_id", "simulation" )
    ) %>%
    mutate(
      transmissions = replace_na(transmissions, 0),
      found = !is.na(case_found_by),
      traced = found & case_found_by == "contact_tracing",
      screened = found & case_found_by == "screening",
      infection_to_isolation = isolation_day - infection_day
    )
  
  sources %>%
    group_by(
      simulation,
      vaccinated,
      symptomatic
    ) %>%
    summarise(
      TP = mean(transmissions),
      ascertainment = mean(found),
      tracing = mean(traced),
      screening = mean(screened),
    ) %>%
    group_by(
      vaccinated,
      symptomatic
    ) %>%
    summarise(
      across(
        c(TP, ascertainment, tracing, screening),
        .fns = list(
          mean = ~mean(.x),
          variance = ~var(.x)
        )
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(ends_with("mean"), ends_with("variance")),
      names_to = c("metric", "statistic"),
      values_to = "value",
      names_sep = "_"
    )
  
}


  # sources %>%
#   filter(
#     case_found_by == "contact_tracing"
#   ) %>%
#   ggplot(
#     aes(x = infection_to_isolation)
#   ) +
#   geom_histogram(
#     binwidth = 1,
#     colour = "white"
#   ) +
#   geom_vline(
#     xintercept = 5,
#     linetype = 2
#   ) +
#   theme_minimal()
# 
# # split TPs by factors for sanity checking
# sources %>%
#   group_by(
#     found,
#     symptomatic,
#     vaccinated,
#   ) %>%
#   summarise(
#     TP = mean(transmissions),
#   ) %>%
#   arrange(
#     found, desc(symptomatic),
#     vaccinated
#   )
# 
# 
# # check simulated delays match the expected
# delays <- res %>%
#   filter(
#     !is.na(source_id)
#   ) %>%
#   select(
#     contact_id = id,
#     id = source_id,
#     contact_isolation_day = isolation_day,
#     contact_infection_day = infection_day,
#     contact_case_found_by = case_found_by
#   ) %>%
#   left_join(
#     sources,
#     by = "id"
#   ) %>%
#   select(
#     source_id = id,
#     contact_id,
#     source_isolation_day = isolation_day,
#     source_infection_day = infection_day,
#     contact_isolation_day,
#     contact_infection_day,
#     contact_case_found_by
#   ) %>%
#   mutate(
#     contact_tracing_delay = contact_isolation_day - source_isolation_day,
#     generation_interval = contact_infection_day - source_infection_day
#   )
# 
# # check the CT delay (source isolation day to infectee isolation day, for
# # infectees found by contact tracing) has the same distribution as expected delays
# delays %>%
#   filter(
#     contact_case_found_by == "contact_tracing"
#   ) %>%
#   ggplot(
#     aes(
#       x = contact_tracing_delay
#     )
#   ) +
#   geom_histogram(
#     binwidth = 1,
#     colour = "white"
#   ) +
#   coord_cartesian(
#     xlim = c(0, 10)
#   )
# 
# # inputs
# tibble(
#   contact_tracing_delay = optimal_isol_interview_samples + rpois(length(optimal_isol_interview_samples), 0.5)
# ) %>%
#   ggplot(
#     aes(
#       x = contact_tracing_delay
#     )
#   ) +
#   geom_histogram(
#     binwidth = 1,
#     colour = "white"
#   ) +
#   coord_cartesian(
#     xlim = c(0, 10)
#   )
# 
# gi_density <- tibble(
#   days = 0:20
# ) %>%
#   mutate(
#     pmf = gi_pmf_discrete(days)
#   )
# 
# # check generation interval distribution 
# gi_delays <- delays %>%
#   filter(
#     is.finite(generation_interval)
#   )
# 
# # this should only match in the absence of TTIQ, TTIQ shifts it left
# gi_delays %>%
#   ggplot(
#     aes(
#       x = generation_interval
#     )
#   ) +
#   geom_histogram(
#     binwidth = 1,
#     colour = "white"
#   ) +
#   geom_line(
#     aes(
#       y = pmf * nrow(gi_delays),
#       x = days
#     ),
#     data = gi_density
#   )

