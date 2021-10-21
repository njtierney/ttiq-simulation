
# get contact tracing delays for optimal period
optimal_isol_interview_samples <- read_csv(
  "outputs/optimal_delay_samples.csv",
  col_types = cols(
    time_to_swab = col_double(),
    test_turnaround_time = col_double(),
    time_to_interview = col_double(),
    time_to_isolation = col_double()
  )
) %>%
  # drop the time from source interview to isolation, since that is different
  # for each contact, and sampled independently
  select(
    -time_to_isolation
  ) %>%
  mutate(
    tracing_delay = Reduce(
      "+",
      across(
        everything()
      )
    )
  ) %>%
  pull(tracing_delay)

parameters <- setup_abm(
  R = 3.9,
  vaccination_coverage = 0.9,
  vaccination_test_seeking_multiplier = 1,
  passive_detection_given_symptoms = 0.5,
  isolation_to_interview_samples = optimal_isol_interview_samples
)

future::plan(multisession(workers = 5))
res <- get_valid_abm_samples(parameters, n_samples = 10)
metrics <- compute_abm_metrics(res)

# set up to run for multiple scenarios; this as the baseline one, then ones for reduced stringency

# compute TP ratios


# 
# 
# 
# 
# 
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

