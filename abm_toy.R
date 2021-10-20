res <- sim_abm(
  infections = sim_initial_infections(30),
    parameters = setup_abm(
      R = 3.5,
      vaccination_coverage = 0.9,
      vaccination_test_seeking_multiplier = 0
  ),
  max_infections = 10000
)

hist(res$infection_day, breaks = 365)

res$isolation_day - res$infection_day

ascertainment <- mean(!is.na(res$case_found_by))
ascertainment

# plot times to isolation
res %>%
  filter(
    !is.na(case_found_by)
  ) %>%
  mutate(
    infection_to_isolation = isolation_day - infection_day
  ) %>%
  ggplot(
    aes(
      x = infection_to_isolation
    )
  ) +
  geom_histogram(
    binwidth = 1,
    color = "white"
  ) +
  theme_minimal() +
  geom_vline(
    xintercept = 5,
    linetype = 2
  )

time_to_isolation <- res$isolation_day - res$infection_day
tp_multiplier <- tp_reduction(
  time_to_isolation,
  meanlog = 1.375738,
  sdlog = 0.5665299
)

# percentage reduction
100 * (1 - tp_multiplier)

