parameters <- setup_abm(
  R = 4,
  vaccination_coverage = 0.9,
  vaccination_test_seeking_multiplier = 1,
  screening = TRUE,
  contact_tracing = TRUE
)

res <- sim_abm(
  infections = sim_initial_infections(30),
  parameters = parameters,
  max_infections = 10000
)

hist(res$infection_day, breaks = 365)

# compute the numbers of onward transmissions for all sources
transmissions <- res %>%
  group_by(
    source_id
  ) %>%
  summarise(
    transmissions = n()
  )

# find source infections that we can trust onward infection for, and join on
# their number of transmissions 
sources <- res %>%
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
    by = c(id = "source_id")
  ) %>%
  mutate(
    transmissions = replace_na(transmissions, 0),
    found = !is.na(case_found_by),
    infection_to_isolation = isolation_day - infection_day
  )

sources %>%
  summarise(
    TP = mean(transmissions),
    ascertainment = mean(found)
  )

sources %>%
  ggplot(
    aes(x = infection_to_isolation)
  ) +
  geom_histogram(
    binwidth = 1,
    colour = "white"
  ) +
  geom_vline(
    xintercept = 5,
    linetype = 2
  ) +
  theme_minimal()

# split TPs by factors for sanity checking
sources %>%
  group_by(
    vaccinated,
    symptomatic,
    found
  ) %>%
  summarise(
    TP = mean(transmissions),
  )


