
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
  R = 4.5,
  vaccination_coverage = 0.9,
  vaccination_test_seeking_multiplier = 1,
  passive_detection_given_symptoms = 0.5,
  screening = TRUE,
  contact_tracing = TRUE,
  isolation_to_interview_samples = optimal_isol_interview_samples
)

res <- sim_abm(
  infections = sim_initial_infections(500),
  parameters = parameters,
  max_infections = 10000,
  max_days = 365
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
  filter(
    case_found_by == "contact_tracing"
  ) %>%
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
    found,
    symptomatic,
    vaccinated,
  ) %>%
  summarise(
    TP = mean(transmissions),
  ) %>%
  arrange(
    found, desc(symptomatic),
    vaccinated
  )


# check simulated delays match the expected
delays <- res %>%
  filter(
    !is.na(source_id)
  ) %>%
  select(
    contact_id = id,
    id = source_id,
    contact_isolation_day = isolation_day,
    contact_infection_day = infection_day,
    contact_case_found_by = case_found_by
  ) %>%
  left_join(
    sources,
    by = "id"
  ) %>%
  select(
    source_id = id,
    contact_id,
    source_isolation_day = isolation_day,
    source_infection_day = infection_day,
    contact_isolation_day,
    contact_infection_day,
    contact_case_found_by
  ) %>%
  mutate(
    contact_tracing_delay = contact_isolation_day - source_isolation_day,
    generation_interval = contact_infection_day - source_infection_day
  )

# check the CT delay (source isolation day to infectee isolation day, for
# infectees found by contact tracing) has the same distribution as expected delays
delays %>%
  filter(
    contact_case_found_by == "contact_tracing"
  ) %>%
  ggplot(
    aes(
      x = contact_tracing_delay
    )
  ) +
  geom_histogram(
    binwidth = 1,
    colour = "white"
  ) +
  coord_cartesian(
    xlim = c(0, 10)
  )

# inputs
tibble(
  contact_tracing_delay = optimal_isol_interview_samples + rpois(length(optimal_isol_interview_samples), 0.5)
) %>%
  ggplot(
    aes(
      x = contact_tracing_delay
    )
  ) +
  geom_histogram(
    binwidth = 1,
    colour = "white"
  ) +
  coord_cartesian(
    xlim = c(0, 10)
  )

gi_density <- tibble(
  days = 0:20
) %>%
  mutate(
    pmf = gi_pmf_discrete(days)
  )

# check generation interval distribution 
gi_delays <- delays %>%
  filter(
    is.finite(generation_interval)
  )

# this should only match in the absence of TTIQ, TTIQ shifts it left
gi_delays %>%
  ggplot(
    aes(
      x = generation_interval
    )
  ) +
  geom_histogram(
    binwidth = 1,
    colour = "white"
  ) +
  geom_line(
    aes(
      y = pmf * nrow(gi_delays),
      x = days
    ),
    data = gi_density
  )

