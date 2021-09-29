
nsw <- read_cases_nsw("~/not_synced/vaccination/nsw/CASES_FROM_20200701_0000_TO_20210913_1115.xlsx")

nsw_delays <- nsw %>%
  select(
    date_infection = setting_of_transmission_date,
    date_onset = symptom_onset_date,
    date_isolation = date_isolation_began,
    date_detection = earliest_confirmed_or_probable
  ) %>%
  mutate(
    across(
      everything(),
      as_date
    )
  ) %>%
  mutate(
    date_infection = case_when(
      date_infection > date_onset ~ as_date(NA),
      date_infection > date_isolation ~ as_date(NA),
      date_infection > date_detection ~ as_date(NA),
      date_infection < as.Date("2020-01-01") ~ as_date(NA),
      TRUE ~ date_infection
    )
  ) %>%
  mutate(
    date_onset = date_infection + 5,
    time_to_isolation = as.numeric(date_isolation - date_onset),
    time_to_detection = as.numeric(date_detection - date_onset),
    state = "NSW"
  ) %>%
  mutate(
    time_to_isolation = set_na_when_not_between(
      time_to_isolation,
      -5, 21
    ),
    time_to_detection = set_na_when_not_between(
      time_to_detection,
      -5, 21
    )
  ) %>%
  filter(
    !is.na(time_to_isolation)
  ) 

# nsw_delays %>%
#   ggplot(
#     aes(
#       x = time_to_isolation,
#       y = time_to_detection
#     )
#   ) +
#   geom_jitter()

# 'optimal'
nsw_delays %>%
  filter(
    date_detection < as_date("2021-02-01")
  ) %>%
  ggplot(
    aes(
      x = time_to_isolation
    )
  ) +
  geom_histogram(
    binwidth = 1,
    color = "white"
  )



# do this as per partial - compute the surveillance effect delay from NNDSS

cutoff_date <- as_date("2021-02-01")

# compute overall ecdf from before cutoff
optimal_ecdf <- nsw_delays %>%
  filter(
    date_infection < cutoff_date
  ) %>%
  pull(time_to_isolation) %>%
  ecdf

# interpolate to now, based on surveillance effect
surveillance_cdfs <- readRDS("data/delay_from_onset_cdfs.RDS")
surveillance <- readRDS("data/surveillance_matrix.RDS")

# convert surveillance effect to weights (to represent how the effectiveness
# of the contact tracing system changed over time) and compute a weighted
# time-to-isolation cdf for each date and state
isolation_cdfs <- surveillance_cdfs %>%
  ungroup() %>%
  rename(
    surveillance_cdf = ecdf
  ) %>%
  mutate(
    surveillance_effect = c(t(surveillance)),
    optimal_ecdf = list(optimal_ecdf),
    isolation_weight = 1 - surveillance_effect,
    isolation_weight = isolation_weight / max(isolation_weight),
    isolation_ecdf = mapply(
      FUN = weight_ecdf,
      surveillance_cdf,
      optimal_ecdf,
      1 - isolation_weight,
      SIMPLIFY = FALSE
    )
  ) %>%
  select(
    date,
    state,
    ecdf = isolation_ecdf
  )

# ifind the minimum
surveillance_cdfs %>%
  filter(state == "NSW") %>%
  pull(date) %>%
  `[`(which.min(surveillance[, 2]))

ttiq_ecdfs <- isolation_cdfs %>%
  mutate(
    which = case_when(
      state == "NSW" & date == as_date("2021-01-14") ~ "optimal",
      state == "VIC" & date == as_date("2020-08-04") ~ "partial",
      state == "NSW" & date == as_date("2021-08-15") ~ "current",
      TRUE ~ NA_character_
    ) 
  ) %>%
  filter(
    !is.na(which)
  ) %>%
  select(which, ecdf) %>%
  pivot_wider(
    names_from = which,
    values_from = ecdf
  )

tti_distributions <- tibble(
  days = environment(ttiq_ecdfs$partial[[1]])$x,
  partial = environment(ttiq_ecdfs$partial[[1]])$y
) %>%
  left_join(
    tibble(
      days = environment(ttiq_ecdfs$optimal[[1]])$x,
      optimal = environment(ttiq_ecdfs$optimal[[1]])$y
    ),
    by = "days"
  ) %>%
  left_join(
    tibble(
      days = environment(ttiq_ecdfs$current[[1]])$x,
      current = environment(ttiq_ecdfs$current[[1]])$y
    ),
    by = "days"
  ) %>%
  arrange(
    days
  ) %>%
  mutate(
    across(
      c(partial, optimal, current),
      ~diff(c(0, .x))
    ),
    across(
      c(partial, optimal, current),
      ~replace_na(.x, 0)
    ),
    across(
      c(partial, optimal, current),
      ~.x / sum(.x)
    )
  )

tti_distributions %>%
  mutate(
    days = days + 5 - 0.5,
    across(
      c(optimal, partial, current),
      cumsum
    )
  ) %>%
  filter(
    days <= 27
  ) %>%
  pivot_longer(
    cols = c("current", "optimal"),
    names_to = "TTIQ effectiveness",
    values_to = "cdf"
  ) %>%
  mutate(
    `TTIQ effectiveness` = factor(
      `TTIQ effectiveness`,
      levels = rev(unique(`TTIQ effectiveness`))
    )
  ) %>%
  ggplot(
    aes(
      days, cdf, col = `TTIQ effectiveness`
    )
  ) +
  geom_vline(
    xintercept = 5,
    linetype = 3,
    col = grey(0.5)
  ) +
  annotate(
    geom = "text",
    x = 5 + 0.2,
    y = 0,
    hjust = 0,
    vjust = 0,
    label = "Symptom\nonset",
    col = grey(0.5),
    size = 3
  ) +
  geom_step() +
  geom_text(
    aes(
      x = days + 0.5,
      label = scales::percent(cdf, accuracy = 1),
      vjust = ifelse(
        `TTIQ effectiveness` == "optimal",
        -1, 2)
    ),
    size = 2.5
  ) +
  annotate(
    "text",
    x = 2,
    y = 0.75,
    label = "optimal TTIQ",
    col = scales::hue_pal()(2)[1]
  ) +
  annotate(
    "text",
    x = 8,
    y = 0.5,
    label = "current TTIQ",
    col = scales::hue_pal()(2)[2]
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Days since infection") +
  ylab("Cases isolated") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = "figs/nsw_ttiq_step.png",
  bg = "white",
  height = 4,
  width = 5
)

# calculate tp reductions
tp_reductions <- tti_distributions %>%
  mutate(
    gi_pmf = gi_pmf_discrete(
      days + 5
    )
  ) %>%
  pivot_longer(
    cols = c(partial, optimal, current),
    names_to = "ttiq_effectiveness",
    values_to = "delay_pmf"
  ) %>%
  group_by(
    ttiq_effectiveness
  ) %>%
  arrange(
    ttiq_effectiveness,
    days
  ) %>%
  summarise(
    avg_days = weighted.mean(days + 5, delay_pmf),
    tp_reduction = round(1 - weighted.mean(1 - cumsum(delay_pmf), gi_pmf), 2)
  )

df_annotate <- tp_reductions %>%
  rename(
    `TTIQ effectiveness` = ttiq_effectiveness
  ) %>%
  mutate(
    `TTIQ effectiveness` = paste(
      stringr::str_to_sentence(`TTIQ effectiveness`),
      "TTIQ"
    ),
    `TTIQ effectiveness` = factor(
      `TTIQ effectiveness`,
      levels = c("Optimal TTIQ", "Current TTIQ", "Partial TTIQ"),
      labels = c("Optimal",
                 "Current without case-initated CT",
                 "Current + case-initated CT" 
                 )
    ),
    x = 12,
    y = 0.1,
    message = glue("{percent(tp_reduction, accuracy = 1)} reduction\n{round(avg_days)} day average")
  )
  
cols = scales::hue_pal()(3)[c(1, 3)]

# plot histograms
tti_distributions %>%
  pivot_longer(
    cols = c("current", "partial", "optimal"),
    names_to = "TTIQ effectiveness",
    values_to = "pdf"
  ) %>%
  mutate(
    colour = case_when(
      `TTIQ effectiveness` == "optimal" ~ 1,
      `TTIQ effectiveness` == "partial" ~ 2,
      `TTIQ effectiveness` == "current" ~ 3,
    ),
    `TTIQ effectiveness` = paste(
      stringr::str_to_sentence(`TTIQ effectiveness`),
      "TTIQ"
    )
  ) %>%
  mutate(
    `TTIQ effectiveness` = factor(
      `TTIQ effectiveness`,
      levels = c("Optimal TTIQ", "Current TTIQ", "Partial TTIQ"),
      labels = c("Optimal",
                 "Current without case-initated CT",
                 "Current + case-initated CT" 
      )
    )
  ) %>%
  ggplot(
    aes(
      days + 5,
      pdf,
      fill = `TTIQ effectiveness`
    )
  ) +
  facet_wrap(
    facets = vars(`TTIQ effectiveness`),
    ncol = 3
  ) +
  geom_vline(
    xintercept = 5,
    linetype = 3,
    col = grey(0.5)
  ) +
  geom_col() +
  coord_cartesian(xlim = c(-1, 13.5)) +
  scale_x_continuous(
    breaks = seq(0, 15, by = 5),
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab("Days since infection") +
  ylab("Cases isolated") +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  scale_fill_discrete(type = cols) +
  geom_text(
    aes(
      x = x,
      y = y,
      label = message
    ),
    data = df_annotate
  )

ggsave(
  filename = "figs/nsw_ttiq_hist.png",
  bg = "white",
  height = 6,
  width = 6
)
