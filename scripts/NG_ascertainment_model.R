# messing about with the ascertainment test model
library(tidyverse)
library(patchwork)

# make a fake temporal trend
random_trend <- function(dates, prob = 0.5, variance = 0.3) {
  plogis(qlogis(prob) + variance * sin(dates / runif(1, 50, 100) - runif(1, 50, 100)))
}

# calculate the ascertainment probability for a set of scalar dates
predict_ascertainment_once <- function(
  close_contact_test_prob, # prob test bc cc?
  symptomatic_test_prob, # prob of test bc symptomatic?
  screening_test_prob, # prob of testing bc work context?
  symptomatic_fraction, # fraction of infections that are symptomatic
  close_contact_fraction # prob of being a close contact
) {

  # the probability of being detected based on symptoms alone, screening alone,
  # close contact notification alone
  # detected means that a person presents themselves for a test
  # a test means PCR swab only...??
  p_detected_symptoms <- symptomatic_fraction * symptomatic_test_prob
  p_detected_screening <- screening_test_prob
  p_detected_cc <- close_contact_test_prob

  # probability of being a close contact
  p_cc <- close_contact_fraction
  p_ncc <- 1 - p_cc

  # for all detected cases, the fraction belonging[??] to close contacts is
  # 'close_contact_fraction', so the fraction being not close contacts is 1
  # minus that

  # for close contacts, the probability of nondetection is calculated from the
  # probabilities of each other type of detection, and the probability of
  # detection is the complement of this
  p_undetected_cc <- (1 - p_detected_cc) *
    (1 - p_detected_screening) *
    (1 - p_detected_symptoms)

  p_detected_cc <- 1 - p_undetected_cc

  # for non-close contacts, detection is solely due to symptomatic test seeking
  p_detected_ncc <- p_detected_symptoms
  p_undetected_ncc <- 1 - p_detected_ncc

  # combine with the probability of being a close contact to get all elements of
  # the matrix

  # if source is detected, the offspring are a mixture of close contacts and
  # non-close contacts, each with their own detection probabilities
  detected_detected <- p_cc * p_detected_cc + p_ncc * p_detected_ncc
  detected_undetected <- p_cc * p_undetected_cc + p_ncc * p_undetected_ncc

  # if the source was undetected, the offspring are all non-close contacts
  undetected_detected <- p_detected_ncc
  undetected_undetected <- p_undetected_ncc

  # build the 2x2 matrix, where the two states are: detected, undetected, in
  # that order
  mat <- matrix(
    c(
      detected_detected,
      detected_undetected,
      undetected_detected,
      undetected_undetected
    ),
    nrow = 2,
    ncol = 2
  )

  # analyse the matrix to get the fraction in each bin
  stable_state <- eigen(mat)$vectors[, 1]
  stable_state <- stable_state / sum(stable_state)

  # the ascertainment rate is the stable fraction in the first bin (proportion
  # of infections in the detected bin at each iteration)
  ascertainment <- stable_state[1]

  # note - this has an analytic solution. I'm not calculating it as we will
  # likely switch to multiple states (reasons for testing), and non-stable
  # state solutions

  ascertainment

}


predict_ascertainment <- function(
  close_contact_test_prob,
  symptomatic_test_prob,
  screening_test_prob,
  symptomatic_fraction,
  close_contact_fraction
) {

  output <- mapply(
    FUN = predict_ascertainment_once,
    close_contact_test_prob = close_contact_test_prob,
    symptomatic_test_prob = symptomatic_test_prob,
    screening_test_prob = screening_test_prob,
    symptomatic_fraction = symptomatic_fraction,
    close_contact_fraction = close_contact_fraction
  )

}

set.seed(2)

# make fake ascertainment data streams ----
data <- tibble(
  date_num = -30:150,
  date = Sys.Date() + date_num
) %>%
  mutate(
    close_contact_test_prob = random_trend(date_num, 0.95), 
    symptomatic_test_prob = random_trend(date_num, 0.8, variance = 1),
    screening_test_prob = random_trend(date_num, 0.1),
    symptomatic_fraction = random_trend(date_num, 0.5),
    close_contact_fraction = random_trend(date_num, 0.9, variance = 2)
  ) %>%
  pivot_longer(
    cols = c(-date, -date_num),
    names_to = "parameter",
    values_to = "value"
  )

# compute ascertainment over these
predictions <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  ) %>%
  mutate(
    ascertainment = predict_ascertainment(
      close_contact_test_prob = close_contact_test_prob,
      symptomatic_test_prob = symptomatic_test_prob,
      screening_test_prob = screening_test_prob,
      symptomatic_fraction = symptomatic_fraction,
      close_contact_fraction = close_contact_fraction
    ),
    .after = date
  )

# plot simulated datastreams and predictions
data_plot <- data %>%
  ggplot(
    aes(
      x = date,
      y = value,
      colour = parameter
    )
  ) +
  geom_line(
    size = 1.5
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  theme_minimal() +
  ggtitle(
    "Simulated data timeseries"
  )


pred_plot <- predictions %>%
  ggplot(
    aes(
      x = date,
      y = ascertainment
    )
  ) +
  geom_line(
    size = 1.5
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  theme_minimal() +
  ggtitle(
    "Ascertainment prediction"
  )

ggsave(
  "~/Desktop/ascertainment_sim.png",
  plot = data_plot + pred_plot,
  bg = "white",
  width = 10,
  height = 4
)
