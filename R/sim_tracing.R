
#' Use dist_test_turnaround_time to create cases entering the system then simulate
#' the interview queue.
#' 
#' @param cases_scenario Dataframe of delay observations (unused)
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#' used to generate `n_samples` people. Ignores `dist_time_to_interview`
#' @param n_samples Length of queue to simulate. Currently no burn-in period.
#' 
#' @return Dataframe of generated delay samples including simulated time from notification to interview
#' 
#' @export
sim_tracing <- function(cases_scenario, derived_delay_distributions, n_samples=1e4) {
  
  generated_samples <- derived_delay_distributions %>%
    select(scenario, dist_isol_swab, dist_test_turnaround_time) %>%
    mutate(across(
      .cols = c(starts_with("dist_")),
      .fns = ~ generate(.x, times = n_samples),
      .names = "samples_{.col}"
    )) %>%
    rename_with(
      .cols = starts_with("samples_"),
      .fn = ~ str_remove_all(.x, "dist_")
    ) %>%
    select(-starts_with("dist_"))
  
  
  # hardcoded parameters for now
  rate = 20 # poisson(rate) per day
  if (n_samples / rate < 1000) warning("n_samples / rate < 1000. Recommend increasing n_samples or decreasing rate to simulate more days.")
  capacity = 0.8 * rate
  
  queue_samples = lapply(generated_samples$scenario, function(scenario) {
    # Draw from distributions
    scenario_samples = generated_samples %>%
      filter(scenario == !!scenario) %>%
      unnest(cols = starts_with("samples_")) %>%
      mutate(interview_date = NA_integer_,
             priority_group = runif(n()) < 0.4,
             priority_info_delay = rpois(n(), 2))
    
    # Generate a sample case rate with a Poisson process
    # Loop isn't slow enough to bother vectorising
    .swab_date = numeric(0)
    i = 1
    while (length(.swab_date) < n_samples) {
      # .swab_date = c(.swab_date, rep(i, rpois(1, rate)))
      .swab_date = c(.swab_date, rep(i, max(0, rnorm(1, rate, sqrt(0.25*rate)))))
      i = i + 1
    }
    # Notify Dept after test is processed
    scenario_samples = scenario_samples %>%
      mutate(swab_date = .swab_date[seq_len(n_samples)],
             notification_date = swab_date + samples_test_turnaround_time)
    setDT(scenario_samples)
    
    # Simulate queue
    message(glue("Simulating queue for {max(scenario_samples$notification_date)} iterations/days"))
    pb = txtProgressBar(min = 1, max = max(scenario_samples$notification_date), initial = 1) 
    for (sim_day in seq_len(max(scenario_samples$notification_date))) {
      setTxtProgressBar(pb, sim_day)
      
      # scenario_samples = scenario_samples %>%
      #   arrange(
      #     desc(notification_date <= sim_day & is.na(interview_date) & notification_date > (sim_day-14)),
      #     desc(priority_group & (notification_date + priority_info_delay) <= sim_day),
      #     desc(notification_date >= sim_day),
      #     desc(swab_date),
      #   ) %>%
      #   mutate(interview_date = ifelse(
      #     is.na(interview_date) & notification_date <= sim_day & row_number() <= capacity,
      #     ifelse(runif(n()) < 0.3, sim_day + 1, sim_day),
      #     interview_date))
      
      # DT version
      # Arrange in order of priorities
      scenario_samples <- scenario_samples[order(scenario_samples[, .(notification_date <= sim_day & is.na(interview_date) & notification_date > (sim_day-14), 
                                                                      priority_group & (notification_date + priority_info_delay) <= sim_day,
                                                                      notification_date >= sim_day,
                                                                      swab_date)],
                                                 decreasing = T),]
      # Set the first `capacity` to be interviewed if not already interviewed
      scenario_samples[,row_number := 1:.N][, interview_date := as.numeric(interview_date)][ 
        is.na(interview_date) & notification_date <= sim_day & row_number <= capacity, 
        # Fraction get interviewed on the next day if they are too late in the day
        # Currently affects all interviews. Maybe it should only affect day 0 interviews?
        interview_date := ifelse(runif(.N) < 0.3, sim_day + 1, sim_day)]
    }
    close(pb)
    
    # Assemble output dataframe
    tibble(scenario = scenario,
           samples_time_to_interview = replace_na(scenario_samples$interview_date - scenario_samples$notification_date, -2),
           samples_priority_group = scenario_samples$priority_group) %>%
      nest(samples_time_to_interview = samples_time_to_interview,
           samples_priority_group = samples_priority_group)
  }) %>%
    bind_rows()
  
  # Return a dataframe with additional columns
  generated_samples = generated_samples %>%
    left_join(queue_samples, by="scenario")
  
  
  # Diagnostic plots
  # {
  #   scenario_samples = generated_samples %>%
  #     filter(scenario == "current_nsw") %>%
  #     unnest(cols = starts_with("samples_"))
  #   xmax = max(scenario_samples$samples_time_to_interview)
  #   p1 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
  #     geom_bar() +
  #     coord_cartesian(xlim=c(-2, NA)) +
  #     scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax)) +
  #     labs(title = glue("Interview capacity of {capacity/rate} x rate"),
  #          fill = "Interviewed\n(to show NAs)")
  #   
  #   p2 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
  #     geom_bar() +
  #     facet_wrap(vars(samples_priority_group)) +
  #     coord_cartesian(xlim=c(-2, NA)) +
  #     scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax)) +
  #     labs(title = glue("Facet by priority group"),
  #          fill = "Interviewed\n(to show NAs)")
  #   
  #   p1 / p2
  #   }
}

# # From Nick Golding on Slack
# test_queue <- function() {
#   sample_test_turnaround <- function(n) rpois(n, 1)
#   sim_new_cases <- function(n) {
#     tibble(
#       test_turnaround = sample_test_turnaround(n),
#       case_age = rep(0, n)
#     )
#     
#   }
#   
#   library(tidyverse)
#   
#   daily_capacity <- 50
#   initial_cases <- 200
#   
#   cases_to_interview <- sim_new_cases(initial_cases)
#   cases_processed <- NULL
#   
#   for (i in 1:200) {
#     
#     new_cases_to_interview <- sim_new_cases(runif(1, 50, 100))
#     # new_cases_to_interview <- sim_new_cases(daily_capacity)
#     cases_to_interview <- bind_rows(
#       cases_to_interview,
#       new_cases_to_interview
#     ) %>%
#       mutate(
#         time_since_swab = test_turnaround + case_age
#       ) %>%
#       # prioritise
#       arrange(
#         
#         # prioritise those entered in the last week
#         time_since_swab > 8,
#         
#         # within the rest, split into a priority group (40% - clusters we are
#         # focussing on?)
#         # rbinom(n(), 1, 0.4),
#         
#         # among those two groups, prioritise those entered today
#         time_since_swab > 0,
#         
#         # otherwise do the oldest ones first
#         # desc(time_since_swab)
#         time_since_swab
#         
#       )
#     
#     # track the number processed  
#     cases_processed <- cases_to_interview %>%
#       head(daily_capacity) %>%
#       mutate(
#         iteration = i
#       ) %>%
#       bind_rows(
#         cases_processed,
#       )
#     
#     
#     # reset the remainign cases and increment their time in the system
#     cases_to_interview <- tail(cases_to_interview, initial_cases) %>%
#       mutate(
#         case_age = case_age + 1
#       )
#     
#   }
#   
#   cases_sampled <- cases_processed %>%
#     rename(
#       time_to_interview = case_age
#     ) %>%
#     filter(
#       iteration > 100
#     )
#   
#   cases_sampled %>%
#     plot(
#       jitter(time_to_interview) ~
#         jitter(test_turnaround),
#       data = .)
#   
#   cases_sampled %>%
#     pull(time_to_interview) %>%
#     hist()
# }
