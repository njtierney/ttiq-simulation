
#' Use dist_test_turnaround_time to create cases entering the system then simulate
#' the interview queue.
#' 
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#' @param capacity_ratio System can interview this proportion of the mean case rate
#' @param prop_priority Fraction of cases that are high priority
#' @param prop_time_delay Fraction of notifications that come in too late and can't
#' be interviewed on day 0
#' @param priority_delay_distribution Function that characterises the delay from notification
#' to finding out whether a case is a priority
#' @param f_priority Function that prioritises interviews
#' @param n_samples Length of queue to simulate. Currently no burn-in period.
#' 
#' @return Dataframe of generated delay samples including simulated time from notification to interview
#' 
#' @example sim_tracing(derived_delay_distributions,
#'   priority_delay_distribution = function(n) rpois(n, 2),
#'   f_priority = function(x) {
#'     x %>%
#'       arrange(
#'         # Whether case is eligible to be interviewed
#'         desc(notification_date <= sim_day & is.na(interview_date) & notification_date > (sim_day-14) & interview_time > prop_time_delay),
#'         # Priorities
#'         desc(priority_group & (notification_date + priority_info_delay) <= sim_day),
#'         desc(notification_date >= sim_day),
#'         desc(swab_date),
#'       )
#'   }
#' )
#' 
#' @export
sim_tracing <- function(derived_delay_distributions,
                        capacity_ratio = 0.8,
                        prop_priority = 0.4,
                        prop_time_delay = 0.2,
                        priority_delay_distribution = NULL,
                        f_priority = NULL,
                        proportion_cases_vaccinated = 0.05,
                        n_samples = 1e4) {
  
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
  capacity = capacity_ratio * rate
  
  queue_samples = lapply(generated_samples$scenario, function(scenario) {
    # Draw from distributions
    scenario_samples = generated_samples %>%
      filter(scenario == !!scenario) %>%
      unnest(cols = starts_with("samples_")) %>%
      mutate(interview_date = NA_integer_,
             notification_time = runif(n()),
             priority_group = runif(n()) < prop_priority,
             priority_info_delay = priority_delay_distribution(n()))
    
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
    # setDT(scenario_samples)

    # Set vaccinated or not
    scenario_samples <- scenario_samples %>% mutate(vaccinated = runif(nrow(scenario_samples)) <= proportion_cases_vaccinated)

    # Simulate queue
    message(glue("Simulating queue for {scenario} over {max(scenario_samples$notification_date)} iterations/days"))
    pb = txtProgressBar(min = 1, max = max(scenario_samples$notification_date), initial = 1) 
    for (sim_day in seq_len(max(scenario_samples$notification_date))) {
      setTxtProgressBar(pb, sim_day)

      scenario_samples = scenario_samples %>%
        #                               Case has been notified         no interview yet        less than 2 weeks old              if notified today, not notified too late
        mutate(eligible_for_interview = notification_date <= sim_day & is.na(interview_date) & notification_date > (sim_day-14) & !(notification_date==sim_day & notification_time > prop_time_delay)) %>%
        f_priority(sim_day, notification_time) %>%
        mutate(interview_date = ifelse(
          # Condition on whether case is eligible to be interviewed
          # Within capacity and      able to be interviewed
          row_number() <= capacity & eligible_for_interview,
          sim_day,         # Interview
          interview_date)) # No interview
      
      # DT version (contact Michael Lydeamore)
      # Arrange in order of priorities
      # scenario_samples <- scenario_samples[order(scenario_samples[, .(notification_date <= sim_day & is.na(interview_date) & notification_date > (sim_day-14),
      #                                                                 priority_group & (notification_date + priority_info_delay) <= sim_day,
      #                                                                 notification_date >= sim_day,
      #                                                                 swab_date)],
      #                                            decreasing = T),]
      # # Set the first `capacity` to be interviewed if not already interviewed
      # scenario_samples[,row_number := 1:.N][, interview_date := as.numeric(interview_date)][
      #   is.na(interview_date) & notification_date <= sim_day & row_number <= capacity,
      #   # Fraction get interviewed on the next day if they are too late in the day
      #   # Currently affects all interviews. Maybe it should only affect day 0 interviews?
      #   interview_date := ifelse(runif(.N) < 0.3, sim_day + 1, sim_day)]
    }
    close(pb)
    
    # Assemble output dataframe
    tibble(scenario = scenario,
           samples_time_to_interview = replace_na(scenario_samples$interview_date - scenario_samples$notification_date, -2),
           samples_priority_group = scenario_samples$priority_group,
           samples_vaccinated = scenario_samples$vaccinated
    ) %>%
      nest(samples_time_to_interview = samples_time_to_interview,
           samples_priority_group = samples_priority_group,
           samples_vaccinated = samples_vaccinated
           ) %>%
      mutate(samples_time_to_interview = lapply(samples_time_to_interview, function(x) x[[1]]),
             samples_priority_group = lapply(samples_priority_group, function(x) x[[1]]),
             samples_vaccinated = lapply(samples_vaccinated, function(x) x[[1]])
      )
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
