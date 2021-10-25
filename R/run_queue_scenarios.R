#' Runs experiments with `sim_tracing()`
#'
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#' @param n_samples How many samples to run
#'
#' @return list of simulation results
#' @export
run_queue_scenarios <- function(derived_delay_distributions, n_samples = 1e4) {
  
  # Define arbitrary ranking functions
  priority_ranking_priority_new_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        desc(swab_date), # newest first
      )
  }
  
  random_swab <- function(x, sim_day, notification_time) {
    x %>%
      mutate(ix = runif(n())) %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        ix
      ) %>%
      select(-ix)
  }
  
  priority_ranking_new_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        desc(swab_date), # newest first
      )
  }
  
  priority_ranking_new_swab_vaccine <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(swab_date), # newest first
        vaccinated # vaccinated FALSE first
      )
  }
  
  
  priority_ranking_priority_vaccine_old_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        vaccinated, # vaccinated FALSE first
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        swab_date, # oldest first
      )
  }
  
  
  priority_ranking_vaccine_new_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        vaccinated, # vaccinated FALSE first
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        desc(swab_date), # newest first
      )
  }
  
  priority_ranking_priority_vaccine_new_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        vaccinated, # vaccinated FALSE first
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        desc(swab_date), # newest first
      )
  }
  
  priority_ranking_vaccine_priority_old_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        vaccinated, # vaccinated FALSE first
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        swab_date, # oldest first
      )
  }
  
  priority_ranking_vaccine_priority_new_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        vaccinated, # vaccinated FALSE first
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        desc(swab_date), # newest first
      )
  }
  priority_ranking_vaccine_priority_old_notification_old_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        vaccinated, # vaccinated FALSE first
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        notification_date >= sim_day, # notified today last
        swab_date, # oldest first
      )
  }
  
  ranking_functions = list(
    # 'priority_new_swab'  = priority_ranking_priority_new_swab,
    # 'priority_vaccine_old_swab' = priority_ranking_priority_vaccine_old_swab,
    'random' = random_swab,
    'new_swab' = priority_ranking_new_swab,
    'vaccine_new_swab' = priority_ranking_vaccine_new_swab,
    'new_swab_vaccine' = priority_ranking_new_swab_vaccine
    # 'priority_vaccine_new_swab' = priority_ranking_priority_vaccine_new_swab,
    # 'vaccine_priority_old_swab' = priority_ranking_vaccine_priority_old_swab,
    # 'vaccine_priority_new_swab' = priority_ranking_vaccine_priority_new_swab#,
    # 'random_swab' = random_swab
    # 'vaccine_priority_old_notification_old_swab' = priority_ranking_vaccine_priority_old_notification_old_swab
  )
  
  priority_delay_function = list(
    # delayzero = function(n) rep(0, n),
    delaypoisson1 = function(n) rpois(n, 1)
  )
  
  capacity_ratios = c(capacity0pc=0, capacity20pc=0.2, capacity50pc=0.5, capacity80pc=0.8)
  
  max_days = c(max_5d=5)
  
  cases_vaccinated = c(casesvaccinated25pc=0.25, casesvaccinated50pc=0.5)
  
  # Create a list crossing combinations of input parameters for each run
  sim_params = lapply(ranking_functions, function(x) {
    lapply(priority_delay_function, function(y) {
      lapply(capacity_ratios, function(z) {
        lapply(max_days, function(w) {
          lapply(cases_vaccinated, function(v) {
            list(f = x,
                 priority_delay_fn = y,
                 capacity_ratio = z,
                 proportion_cases_vaccinated = v,
                 max_interview_delay = w)
          })
        }) %>%
          unlist(recursive=FALSE)
      }) %>%
        unlist(recursive=FALSE)
    }) %>%
      unlist(recursive=FALSE)
  }) %>%
    unlist(recursive=FALSE)
  
  
  message("Iterating over ", length(sim_params), " elements of `sim_params`")
  
  results = future_lapply(sim_params, function(x) {
    sim_tracing_output = sim_tracing(
      derived_delay_distributions %>%
        # only using 'optimal' for now
        filter(scenario == "optimal"),
      capacity_ratio = x$capacity_ratio,
      prop_priority = 0.2,
      prop_time_delay = 0.2,
      max_interview_delay = x$max_interview_delay,
      priority_delay_distribution = x$priority_delay_fn,
      f_priority = x$f,
      proportion_cases_vaccinated = x$proportion_cases_vaccinated,
      n_samples = n_samples
    )
  })
  
}
#sim_tracing_output_list = run_sim_tracing(derived_delay_distributions)