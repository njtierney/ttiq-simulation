#' Runs experiments with `sim_tracing()`
#'
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#'
#' @return list of simulation results
#' @export
run_sim_tracing <- function(derived_delay_distributions) {
  
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
    # 'priority_vaccine_new_swab' = priority_ranking_priority_vaccine_new_swab,
    # 'vaccine_priority_old_swab' = priority_ranking_vaccine_priority_old_swab,
    'vaccine_priority_new_swab' = priority_ranking_vaccine_priority_new_swab#,
    # 'vaccine_priority_old_notification_old_swab' = priority_ranking_vaccine_priority_old_notification_old_swab
  )
  
  priority_delay_function = list(
    # delayzero = function(n) rep(0, n),
    delaypoisson1 = function(n) rpois(n, 1)
  )
  
  capacity_ratios = c(capacity30pct=0.3, capacity50pct=0.5, capacity80pct=0.8)
  
  # Create a list crossing combinations of input parameters for each run
  sim_params = lapply(ranking_functions, function(x) {
    lapply(priority_delay_function, function(y) {
      lapply(capacity_ratios, function(z) {
        list(f = x,
             priority_delay_fn = y,
             capacity_ratio = z)
      })
    }) %>%
      unlist(recursive=FALSE)
  }) %>%
    unlist(recursive=FALSE)
  
  results = lapply(sim_params, function(x) {
    sim_tracing_output = sim_tracing(
      derived_delay_distributions %>%
        # only using 'optimal' for now
        filter(scenario == "optimal"),
      capacity_ratio = x$z,
      prop_priority = 0.2,
      prop_time_delay = 0.2,
      priority_delay_distribution = x$priority_delay_fn,
      f_priority = x$f,
      proportion_cases_vaccinated = 0.05,
      n_samples = 1e4
    )
  })
  
}
#sim_tracing_output_list = run_sim_tracing(derived_delay_distributions)