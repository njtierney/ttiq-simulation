#' Runs experiments with `sim_tracing()`
#' 
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#' 
#' @return list of simulation results
#' @export
run_sim_tracing <- function(derived_delay_distributions) {
  
  priority_ranking_1 <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        vaccinated, # vaccinated FALSE first
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        desc(swab_date), # oldest first
      )
  }
  
  example1 = sim_tracing(
    derived_delay_distributions,
    capacity_ratio = 0.8,
    prop_priority = 0.4,
    prop_time_delay = 0.2,
    priority_delay_distribution = function(n) rpois(n, 2),
    f_priority = priority_ranking_1,
    proportion_cases_vaccinated = 0.8,
    n_samples = 1e4
  )
  
  list(example1)
}
