#' Runs experiments with `sim_tracing()`
#' 
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#' 
#' @return list of simulation results
#' @export
run_sim_tracing <- function(derived_delay_distributions) {
  example1 = sim_tracing(
    derived_delay_distributions,
    capacity_ratio = 0.8,
    prop_priority = 0.4,
    prop_time_delay = 0.2,
    priority_delay_distribution = function(n) rpois(n, 2),
    f_priority = function(x, sim_day, notification_time) {
      x %>%
        arrange(
          # Whether case is eligible to be interviewed
          desc(notification_date <= sim_day & is.na(interview_date) & notification_date > (sim_day-14) & notification_time > .8),
          # Priorities
          desc(priority_group & (notification_date + priority_info_delay) <= sim_day),
          desc(notification_date >= sim_day),
          desc(swab_date),
        )
    },
    n_samples = 1e2
  )
  
  list(example1)
}

run_sim_tracing(derived_delay_distributions)