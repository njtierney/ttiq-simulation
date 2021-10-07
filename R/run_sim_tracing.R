#' Runs experiments with `sim_tracing()`
#'
#' @param derived_delay_distributions Dataframe of fitted delay distributions
#'
#' @return list of simulation results
#' @export
run_sim_tracing <- function(derived_delay_distributions) {

  priority_ranking_priority_new_swab <- function(x, sim_day, notification_time) {
    x %>%
      arrange(
        # Whether case is eligible to be interviewed
        desc(eligible_for_interview),
        # Priorities, in order of appearance
        desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
        desc(notification_date >= sim_day), # notified today first (maximise day 0s)
        swab_date, # newest first
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
        desc(swab_date), # oldest first
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
        swab_date, # newest first
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
        desc(swab_date), # oldest first
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
        swab_date, # newest first
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
        desc(swab_date), # oldest first
      )
  }

  priority_ranking_function_list_names = c('priority_ranking_priority_new_swab',
                                           'priority_ranking_priority_vaccine_old_swab',
                                           'priority_ranking_priority_vaccine_new_swab',
                                           'priority_ranking_vaccine_priority_old_swab',
                                           'priority_ranking_vaccine_priority_new_swab',
                                           'priority_ranking_vaccine_priority_old_notification_old_swab'
                            )

  priority_ranking_function_list = c(priority_ranking_priority_new_swab,
                                     priority_ranking_priority_vaccine_old_swab,
                                     priority_ranking_priority_vaccine_new_swab,
                                     priority_ranking_vaccine_priority_old_swab,
                                     priority_ranking_vaccine_priority_new_swab,
                                     priority_ranking_vaccine_priority_old_notification_old_swab
                            )

  priority_delay_average_list = c(0, 1, 2)

  output_list = list()
  output_names_list = c()
  for (i in 1:length(priority_ranking_function_list)){
    for (j in 1:length(priority_delay_average_list)){
      sim_tracing_output = sim_tracing(
        derived_delay_distributions,
        capacity_ratio = 0.8,
        prop_priority = 0.2,
        prop_time_delay = 0.2,
        priority_delay_distribution = function(n) rpois(n, priority_delay_average_list[j]),
        f_priority = priority_ranking_function_list[[i]],
        proportion_cases_vaccinated = 0.05,
        n_samples = 1e4
                      )
      sim_output_name = paste0(priority_ranking_function_list_names[i],
                               '_priority_delay_',
                               priority_delay_average_list[j])
      output_list = append(output_list, list(sim_tracing_output))
      output_names_list = c(output_names_list, sim_output_name)
      print(c(i,j))
    }
  }
names(output_list) = output_names_list
  output_list
  #example1 = sim_tracing(
  #  derived_delay_distributions,
  #  capacity_ratio = 0.8,
  #  prop_priority = 0.2,
  #  prop_time_delay = 0.2,
  #  priority_delay_distribution = function(n) rpois(n, 2),
  #  f_priority = priority_ranking_1,
  #  proportion_cases_vaccinated = 0.8,
  #  n_samples = 1e4
  #)
  #
  #list(example1)
}
#sim_tracing_output_list = run_sim_tracing(derived_delay_distributions)