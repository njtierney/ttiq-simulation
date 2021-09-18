#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples_against_data
#' @return
#' @author Nicholas Tierney
#' @export
prepare_case_samples_for_plots <- function(nsw_delay_samples_against_data) {

  
  nsw_delay_samples_against_data %>% 
    rename(
      other_delays = isol_swab,
      swab_to_notification = test_turnaround_time,
      notification_to_interview = time_to_interview
    ) %>% 
    mutate(
      data_type = case_when(
        data_type == "samples" ~ "model",
        data_type == "data" ~ "data"
      )
    ) %>% 
    pivot_longer(
      cols = -c(data_type, scenario),
      names_to = "delay_type",
      values_to = "days"
    ) %>% 
    filter(delay_type != "full_contact_delay") %>% 
    filter(delay_type != "test_to_interview") %>% 
    mutate(delay_type = as_factor(delay_type),
           delay_type = fct_relevel(delay_type,
                                    "swab_to_notification",
                                    "notification_to_interview",
                                    "other_delays"
           ),
           scenario = as_factor(scenario),
           scenario = fct_relevel(scenario,
                                  "optimal",
                                  "current",
                                  "current_case_init")) %>% 
    group_by(
      scenario, delay_type, data_type, days
    ) %>%
    summarise(
      count = n()
    ) %>%
    group_by(
      scenario, delay_type, data_type
    ) %>%
    mutate(
      fraction = count / sum(count)
    )
  
}
