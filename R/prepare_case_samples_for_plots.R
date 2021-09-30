#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples_against_data
#' @return
#' @author Nicholas Tierney
#' @export
prepare_case_samples_for_plots <- function(delay_samples_against_data) {
  
  delay_samples_against_data %>% 
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
    drop_na() %>%
    filter(delay_type != "full_contact_delay") %>% 
    filter(delay_type != "test_to_interview") %>% 
    mutate(
      delay_type = str_replace_all(delay_type, "_", " "),
      delay_type = as_factor(delay_type),
      delay_type = fct_relevel(
        delay_type,
        "swab to notification",
        "notification to interview",
        "other delays"
      ),
      scenario = case_when(
        scenario == "optimal" ~ "Optimal",
        scenario == "current" ~ "NSW Current\nwithout case-initiated",
        scenario == "current_case_init" ~ "NSW Current\nwith case-initiated",
      ),
      scenario = as_factor(scenario),
      scenario = fct_relevel(
        scenario,
        "Optimal",
        "NSW Current\nwithout case-initiated",
        "NSW Current\nwith case-initiated"
      )
    ) %>% 
    group_by(
      scenario, delay_type, data_type, days
    ) %>%
    drop_na() %>%
    summarise(
      count = n()
    ) %>%
    group_by(
      scenario, delay_type, data_type
    ) %>%
    mutate(
      fraction = count / sum(count)
    )
  # x %>% filter(scenario=="current", delay_type=="notification_to_interview", data_type=="data") %>%
  #   pull(fraction) %>% sum
  # table(x$scenario, x$delay_type)
}
