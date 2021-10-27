#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples_against_data
#' and color.
#' @return
#' @author Nicholas Tierney
#' @export
prepare_case_samples_for_plots <- function(delay_samples_against_data) {

  delay_samples_against_data %>% 
    # remove the extra scenarios with extra zeros being 0.2, 0.4, 0.6
    filter(is.na(prop_current_case_zero) | prop_current_case_zero == 0.8) %>% 
    select(-prop_current_case_zero) %>% 
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
    ) %>% 
    ungroup() %>% 
    mutate(
      scenario_renamed = case_when(
        scenario == "current_nsw" ~ "NSW Current\nwithout case-initiated",
        scenario == "current_nsw_case_init_0.8" ~ "NSW Current\nwith case-initiated",
        scenario == "current_vic" ~ "VIC Current\nwithout case-initiated",
        scenario == "current_vic_case_init_0.8" ~ "VIC Current\nwith case-initiated",
        scenario == "optimal" ~ "NSW Optimal",
        scenario == "partial" ~ "Partial",
      ),
      scenario_renamed = factor(scenario_renamed),
      scenario_renamed = fct_relevel(
        .f = scenario_renamed,
          "NSW Optimal",
          "NSW Current\nwithout case-initiated",
          "NSW Current\nwith case-initiated",
          "VIC Current\nwithout case-initiated",
          "VIC Current\nwith case-initiated",
          "Partial"
      )
    ) 
    
}
