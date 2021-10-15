#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param experiment_result
#' @return
#' @author Nicholas Tierney
#' @export
tidy_queue_simulation <- function(experiment_result) {

  df <- bind_rows(
    experiment_result,
    .id = "queue_type"
    ) %>% 
    separate(
      col = queue_type,
      into = c("priority_type",
               "priority_growth_rate",
               "capacity",
               "max"),
      sep = "\\."
    ) %>% 
    mutate(
      priority_growth_rate = str_replace_all(priority_growth_rate,
                                             "delay",
                                             "delay_"),
      scenario = glue(
        "{scenario}_{priority_type}_{priority_growth_rate}_{capacity}_{max}"
      )
    )
  
  df_samples <- df %>%
    rowwise() %>% 
    mutate(samples = list(tibble(samples_isol_swab,
                                 samples_test_turnaround_time,
                                 samples_time_to_interview,
                                 samples_vaccinated,
                                 samples_priority_group)),
           .after = scenario) %>% 
    select(
      - samples_isol_swab,
      - samples_test_turnaround_time,
      - samples_time_to_interview,
      - samples_vaccinated,
      - samples_priority_group
    ) %>% 
    select(scenario,
           samples) %>% 
    ungroup() %>% 
    unnest(cols = everything()) %>% 
    mutate(
      tracing_delay = samples_isol_swab + 
        samples_test_turnaround_time + 
        samples_time_to_interview,
      .after = scenario
    ) %>% 
    rename(vaccinated = samples_vaccinated,
           priority = samples_priority_group) %>% 
    relocate(vaccinated,
             priority,
             .after = tracing_delay) %>% 
    group_by(scenario) %>% 
    nest(samples = -scenario)
  
  df %>% 
    select(scenario,
           priority_type,
           priority_growth_rate,
           capacity,
           max,
           capacity_ratio) %>% 
    left_join(
      df_samples,
      by = "scenario"
    )
  
}
