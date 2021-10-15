#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param queue_scenarios
#' @return
#' @author Nicholas Tierney
#' @export
tidy_queue_scenario <- function(queue_scenarios) {

  df <- bind_rows(
    queue_scenarios,
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
    ) %>% 
  # only `samples_time_to_interview` contains negative values (-2), which 
    # indicates individuals who were missed by passive and active tracing.
    # we are replacing these with Inf.
    mutate(samples_time_to_interview = map(
      .x = samples_time_to_interview,
      .f = ~impute_inf(x = .x, value = -2)
    ))
  
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
  
  df_joined <- df %>% 
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
  
  df_joined
  
}
