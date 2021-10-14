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
               "priority_growth_rate"),
      sep = "\\."
    ) %>% 
    mutate(
      priority_growth_rate_num = parse_number(priority_growth_rate),
      priority_growth_rate = str_remove_all(string = priority_growth_rate, 
                                            pattern = "[0-9]"),
      priority_growth_rate = glue(
        "{priority_growth_rate}_{priority_growth_rate_num}"
        ),
      scenario = glue(
        "{scenario}_{priority_type}_{priority_growth_rate}"
      )
    ) %>% 
    select(- priority_growth_rate_num,
           -priority_type,
           -priority_growth_rate)
  
  df %>% 
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
             .after = tracing_delay)
  
}
