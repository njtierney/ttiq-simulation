#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_delays
#' @return
#' @author Nicholas Tierney
#' @export
derive_distributions <- function(cases_nsw_delays,
                                 prop_current_case_zero = 0.5) {
  
  df_distributions <- cases_nsw_delays %>% 
    filter(scenario != "outside") %>% 
    group_by(scenario) %>% 
    summarise(
      across(
        .cols = c(
          test_turnaround_time,
          time_to_interview,
          # full_contact_delay
        ),
        .fns = derive_poisson_mixture,
        .names = "dist_{.col}"
      )
    ) %>% 
    mutate(dist_isol_swab = dist_poisson(1),
           .before = dist_test_turnaround_time)
  
  # rename isol_swab to "other"
  
  current_case <- df_distributions %>% 
    filter(scenario == "current") %>% 
    mutate(scenario = "current_case_init") %>% 
    mutate(dist_time_to_interview = dist_mixture(
      dist_uniform(0,0),
      dist_time_to_interview,
      weights = c(prop_current_case_zero, 1 - prop_current_case_zero) 
    ))
  
  bind_rows(
    df_distributions,
    current_case
  )
  
}
