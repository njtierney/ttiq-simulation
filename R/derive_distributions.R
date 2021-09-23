#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_scenario
#' @return
#' @author Nicholas Tierney
#' @export
derive_distributions <- function(cases_scenario,
                                 prop_current_case_zero = 0.5) {
  
  df_distributions <- cases_scenario %>% 
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
    # check: are we going to get rid of this dist_poisson step?
    mutate(dist_isol_swab = dist_poisson(1),
           .before = dist_test_turnaround_time)
  
  # rename isol_swab to "other"
  
  current_case <- df_distributions %>% 
    # check: should this part of adding things to current case
    # be put into another step outside of this function?
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
