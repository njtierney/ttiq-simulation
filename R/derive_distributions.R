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
                                 prop_current_case_zero) {
  
  df_distributions <- cases_scenario %>% 
    group_by(scenario) %>% 
    summarise(
      across(
        .cols = c(
          test_turnaround_time,
          time_to_interview,
          # full_contact_delay
        ),
        .fns = create_empirical_dist_mixture,
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
    filter(str_detect(scenario, "current")) %>% 
    mutate(scenario = paste0(scenario, "_case_init")) %>% 
    expand_grid(prop_current_case_zero = prop_current_case_zero) %>% 
    relocate(prop_current_case_zero, 
             .after = scenario) %>% 
    group_by(scenario,
             prop_current_case_zero) %>%
    
    # new changes to establishing distributions means that we 
    # need to consider how to do this step below
    # mutate(
    #   dist_time_to_interview = set_fraction_zero(
    #     dist_time_to_interview,
    #     fraction_extra_zero
    #   )
    # )
    
    mutate(dist_time_to_interview = dist_mixture(
      dist_degenerate(0),
      dist_time_to_interview,
      weights = c(prop_current_case_zero, 1 - prop_current_case_zero) 
    ))
  
  bind_rows(
    df_distributions,
    current_case
  )
  
}
