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
        .fns = create_empirical_dist,
        .names = "dist_{.col}"
      )
    ) %>% 
    # TODO: modify this so we can inflate the zeros
    # 
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
    mutate(
      dist_time_to_interview = dist_inflated(
        dist = dist_time_to_interview, 
        prob = prop_current_case_zero,
        x = 0
        )
    )
  
  bind_rows(
    df_distributions,
    current_case
  ) %>% 
    mutate(scenario = as.character(glue("{scenario}_{prop_current_case_zero}")),
           scenario = str_remove_all(scenario,
                                     "_NA"))
  
}
