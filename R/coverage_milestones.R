#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage
#' @return
#' @author Nicholas Tierney
#' @export
coverage_milestones <- function(vaccination_coverage) {

  overall_population_coverage <- vaccination_coverage %>% 
    group_by(time_dose_1,
             time_dose_2) %>% 
    summarise(n_vaccinated = sum(n_vaccinated),
              population = sum(population),
              .groups = "drop") %>% 
    arrange(time_dose_2) %>% 
    mutate(cumulative_n_vac = cumsum(n_vaccinated),
           cumulative_prop_vac = cumulative_n_vac / population) %>% 
    mutate(milestone = case_when(
      cumulative_prop_vac >= 0.9 ~ "over_90_pct",
      cumulative_prop_vac >= 0.8 ~ "over_80_pct",
      cumulative_prop_vac >= 0.7 ~ "over_70_pct",
      TRUE ~ NA_character_
      )
    ) %>% 
    filter(!is.na(milestone)) %>% 
    group_by(milestone) %>% 
    arrange(time_dose_2) %>% 
    slice(1) %>% 
    ungroup()
  
  age_coverage_at_milestone <- vaccination_coverage %>% 
    filter(time_dose_2 %in% overall_population_coverage$time_dose_2) %>% 
    arrange(time_dose_2) %>% 
    group_by(age_band_id) %>% 
    slice(1)
  
  age_coverage_at_milestone
  overall_population_coverage
    
    
}
