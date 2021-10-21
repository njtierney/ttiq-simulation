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
  
  calculate_milestones <- function(vaccination_coverage){
    vaccination_coverage %>% 
    group_by(time_dose_1,
             time_dose_2) %>% 
      summarise(n_vaccinated = sum(n_vaccinated),
                population = sum(population),
                .groups = "drop") %>% 
      arrange(time_dose_2) %>% 
      mutate(cumulative_n_vac = cumsum(n_vaccinated),
             cumulative_prop_vac = cumulative_n_vac / population) %>% 
      mutate(milestone = case_when(
        cumulative_prop_vac >= 0.895 ~ "over_90_pct",
        cumulative_prop_vac >= 0.795 ~ "over_80_pct",
        cumulative_prop_vac >= 0.695 ~ "over_70_pct",
        TRUE ~ NA_character_
      )
      ) %>% 
      filter(!is.na(milestone)) %>% 
      group_by(milestone) %>% 
      arrange(time_dose_2) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(time_dose_1,
             time_dose_2,
             population,
             cumulative_n_vac,
             cumulative_prop_vac,
             milestone)
  }

  overall_population_coverage_12_plus <- vaccination_coverage %>% 
    filter(age_band_id != "0-11") %>% 
    calculate_milestones()
  
  overall_population_coverage_16_plus <- vaccination_coverage %>% 
    filter(age_band_id != "0-11",
           age_band_id != "12-15")  %>% 
    calculate_milestones()
    
  bind_rows(
    "12+" = overall_population_coverage_12_plus,
    "16+" = overall_population_coverage_16_plus,
    .id = "age_cutoff"
  )
    
    
}
