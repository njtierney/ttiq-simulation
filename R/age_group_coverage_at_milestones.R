#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage
#' @param vaccination_coverage_milestones
#' @return
#' @author Nicholas Tierney
#' @export
age_group_coverage_at_milestones <- function(vaccination_coverage,
                                             vaccination_coverage_milestones) {

  vaccination_coverage_milestones_12plus <- vaccination_coverage_milestones %>% 
    filter(age_cutoff == "12+") 
  
  vaccination_coverage_milestones_16plus <- vaccination_coverage_milestones %>% 
    filter(age_cutoff == "16+") 
  
  vaccination_coverage_milestones_12plus_70_pct <- 
    vaccination_coverage_milestones_12plus %>% 
    filter(milestone == "over_70_pct") 
  
  vaccination_coverage_milestones_12plus_80_pct <- 
    vaccination_coverage_milestones_12plus %>% 
    filter(milestone == "over_80_pct") 
  
  vaccination_coverage_milestones_16plus_70_pct <- 
    vaccination_coverage_milestones_16plus %>% 
    filter(milestone == "over_70_pct") 
  
  vaccination_coverage_milestones_16plus_80_pct <- 
    vaccination_coverage_milestones_16plus %>% 
    filter(milestone == "over_80_pct") 
  
  filter_to_milestone_time <- function(vaccination_coverage,
                                       milestone_point){
    vaccination_coverage %>% 
    filter(
      time_dose_2 %in% milestone_point[["time_dose_2"]]
    ) %>% 
      arrange(time_dose_2) %>% 
      group_by(age_band_id) %>% 
      slice(1)
  }
  
  age_coverage_at_milestone_12plus_70pct <- filter_to_milestone_time(
    vaccination_coverage,
    vaccination_coverage_milestones_12plus_70_pct
  )
  
  age_coverage_at_milestone_12plus_80pct <- filter_to_milestone_time(
    vaccination_coverage,
    vaccination_coverage_milestones_12plus_80_pct
  )
  
  age_coverage_at_milestone_16plus_70_pct <- filter_to_milestone_time(
    vaccination_coverage,
    vaccination_coverage_milestones_16plus_70_pct
  )
  
  age_coverage_at_milestone_16plus_80_pct <- filter_to_milestone_time(
    vaccination_coverage,
    vaccination_coverage_milestones_16plus_80_pct
  )
  
  bind_rows(
    "12_plus_70_pct" = age_coverage_at_milestone_12plus_70pct,
    "12_plus_80_pct" = age_coverage_at_milestone_12plus_80pct,
    "16_plus_70_pct" = age_coverage_at_milestone_16plus_70_pct,
    "16_plus_80_pct" = age_coverage_at_milestone_16plus_80_pct,
    .id = "age_vacc_category"
  )

}
