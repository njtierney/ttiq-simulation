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

  # when does it hit 70?
  vaccination_coverage %>% 
    arrange(time_dose_2) %>% 
    group_by(age_band_id) %>% 
    mutate(milestone = case_when(
      cumulative_prop_vac >= 0.9 ~ "over_90_pct",
      cumulative_prop_vac >= 0.8 ~ "over_80_pct",
      cumulative_prop_vac >= 0.7 ~ "over_70_pct",
      TRUE ~ NA_character_
      )
    ) %>% 
    filter(!is.na(milestone)) %>% 
    group_by(age_band_id,
             milestone) %>% 
    slice(1) %>% 
    select(age_band_id,
           time_dose_2,
           cumulative_prop_vac,
           milestone) %>% 
    arrange(milestone,
            age_band_id)
}
