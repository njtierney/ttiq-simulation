#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccinations_raw
#' @return
#' @author Nicholas Tierney
#' @export
tidy_vaccinations <- function(vaccinations_raw,
                              dim_age_band,
                              dim_time,
                              dim_vaccine,
                              aggregated_populations) {

  partial_df <- vaccinations_raw %>% 
    left_join(dim_vaccine,
              by = "vaccine") %>% 
    select(-vaccine) %>% 
    rename(vaccine = name) %>% 
    relocate(vaccine,
             .after = age_band_id) %>% 
    left_join(dim_age_band,
              by = "age_band_id")  %>% 
    select(-age_band_id) %>% 
    rename(age_band_id = age_band) %>% 
    relocate(age_band_id,
             .before = vaccine) %>% 
    left_join(
      dim_time,
      by = c("time_dose_1" = "time")
    ) %>% 
    select(-time_dose_1) %>% 
    rename(time_dose_1 = week_starting) %>% 
    relocate(time_dose_1, 
             .after = vaccine) %>% 
    left_join(
      dim_time,
      by = c("time_dose_2" = "time")
    ) %>% 
    select(-time_dose_2) %>% 
    rename(time_dose_2 = week_starting) %>% 
    relocate(time_dose_2, 
             .after = time_dose_1)
  
  sort(unique(partial_df$age_band_id))
  sort(unique(aggregated_populations$vac_age_group))
  
  partial_df %>% 
    left_join(select(aggregated_populations,
                     ,
              by = c("sa4_code16",
                     "age_band_id" = "vac_age_group"))
  
  x# unique(partial_df$age_band_id)
  # populations
  
  

}
