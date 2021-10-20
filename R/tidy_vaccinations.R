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

  vaccinations_enriched <- vaccinations_raw %>% 
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
             .after = time_dose_1) %>% 
    pivot_longer(
      cols = c(
        time_dose_1,
        time_dose_2
        ),
      names_to = "dose",
      values_to = "date",
      names_prefix = "time_dose_"
    ) %>% 
    relocate(num_people,
             .after = everything()) %>% 
    group_by(sa4_code16,
             age_band_id,
             vaccine,
             dose,
             date) %>% 
    summarise(num_people = sum(num_people),
              .groups = "drop")
  
  aggregated_short <- aggregated_populations %>% 
    select(sa4_code16,
           vac_age_group,
           population)
  
  vaccinations_with_population <- vaccinations_enriched %>% 
    left_join(aggregated_short,
              by = c("sa4_code16",
                     "age_band_id" = "vac_age_group"))  %>% 
    group_by(age_band_id,
             vaccine,
             dose,
             date) %>% 
    summarise(num_people = sum(num_people),
              population = sum(population),
              .groups = "drop")
  
  # still need to add on the vaccination 0-11 age group
    # expand_grid(
    #   sa4_code16 = unique(vaccinations_enriched$sa4_code16),
    #   date = unique(vaccinations_with_population$date),
    #   vaccine = unique(vaccinations_with_population$vaccine),
    #   dose = unique(vaccinations_with_population$dose)
    # ) %>% 
    #   mutate(vac_age_group = "0-11") %>% 
    #   left_join({
    #     aggregated_short %>% 
    #       filter(vac_age_group == "0-11")
    #   },
    #   by = c("sa4_code16",
    #          "vac_age_group")
    #   ) %>% 
    #   distinct() %>% 
    #   group_by(date,
    #            vaccine,
    #            dose,
    #            vac_age_group) %>% 
    #   summarise(population = sum(population))
    #   mutate(num_people = 0) 
  
  vaccinations_with_population
  
  
  
  x# unique(partial_df$age_band_id)
  # populations
  
  

}
