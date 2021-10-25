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
                              aggregated_populations,
                              sa4_lookup) {

  vaccinations_enriched <-
  
    vaccinations_raw %>% 
    # add on whether vaccine as AstraZenze, Pfizer, or Moderna
    left_join(dim_vaccine,
              by = "vaccine") %>% 
    select(-vaccine) %>% 
    rename(vaccine = name) %>% 
    relocate(vaccine,
             .after = age_band_id) %>% 
  # recode moderna as pfizer, as they are treated the same in the model
    mutate(vaccine = case_when(vaccine == "Moderna" ~ "Pfizer",
                               TRUE ~ vaccine)) %>% 
    # convert age bands from 1...9, into 12-15, ... 80+
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
    relocate(time_dose_1, .after = vaccine) %>% 
    left_join(
      dim_time,
      by = c("time_dose_2" = "time")
    ) %>% 
    select(-time_dose_2) %>% 
    rename(time_dose_2 = week_starting) %>% 
    relocate(time_dose_2, .after = time_dose_1) %>% 
    group_by(age_band_id,
             vaccine,
             time_dose_1,
             time_dose_2) %>% 
    summarise(
      num_people = sum(num_people),
      .groups = "drop"
    ) %>% 
    add_row(age_band_id = "0-11",
            vaccine = "Pfizer",
            time_dose_1 = as_date("2020-12-14"),
            time_dose_2 = as_date("2021-02-22"),
            num_people = 0,
            .before = 1) %>% 
    complete(
      age_band_id,
      vaccine,
      time_dose_1,
      time_dose_2,
      fill = list(num_people = 0)
    ) 
  
  population_age <- aggregated_populations %>%
    distinct(vac_age_group,
             population) %>% 
    relocate(vac_age_group,
             population) 
  
  vaccinations_with_population <- vaccinations_enriched %>% 
    left_join(population_age,
              by = c("age_band_id" = "vac_age_group")) %>% 
    rename(n_vaccinated = num_people)
  
  ## population sums to australia
  # oz_pop <- vaccinations_with_population %>% 
  #   distinct(age_band_id,
  #            population) %>% 
  #   pull(population) %>% 
  #   sum() %>% 
  #   comma()
  # oz_pop
  
  
  vaccinations_with_population
  
}
