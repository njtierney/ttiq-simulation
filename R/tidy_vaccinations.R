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

  vaccinations_enriched <- vaccinations_raw %>% 
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
    left_join(
      select(sa4_lookup, sa4_code16, STE_NAME16),
      by = "sa4_code16"
    ) %>% 
    rename(ste_name16 = STE_NAME16)
  
  # vaccinations_resummed <- vaccinations_enriched %>% 
  #   group_by(sa4_code16,
  #            age_band_id,
  #            vaccine) %>% 
  #   summarise(num_people = sum(num_people),
  #             .groups = "drop")
  
  # aggregated_short <- aggregated_populations %>% 
  
  population_state_age <- aggregated_populations %>%
    distinct(ste_name16,
             vac_age_group,
             population)
  
  vaccinations_with_population <- vaccinations_enriched %>% 
    left_join(population_state_age,
              by = c("ste_name16",
                     "age_band_id" = "vac_age_group"))  %>% 
    select(-sa4_code16) %>% 
    distinct() %>% 
    relocate(ste_name16,
             .before = everything())
  
  # # Adding on the vaccination 0-11 age group
  # TODO
  # currently the numbers don't quite add up, they are 
  # not collapsing over the SA4 group properley
  # # Create all combinations of SA4, date, vaccine, and dose
  df_younglings <- expand_grid(
    ste_name16 = unique(vaccinations_enriched$ste_name16),
    vaccine = unique(vaccinations_with_population$vaccine),
    time_dose_1 = unique(vaccinations_with_population$time_dose_1),
    time_dose_2 = unique(vaccinations_with_population$time_dose_2),
  # add on the "0-11" group
    vac_age_group = "0-11"
  ) %>% 
  # then join on the aggregated 0-11 population data
    left_join({
      population_state_age %>%
        filter(vac_age_group == "0-11")
    },
    by = c("ste_name16",
           "vac_age_group")
    ) %>% 
    mutate(num_people = 0) %>% 
    relocate(ste_name16,
             vac_age_group,
             vaccine,
             time_dose_1,
             time_dose_2,
             num_people,
             population) %>% 
    rename(age_band_id = vac_age_group)
  
  bind_rows(
    df_younglings,
    vaccinations_with_population
  ) %>% 
    rename(n_vaccinated = num_people)
  

}
