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
    # pivot the time components
    pivot_longer(
      cols = c(
        time_dose_1,
        time_dose_2
      ),
      names_to = "dose",
      values_to = "date",
      names_prefix = "time_dose_"
    ) %>% 
    # do replace the time components
    left_join(
      dim_time,
      by = c("date" = "time")
    ) %>% 
    select(-date) %>% 
    rename(date = week_starting) %>% 
    relocate(num_people,
             .after = everything())
  
  vaccinations_enriched
  
  vaccinations_resummed <- vaccinations_enriched %>% 
    group_by(sa4_code16,
             age_band_id,
             vaccine,
             dose,
             date) %>% 
    summarise(num_people = sum(num_people),
              .groups = "drop")
  
  # aggregated_short <- aggregated_populations %>% 
  population_sa4_age <- aggregated_populations %>%
    select(sa4_code16,
           vac_age_group,
           population) %>% 
    distinct()
  
  vaccinations_with_population <- vaccinations_resummed %>% 
    left_join(population_sa4_age,
              by = c("sa4_code16",
                     "age_band_id" = "vac_age_group"))  %>% 
    group_by(age_band_id,
             vaccine,
             dose,
             date) %>% 
    summarise(num_people = sum(num_people),
              population = sum(population),
              .groups = "drop")
  
  
  # # Adding on the vaccination 0-11 age group
  # TODO
  # currently the numbers don't quite add up, they are 
  # not collapsing over the SA4 group properley
  # # Create all combinations of SA4, date, vaccine, and dose
  df_younglings <- expand_grid(
    sa4_code16 = unique(vaccinations_enriched$sa4_code16),
    date = unique(vaccinations_with_population$date),
    vaccine = unique(vaccinations_with_population$vaccine),
    dose = unique(vaccinations_with_population$dose),
  # add on the "0-11" group
    vac_age_group = "0-11"
  ) %>% 
  # then join on the aggregated 0-11 population data
    left_join({
      population_sa4_age %>%
        filter(vac_age_group == "0-11")
    },
    by = c("sa4_code16",
           "vac_age_group")
    ) %>% 
    mutate(num_people = 0) %>% 
    group_by(date,
             vaccine,
             dose,
             vac_age_group) %>% 
    summarise(num_people = sum(num_people),
              population = sum(population),
              .groups = "drop")  %>% 
    distinct() %>% 
    relocate(vac_age_group,
             vaccine,
             dose,
             date,
             num_people,
             population) %>% 
    rename(age_band_id = vac_age_group)
  
  bind_rows(
    df_younglings,
    vaccinations_with_population
  ) %>% 
    rename(n_vaccinated = num_people)
  

}
