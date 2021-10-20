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
           population) %>% 
    distinct()
  
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
  
  return(vaccinations_with_population)
  
  # # Adding on the vaccination 0-11 age group
  # TODO
  # currently the numbers don't quite add up, they are 
  # not collapsing over the SA4 group properley
  # # Create all combinations of SA4, date, vaccine, and dose
  df_younglings <- expand_grid(
    sa4_code16 = unique(vaccinations_enriched$sa4_code16),
    date = unique(vaccinations_with_population$date),
    vaccine = unique(vaccinations_with_population$vaccine),
    dose = unique(vaccinations_with_population$dose)
  ) %>% 
  # add on the "0-11" group
    mutate(vac_age_group = "0-11") %>%
  # then join on the aggregated 0-11 population data
    left_join({
      aggregated_short %>%
        filter(vac_age_group == "0-11")
    },
    by = c("sa4_code16",
           "vac_age_group")
    ) %>% 
    mutate(num_people = 0)
  
  df_younglings %>% 
    group_by(date,
             vaccine,
             dose,
             vac_age_group) %>% 
    mutate(num_people = sum(num_people)) %>% 
    ungroup() %>% 
    select(-sa4_code16) %>% 
    distinct()
  
  head(df_younglings)
  tail(df_younglings)
  
  
  
  
  
  x# unique(partial_df$age_band_id)
  # populations
  
  

}
