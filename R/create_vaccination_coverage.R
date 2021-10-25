#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccinations
#' @return
#' @author Nicholas Tierney
#' @export
create_vaccination_coverage <- function(vaccinations) {

  vaccination_coverage <- vaccinations %>% 
    pivot_wider(
      names_from = vaccine,
      values_from = n_vaccinated
    ) %>% 
    mutate(
      n_vaccinated = AstraZeneca + Pfizer
    ) %>% 
    select(-AstraZeneca,
           -Pfizer) %>% 
    relocate(population,
             .after = everything()) %>% 
    group_by(age_band_id,
             time_dose_2) %>% 
    summarise(n_vaccinated = sum(n_vaccinated),
              population = first(population)) %>% 
    arrange(time_dose_2,
            .by_group = TRUE) %>% 
    mutate(cumulative_n_vac = cumsum(n_vaccinated),
           cumulative_prop_vac = cumulative_n_vac / population) %>% 
    ungroup() 
  
  vaccination_coverage

}
