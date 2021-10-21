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

  vaccinations %>% 
    group_by(age_band_id,
             vaccine,
             time_dose_1,
             time_dose_2) %>% 
    summarise(population = sum(population))
  
  vaccination_coverage <- vaccinations %>% 
    group_by(ste_name16,
             age_band_id,
             vaccine) %>% 
    arrange(time_dose_2) %>% 
    mutate(cumulative_n_vac = cumsum(n_vaccinated),
           cumulative_prop_vac = cumulative_n_vac / population) %>% 
    ungroup() 
  
    ggplot(vaccination_coverage,
           aes(x = time_dose_2,
               y = cumulative_prop_vac,
               colour = ste_name16)) + 
    geom_line() +
    facet_grid(age_band_id~vaccine,
               scales = "free") 

}
