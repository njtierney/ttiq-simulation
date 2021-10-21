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
    group_by(age_band_id,
             vaccine,
             dose) %>% 
    arrange(date) %>% 
    mutate(cumulative_n_vac = cumsum(n_vaccinated),
           cumulative_prop_vac = cumulative_n_vac / population) %>% 
    ungroup() 
  
  vaccination_coverage
  
    ggplot(vaccination_coverage,
           aes(x = date,
               y = cumulative_prop_vac,
               colour = dose)) + 
    geom_line() +
    scale_colour_brewer(palette = "Dark2") + 
    facet_grid(age_band_id~vaccine,
               scales = "free") + 
    scale_y_continuous(labels = label_percent()) + 
    geom_hline(yintercept = 1, colour = "orange")

}
