#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param prepared_infections_vax_symp
#' @return
#' @author Nicholas Tierney
#' @export
gg_infections_vax_symp_infections_only <- function(prepared_infections_vax_symp) {

  df <- prepared_infections_vax_symp %>% 
    filter(which == "All infections")
  
  ggplot(
    data = df,
    aes(
      x = vaccination_coverage_percent,
      y = fraction, 
      fill = status
    )
  ) + 
    scale_fill_manual(values = c(darken("#990000", 0.2), 
                                 lighten("#990000", 0.3), 
                                 darken("#000099", 0.2), 
                                 lighten("#000099", 0.3))) +
    geom_col() +
    labs(
      title = "All Infections",
      x = "\nVaccination coverage (uniform across 12+ population)",
      y = "Fraction",
      fill = "",
      alpha = "Symptoms"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    ) 
  

}
