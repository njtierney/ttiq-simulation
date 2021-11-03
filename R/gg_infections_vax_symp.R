#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param oz_baseline_matrix
#' @param ve_onward_transmission
#' @param ve_susceptibility
#' @param ve_symptoms
#' @param detection_asymptomatic
#' @param detection_symptomatic
#' @param vaccination_coverage
#' @param vaccination_age_min
#' @return
#' @author Nick Golding
#' @export
gg_infections_vax_symp <- function(prepared_infections_vax_symp) {

  prepared_infections_vax_symp %>%
    mutate(
      milestone = str_remove_all(milestone, "over_16_"),
      milestone = str_remove_all(milestone, "over_"),
      milestone = str_replace(milestone, "_pct", "%"),
      milestone = str_replace(milestone, "terminal", "90%"),
    ) %>%
    ggplot(
      aes(
        x = milestone,
        y = fraction, 
        fill = status
      )
    ) + 
      scale_fill_manual(
        values = c(
          darken("#990000", 0.2), 
          lighten("#990000", 0.3), 
          darken("#000099", 0.2), 
          lighten("#000099", 0.3)
        )
      ) +
    geom_col() +
    facet_grid(~which) +
    labs(
      x = "\nVaccination coverage milestones",
      y = "Fraction",
      fill = "",
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 12)
    ) 
  
}
