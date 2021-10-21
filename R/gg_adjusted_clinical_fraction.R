#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_vacc_adjusted_cases
#' @return
#' @author dhduncan
#' @export
gg_adjusted_clinical_fraction <- function(age_vacc_adjusted_cases) {

  df <- tibble(
    age_group = rep(
      pull(age_classes()[1]),
      2
    ),
    vaccine_status = rep(
      c("none", "full complement\n(of any)"), 
      each = nrow(age_classes())
    ),
    adj_clinical_fraction = age_vacc_adjusted_cases
  ) %>% 
    mutate(
      vaccine_status = factor(
        vaccine_status, 
        levels = c("none", "full complement\n(of any)"),
        ordered = TRUE
      )
    )
  
  ggplot(test,
         aes(x = fct_inorder(age_group), 
             y = adj_clinical_fraction, 
             fill = vaccine_status)
         ) +
    scale_fill_discrete() +
    geom_col() +
    facet_grid(vaccine_coverage~vaccine_status) + 
    labs(x = "Age group",
         y = "Clinical fraction of infections",
         fill = "Vaccination status")

}
