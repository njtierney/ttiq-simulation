#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_vacc_adjusted_clinical_fraction
#' @return
#' @author dhduncan
#' @export
gg_adjusted_clinical_fraction <- function(age_vacc_adjusted_clinical_fraction) {

  df <- tibble(age_group = rep(pull(age_classes()[1]), 2),
               vaccine_status = rep(c("none", "full complement\n(of any)"), 
                                    each = nrow(age_classes())),
               adj_clinical_fraction = age_vacc_adjusted_clinical_fraction)
  
  ggplot(df,
         aes(x = fct_inorder(age_group), 
             y = adj_clinical_fraction, 
             fill = fct_inorder(vaccine_status))
         ) +
    geom_col() +
    facet_wrap(~vaccine_status) + 
    labs(x = "Age group",
         y = "Clinical fraction of infections",
         fill = "Vaccination status")

}
