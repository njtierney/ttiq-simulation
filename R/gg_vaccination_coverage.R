#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage
#' @return
#' @author Nicholas Tierney
#' @export
gg_vaccination_coverage <- function(vaccination_coverage) {

  ggplot(vaccination_coverage,
         aes(x = time_dose_2,
             y = cumulative_prop_vac)) + 
    geom_line() +
    facet_wrap(~age_band_id) + 
    scale_y_continuous(labels = label_percent())  +
    scale_x_date(date_labels = "%x",
                 date_breaks = "4 months")

}
