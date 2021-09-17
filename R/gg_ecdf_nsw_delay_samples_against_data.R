#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples_against_data
#' @return
#' @author Nicholas Tierney
#' @export
gg_ecdf_nsw_delay_samples_against_data <- function(
  prepared_cases_for_plots
  ) {

  # plot these against the data
  # also plot them as an ecdf
  # we wanted to know the "all" (total delay time) distribution looks the same 
  # as the expected all, which is the sum of 1 and 2
  
  
  ggplot(prepared_cases_for_plots,
         aes(x = days,
             colour = data_type)) + 
    # Don't go to positive/negative infinity
    stat_ecdf(geom = "step", pad = FALSE) + 
    facet_grid(scenario ~ delay_type) +
    theme_bw() +
    theme(aspect.ratio = 1) + 
    scale_colour_brewer(palette = "Dark2") + 
    labs(
      title = "Assumed contact tracing delays for model and data",
      x = "Days",
      y = "Proportion processed by",
      colour = ""
    )
         

}
