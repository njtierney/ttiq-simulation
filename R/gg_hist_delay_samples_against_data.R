#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param prepared_cases_for_plots
#' @return
#' @author Nicholas Tierney
#' @export
gg_hist_delay_samples_against_data <- function(
  prepared_cases_for_plots
  ) {

  # plot these against the data
  # also plot them as an ecdf
  # we wanted to know the "all" (total delay time) distribution looks the same 
  # as the expected all, which is the sum of 1 and 2
  
  prepared_cases_for_plots %>%
    ggplot() + 
    # Don't go to positive/negative infinity
    geom_col(
      aes(x = days,
          y = fraction),
      fill = grey(0.5),
      data = filter(prepared_cases_for_plots,
                    data_type == "model")
    ) + 
    geom_point(
      aes(x = days,
          y = fraction),
      shape = 16,
      data = filter(prepared_cases_for_plots,
                    data_type == "data",
                    fraction > 0.005)
    ) + 
    facet_grid(scenario ~ delay_type) +
    theme_bw() +
    theme(aspect.ratio = 1,
          strip.background = element_blank()) + 
    scale_colour_brewer(palette = "Dark2") + 
    labs(
      title = "Assumed contact tracing delays (dots = data)",
      x = "Days",
      y = "Proportion of cases with delay",
      colour = ""
    )

}
