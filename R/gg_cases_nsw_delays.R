#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_delays
#' @return
#' @author Nicholas Tierney
#' @export
gg_cases_nsw_delays <- function(cases_nsw_delays_long) {

  
  ggplot(aes(x = delay_type,
             y = days)) + 
    geom_boxplot()

}
