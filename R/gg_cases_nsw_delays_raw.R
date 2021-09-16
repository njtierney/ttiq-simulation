#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw
#' @return
#' @author Nicholas Tierney
#' @export
gg_cases_nsw_delays_raw <- function(cases_nsw_raw_delay_long) {

  ggplot(cases_nsw_raw_delay_long,
         aes(x = days)) +
    geom_bar() +
    facet_wrap(~delay_type) + 
    xlim(c(-14, 14))

}
