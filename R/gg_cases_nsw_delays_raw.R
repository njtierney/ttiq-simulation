#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw
#' @return
#' @author Nicholas Tierney
#' @export
gg_cases_delays <- function(cases_longer) {

  ggplot(cases_longer,
         aes(x = days)) +
    geom_bar() +
    facet_wrap(~delay_type) + 
    xlim(c(-14, 14))

}
