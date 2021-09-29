#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delays
#' @return
#' @author Nicholas Tierney
#' @export
gg_nsw_delays_hist <- function(nsw_delays) {

  nsw_delays %>%
    filter(
      date_detection < as_date("2021-02-01")
    ) %>%
    ggplot(
      aes(
        x = time_to_isolation
      )
    ) +
    geom_histogram(
      binwidth = 1,
      color = "white"
    )
  

}
