#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param trace_run
#' @return
#' @author Nicholas Tierney
#' @export
gg_hist_tti <- function(trace_run) {

  
  tibble(
    time_from_infect_to_iso = c(trace_run)
    ) %>% 
    ggplot(aes(x = time_from_infect_to_iso)) +
    geom_histogram(
      bins = 50,
      colour = "white",
      fill = "grey"
    ) +
    labs(
      x = "Time from infection to isolation"
    ) +
    theme_minimal()

}
