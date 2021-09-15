#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw
#' @return
#' @author Nicholas Tierney
#' @export
cases_nsw_longer <- function(cases_nsw_delays) {

    cases_nsw_delays %>% 
    pivot_longer(
      cols = test_turnaround_time:test_to_interview,
      names_to = "delay_type",
      values_to = "days"
    ) 

}
