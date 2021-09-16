#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw
#' @return
#' @author Nicholas Tierney
#' @export
cases_nsw_delay_raw_longer <- function(cases_nsw) {

   cases_nsw %>% 
    select(full_contact_delay,
           time_to_interview,
           test_turnaround_time) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "delay_type",
      values_to = "days"
    )

}
