#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @return
#' @author Nicholas Tierney
#' @export
cases_delay_longer <- function(cases) {

  cases %>% 
    select(test_turnaround_time,
           time_to_interview,
           full_contact_delay,
           test_to_interview) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "delay_type",
      values_to = "days"
    ) 

}
