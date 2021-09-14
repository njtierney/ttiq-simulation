#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_delays
#' @return
#' @author Nicholas Tierney
#' @export
calculate_distributions <- function(cases_nsw_delays) {
  
  cases_nsw_delays %>% 
    group_by(period) %>% 
    summarise(
      across(
        .cols = c(
          test_turnaround_time,
          time_to_interview,
          full_contact_delay
        ),
        .fns = derive_poisson_mixture
      )
    )
  
}
