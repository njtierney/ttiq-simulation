#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param derived_delay_distributions
#' @return
#' @author Nicholas Tierney
#' @export
parameters_test_turnaround_time <- function(derived_delay_distributions) {

  df <- derived_delay_distributions %>% 
    select(scenario,
           dist_time_to_interview) 
  
  parameters(df$dist_time_to_interview[[1]])
    mutate(parameters = parameters(dist_time_to_interview))
  parameters(derived_delay_distributions$dist_test_turnaround_time[[1]])

}
