#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_scenario
#' @return
#' @author David Duncan
#' @export
create_empirical_dist_mixture <- function(cases_scenario) {
  
  # prepare the empirical data part
  delay_days <- cases_scenario %>% 
    filter(scenario == "current_nsw") %>% 
    select(time_to_interview) %>% 
    mutate(days = time_to_interview) %>% 
    drop_na
  
  # make sure we get a prob for each day even if the sample lacked them
  bins <- 0:20
  breaks <- c(bins, max(bins) + 1)- 0.5
  counts <- hist(delay_days$days, breaks = breaks, plot = FALSE)
  delay_params <- list(
    days = bins,
    pmf = counts$density,
    fraction_extra_zero = 0
  )   
}
