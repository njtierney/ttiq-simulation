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
  
  
  # make sure we get a prob for each day even if the sample lacked them
  bins <- 0:20
  breaks <- c(bins, max(bins) + 1)- 0.5
  
  hist_pmf <- function(x, 
                       bins = 0:20,
                       breaks = c(bins, max(bins) + 1)- 0.5){
    hist_info <- hist(x, breaks = breaks, plot = FALSE)
    hist_info$density
  }
  
  x_pmf <- hist_pmf(x)
  
  return(pmf = list(x_pmf))
  # 
  # # prepare the empirical data part
  # delay_days <- cases_scenario %>% 
  #   group_by(scenario) %>%
  #   summarise(
  #     pmf_test_turnaround_time = hist_pmf(test_turnaround_time, 
  #                                         breaks = breaks),
  #     pmf_time_to_interview = hist_pmf(time_to_interview, 
  #                                      breaks = breaks),
  #     pmf_full_contact_delay = hist_pmf(full_contact_delay, 
  #                                       breaks = breaks),
  #     pmf_test_to_interview = hist_pmf(test_to_interview,
  #                                      breaks = breaks)) %>% 
  #   mutate(
  #     days = bins,
  #     fraction_extra_zero = 0) %>% 
  #   relocate(days, .before = pmf_test_turnaround_time)
    
}
