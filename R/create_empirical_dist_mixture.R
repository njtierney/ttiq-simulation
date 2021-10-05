#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_scenario
#' @return
#' @author David Duncan
#' @export
derive_empirical_dist_mixture <- function(cases_scenario, 
                                          fraction_extra_zero,
                                          n_samples) {
  
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
  delay_params <- data.frame(
    days = bins,
    pmf = counts$density
  )   
  
  sim_delay <- function(n_samples, delay_params, fraction_extra_zero = 0) {

    sim_basic <- sample(
      x = delay_params$days,
      size = n_samples,
      prob = delay_params$pmf,
      replace = TRUE
    )
    
    sim_nonzeros <- rbinom(
      n_samples,
      1,
      prob = 1 - fraction_extra_zero
    )
    
    # return distribution object
    sim_basic * sim_nonzeros
    
  }
  # return distribution object
  sim_delay(n_samples , 
            delay_params = delay_params, 
            fraction_extra_zero = fraction_extra_zero)
}
