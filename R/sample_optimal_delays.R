#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param derived_delay_distributions
#' @return
#' @author Nicholas Tierney
#' @export
sample_optimal_delays <- function(derived_delay_distributions,
                                   n_sims = 1000) {

  optimal_nsw <- derived_delay_distributions %>% 
    filter(scenario == "optimal") %>% 
    select(scenario,
           dist_test_turnaround_time,
           dist_time_to_interview)
  
  optimal_densities <- optimal_nsw %>% 
    expand_grid(
      days = 0:20
    ) %>% 
    mutate(
      time_to_swab = dpois(days, 0.5),
      # from optimal
      test_turnaround_time = density(dist_test_turnaround_time, days)[[1]],
      time_to_interview = density(dist_time_to_interview, days)[[1]],
      time_to_isolation = dpois(days, 0.5)
    )
  
  optimal_densities %>% 
    # sample independently from the delays
    summarise(
      across(
        .cols = c(
          time_to_swab,
          test_turnaround_time,
          time_to_interview,
          time_to_isolation
        ),
        ~sample(days, n_sims, replace = TRUE, prob = .x)
      )
    )
  

}
