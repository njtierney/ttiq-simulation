#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param delay_dist_funs
#' @param n_samples
#' @return
#' @author Nicholas Tierney
#' @export
simulate_test_turnaround_time <- function(derived_delay_distributions,
                                          n_samples = 10000) {

  derived_delay_distributions %>% 
    select(scenario,
           dist_test_turnaround_time) %>% 
    mutate(sampled_test_turnaround_time = generate(dist_test_turnaround_time,
                           n_samples)) %>% 
    unnest(cols = sampled_test_turnaround_time) %>% 
    select(scenario, 
           sampled_test_turnaround_time)

}
