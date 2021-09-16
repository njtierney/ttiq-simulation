#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param derive_nsw_delay_distributions
#' @return
#' @author Nicholas Tierney
#' @export
create_dist_sim_fun <- function(derive_nsw_delay_distributions) {

  
  derive_nsw_delay_distributions %>%
    mutate(
      sim_tracing_fun = map2(.x = dist_test_turnaround_time, 
                             .y = dist_time_to_interview,
                             .f = ~get_sim_fun(.x, .y))
    ) %>% 
    select(scenario,
           sim_tracing_fun)

}
