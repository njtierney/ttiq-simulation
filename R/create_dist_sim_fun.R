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
      sim_tracing_fun = list(get_sim_fun(dist_test_turnaround_time, 
                                 dist_time_to_interview))
    ) %>% 
    select(scenario,
           sim_tracing_fun)

}
