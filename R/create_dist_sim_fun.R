#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param derive_nsw_delay_distributions
#' @return
#' @author Nicholas Tierney
#' @export
create_dist_sim_fun <- function(derived_delay_distributions) {

  
  derived_delay_distributions %>%
    mutate(
      sim_tracing_fun = pmap(
        .l = list(d1 = dist_isol_swab,
                  d2 = dist_test_turnaround_time,
                  d3 = dist_time_to_interview),
        .f = get_sim_fun
        )
    ) %>% 
    select(scenario,
           sim_tracing_fun)

}
