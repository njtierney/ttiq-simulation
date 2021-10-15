#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tidied_queue_simulation
#' @return
#' @author Nicholas Tierney
#' @export
derive_distributions_queue <- function(tidied_queue_simulation) {
  
  df_distributions <- tidied_queue_simulation %>% 
    unnest(
      cols = c(
        starts_with("samples_"),
        capacity_ratio
      )
    ) %>% 
    # remove the negative values, which currently mean missing
    # they are missing because they took too long to get into the queue 
    # not sure how to properly deal with these...
    filter(samples_time_to_interview >= 0) %>% 
    group_by(scenario) %>% 
    summarise(
      across(
        .cols = c(
          samples_test_turnaround_time,
          samples_time_to_interview,
          # full_contact_delay
        ),
        .fns = create_empirical_dist,
        .names = "dist_{.col}"
      )
    ) %>% 
    # check: are we going to get rid of this dist_poisson step?
    mutate(dist_isol_swab = dist_poisson(1))
  
  df_distributions %>% 
    rename(
      dist_test_turnaround_time = dist_samples_test_turnaround_time,
      dist_time_to_interview = dist_samples_time_to_interview
    )

}
