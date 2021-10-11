#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dist_test_turnaround_time
#' @param dist_time_to_interview
#' @return
#' @author Nicholas Tierney
#' @export
get_sim_fun <- function (...) {
  distributions <- list(...)
  function(n) {
    sims <- map(.x = distributions, 
                .f = generate, 
                times = n) %>% 
      as_tibble() %>% 
      unnest(cols = everything())
    
    rowSums(sims)
    # sims <- lapply(distributions, generate, n)
    # Reduce(
    #   f = "+",
    #   x = sims
    #   )
  }
}
