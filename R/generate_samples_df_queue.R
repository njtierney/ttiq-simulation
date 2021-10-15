#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tidied_queue_simulation
#' @return
#' @author Nicholas Tierney
#' @export
generate_samples_df_queue <- function(tidied_queue_simulation) {

  tidied_queue_simulation
  
  generated_df <- 
    delay_dist_funs %>% 
    rowwise() %>% 
    mutate(samples = list(
      tibble(
        tracing_delay = sim_tracing_fun(n_samples),
        vaccinated = FALSE,
        priority = FALSE
      )
    )) %>% 
    ungroup()
  
  generated_df
  

}
