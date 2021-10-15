#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param delay_dist_funs
#' @return
#' @author Nicholas Tierney
#' @export
generate_samples_df_delays <- function(delay_dist_funs, n_samples = 10000) {
  
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
