#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples
#' @param cases_nsw_delays
#' @return
#' @author Nicholas Tierney
#' @export
add_data_to_delay_samples <- function(nsw_delay_samples, cases_nsw_delays) {

  cases_nsw_delays_renamed <- cases_nsw_delays %>% 
    filter(scenario != "outside") %>% 
    select(-ends_with("date")) %>% 
    mutate(
      across(
        .cols = -scenario,
        .fns = as.numeric
        )
    )
  
  nsw_delay_samples_renames <- nsw_delay_samples %>% 
    rename_with(
      .fn = ~str_remove_all(.x, "samples_"),
      .cols = starts_with("samples_")
    )
  
  bind_rows(
    data = cases_nsw_delays_renamed,
    samples = nsw_delay_samples_renames,
    .id = "data_type"
  )
    

}
