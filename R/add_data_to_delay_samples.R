#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param delay_samples
#' @param cases_scenario
#' @return
#' @author Nicholas Tierney
#' @export
add_data_to_delay_samples <- function(delay_samples, cases_scenario) {
  
  delay_samples_renamed <- delay_samples %>% 
    rename_with(
      .fn = ~str_remove_all(.x, "samples_"),
      .cols = starts_with("samples_")
    )
  
  cases_delays_renamed <- cases_scenario %>% 
    filter(scenario != "outside") %>% 
    select(-ends_with("date")) %>% 
    mutate(
      across(
        .cols = -scenario,
        .fns = as.numeric
        )
    )
  
  bind_rows(
    data = cases_delays_renamed,
    samples = delay_samples_renamed,
    .id = "data_type"
  ) 

}
