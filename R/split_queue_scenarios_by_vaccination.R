#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param samples_df_queue
#' @param scenario
#' @return
#' @author Nicholas Tierney
#' @export
split_queue_scenarios_by_vaccination <- function(samples_df_queue, 
                                                 scenario = "random_swab") {

  # pull out the tidied queueing delays corresponding to specified scenarios
  samples_list <- samples_df_queue %>% 
    filter(
      str_detect(scenario, "random_swab")
    ) %>% 
    unnest_wider(col = samples) %>% 
    unnest(cols = everything()) %>% 
  # split that into two tibbles, one for vaccination status = TRUE, one for vaccination status = FALSE
    group_split(
      vaccinated, 
      scenario
      )
  
  list_names <- get_scenario_vaccinated(samples_list)
  
  names(samples_list) <- list_names
  
  samples_list
  # save those as two separate files (vaccinated/unvaccinated) for each scenario

}
