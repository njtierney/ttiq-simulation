#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Nicholas Tierney
#' @export
get_scenario_vaccinated <- function(x){
  map_chr(
    .x = x,
    .f = function(x){
      glue_data(
        x,
        "{scenario}_vaccinated_{vaccinated}"
      ) %>% 
        unique()
    }
  )
}