#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param populations
#' @param aggregated_populations
#' @return
#' @author Nicholas Tierney
#' @export
create_state_populations <- function(populations, aggregated_populations) {

  populations %>% 
    distinct(ste_name16,
             sa4_code16) %>% 
    right_join(
      aggregated_populations,
      by = "sa4_code16"
    )

}
