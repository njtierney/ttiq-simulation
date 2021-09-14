#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_path
#' @return
#' @author Nicholas Tierney
#' @export
read_cases_nsw <- function(cases_nsw_path) {

  # Swab date is earliest_detected.
  nsw_cases <- read_csv(
    file = cases_nsw_path,
    name_repair = make_clean_names
    )
  
  nsw_cases
  

}
