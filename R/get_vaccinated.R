#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_vacc_adjusted_cases
#' @return
#' @author dhduncan
#' @export
get_vaccinated <- function(age_vacc_adjusted_cases) {

  age_vacc_adjusted_cases %>% 
    filter(status, grep(pattern = "_vax", status)) %>%
    summarise(fraction_cases_vaccinated = sum(fraction))

  return(
    pull(select(age_vacc_adjusted_cases, fraction_cases_vaccinated))
  )

}
