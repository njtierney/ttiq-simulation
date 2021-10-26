#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_vacc_adjusted_cases
#' @return
#' @author dhduncan
#' @export
get_symptomatic <- function(age_vacc_adjusted_cases) {

  age_vacc_adjusted_cases %>% 
    filter(status, grep(pattern = "_symptomatic", status)) %>%
    summarise(fraction_cases_symptomatic = sum(fraction))
  
  return(
    pull(select(age_vacc_adjusted_cases, fraction_cases_symptomatic))
  )
  
}
