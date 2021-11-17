#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_vacc_adjusted_cases
#' @return
#' @author dhduncan
#' @export
get_frac_vaccinated <- function(age_vacc_adjusted_cases, vaccination_coverage) {

  frac_cases_vaccinated <- age_vacc_adjusted_cases %>% 
    filter(str_detect(status, "Vacc") & vaccination_coverage == vaccination_coverage) %>%
    summarise(fraction_cases_vaccinated = sum(fraction)) %>% 
    pull(fraction_cases_vaccinated)

  fraction_cases_vaccinated

}
