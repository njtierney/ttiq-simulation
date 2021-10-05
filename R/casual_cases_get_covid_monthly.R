#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_vic
#' @param casual_cases
#' @return
#' @author Nicholas Tierney
#' @export
casual_cases_get_covid_monthly <- function(cases_vic, casual_cases) {

  
  casual_cases_monthly <- casual_cases %>% 
    mutate(month = month(CalculatedOnsetDate),
           year = year(CalculatedOnsetDate)) %>% 
    group_by(year,
             month) %>% 
    summarise(
      n_casual_cases = n()
    )
  
  cases_monthly <- cases_vic %>% 
    mutate(month = month(CalculatedOnsetDate),
           year = year(CalculatedOnsetDate)) %>% 
    group_by(year,
             month) %>% 
    summarise(
      n_cases = n()
    )
  
  # would be interesting to correlate this with how far behind contact tracing
  # is - might be interesting to see
  casual_cases_monthly %>% 
    left_join(
      cases_monthly,
      by = c("year", "month")
    ) %>% 
    mutate(proportion = n_casual_cases / n_cases,
           day = 1,
           date = as_date(ISOdate(year = year, month = month, day = day))) %>% 
    relocate(date) 

}
