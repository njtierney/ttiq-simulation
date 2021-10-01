#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_vic
#' @return
#' @author Nicholas Tierney
#' @export
how_many_casual_cases_get_covid <- function(cases_vic) {

  # Out of all of the people identified as casual contacts
  # How many casual cases get covid?
  cases_vic %>% 
    filter(
      # this should tell us who is confirmed as having covid
      Classification == "Confirmed",
      
      CaseFoundBy == "Screening"
    ) %>% 
    relocate(AccountID,
             CasualContact)
    

}
