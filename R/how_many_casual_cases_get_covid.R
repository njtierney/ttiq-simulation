#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_vic
#' @param casual_cases
#' @param casual_vic
#' @return
#' @author Nicholas Tierney
#' @export
how_many_casual_cases_get_covid <- function(cases_vic,
                                            casual_cases,
                                            casual_vic) {

  # Out of all of the people identified as casual contacts
  # How many casual cases get covid?
  
  # tell us the number of people who were identified as casual contacts
  # this will be a really big number, as there are a lot of people identified
  # as casual contacts
  
  n_casual_cases <- nrow(casual_cases) 
  n_casual_contacts <- nrow(casual_vic)
  
  # this tells us the yield - how many cases
  prop_casual_contacts_w_covid <- n_casual_cases / n_casual_contacts
  
  #IF 3% cases are casual contacts - then we will miss 3% of cases
  # SO - of the cases that we have, what 
  # of the 
  prop_cases_who_are_casual_contacts <- n_casual_cases / nrow(cases_vic)
  
  return(
    tibble(
      prop_casual_contacts_w_covid,
      prop_cases_who_are_casual_contacts,
    )
  )
  
  # > So to me there are two reasons to know that figure: 
  # (i) what are the real risks of ditching 'Bunnings' contact tracing (Woolies is reporting 25,000 furloughed staff with zero cases) and 
  # (ii) can it help us to understand how the proportion of all contacts readily identifiable by the case ends up relating to the proportion of all contacts who end up becoming infected. 
  # Given the way mixing patterns have changed over time the relationship between the two components of (ii) is reasonably anticipated to change but is it a LOT? Does that make sense?
    

}
# denominator will be the number of rows in Linelist_Casual
# SCC =  secondary close contact
# pCC =  primary close contact