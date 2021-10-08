#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vic_casual_cases_get_covid
#' @return
#' @author Nicholas Tierney
#' @export
generate_statement_on_casual_cases <- function(vic_casual_cases_get_covid) {

  df <- vic_casual_cases_get_covid %>% 
    mutate(across(.fns = ~scales::percent(.x, accuracy = 0.01)))
  
  prop_casual_contacts_w_covid <- df$prop_casual_contacts_w_covid
  
  prop_cases_who_are_casual_contacts <- df$prop_cases_who_are_casual_contacts
  
  glue(
    "The percentage of casual contacts who end up being identified as having covid is: {prop_casual_contacts_w_covid}
    The percentage of cases who are casual contacts is: {prop_cases_who_are_casual_contacts}"
  )

}
