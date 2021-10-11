#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_vic
#' @return
#' @author Nicholas Tierney
#' @export
filter_casual_cases <- function(cases_vic) {

  
  # Filter down to the individuals who are cases who were identified as
  # being casual contacts
  # this will be a smaller number as not many casual contacts test positive
  cases_vic %>% 
    filter(
      # status when confirmed
      # 'screening' means casual contact
      # 'Contact Tracing' means close contact
      # 'Clinical Presentation' used to mean 'random detection' / no identified
      # contact
      CaseFoundBy == "Screening",
      # this tells us that they've been interviewed - 
      # which tells us that the casual contacts of *this* case are in 
      # the database
      CaseInterview == "Yes"
    ) 

}
