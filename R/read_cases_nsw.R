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
  nsw_cases <- read_excel( # path = cases_nsw_path,
    path = cases_nsw_path,
    sheet = "in",
    na = "NA"
  ) %>%
    clean_names() %>%
    mutate(across(
      .cols = c(
        interviewed_date,
        earliest_detected,
        earliest_confirmed_or_probable
      ),
      .fns = as_date
    )) %>%
    #
    mutate(across(
      c(
        interviewed_date,
        earliest_detected,
        earliest_confirmed_or_probable
      ),
      .fns = ~ .x >= as_date("2020-01-01") & .x <= today(),
      .names = "valid_{.col}"
    )) %>%
    mutate(
      interviewed_date = if_else(
        condition = !valid_interviewed_date,
        true = NA_Date_,
        false = interviewed_date
      ),
      earliest_detected = if_else(
        condition = !valid_earliest_detected,
        true = NA_Date_,
        false = earliest_detected
      ),
      earliest_confirmed_or_probable = if_else(
        condition = !valid_earliest_confirmed_or_probable,
        true = NA_Date_,
        false = earliest_confirmed_or_probable
      )
    ) 
  
  
  nsw_cases
  
}
