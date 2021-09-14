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

  nsw_cases <- read_excel(
    # path = cases_nsw_path,
    path = "data/CASES_FROM_20200701_0000_TO_20210913_1115.xlsx",
    sheet = "in",
    na = "NA"
    ) %>%  
    clean_names() %>% 
    rename(swab_date = earliest_detected,
           interview_date = interviewed_date,
           notification_date = earliest_confirmed_or_probable) %>% 
    mutate(
      across(
        .cols = c(interview_date,
                  swab_date,
                  notification_date),
        .fns = as_date)
    ) %>% 
    # 
    mutate(
      across(
        c(
          interview_date,
          swab_date,
          notification_date
        ),
        .fns = ~.x >= as_date("2020-01-01") & .x <= today(),
        .names = "valid_{.col}"
      )
    ) %>% 
    mutate(
      interview_date = if_else(condition = !valid_interview_date,
                               true = NA_Date_,
                               false = interview_date),
      swab_date = if_else(condition = !valid_swab_date,
                          true = NA_Date_,
                          false = swab_date),
      notification_date = if_else(condition = !valid_notification_date,
                                  true = NA_Date_,
                                  false = notification_date)
    )
  
  nsw_cases
  
  # 1. Swab Date (earliest_detected) Yep
  # 
  # 2. Date of notification (earliest_confirmed_or_probable)
  # 
  # 3. Date of interview (interviewed_date) Yep
  

}
