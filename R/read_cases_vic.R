#' Replicate read_cases_nsw for VIC data
#' 
#' @author Logan Wu
#' @export
read_cases_vic <- function() {
  vic_cases = loadLinelist("cases") %>%
    rename(swab_date = FirstSpecimenPositiveDate,
           interview_date = InterviewDate,
           notification_date = DiagnosisDate) %>%
    mutate_at(vars(interview_date, swab_date, notification_date),
              function (x) ifelse(x > as_date("2020-01-01") & x <= today(),
                                  x,
                                  NA_Date_)) %>%
    # mutate(across(
    #   c(
    #     interview_date,
    #     swab_date,
    #     notification_date
    #   ),
    #   .fns = ~ .x >= as_date("2020-01-01") & .x <= today(),
    #   .names = "valid_{.col}"
    # )) %>%
    # mutate(
    #   interview_date = if_else(
    #     condition = !valid_interview_date,
    #     true = NA_Date_,
    #     false = interview_date
    #   ),
    #   swab_date = if_else(
    #     condition = !valid_swab_date,
    #     true = NA_Date_,
    #     false = swab_date
    #   ),
    #   notification_date = if_else(
    #     condition = !valid_notification_date,
    #     true = NA_Date_,
    #     false = notification_date
    #   )
    # ) %>%
    # mutate(
    #   test_turnaround_time = as.numeric(notification_date - swab_date),
    #   time_to_interview = as.numeric(interview_date - notification_date),
    #   full_contact_delay = as.numeric(interview_date - swab_date)
    # ) %>%
    rename(
      earliest_detected = swab_date,
      interviewed_date = interview_date,
      earliest_confirmed_or_probable = notification_date
    )
  
  # 1. Swab Date (earliest_detected) Yep
  #
  # 2. Date of notification (earliest_confirmed_or_probable)
  #
  # 3. Date of interview (interviewed_date) Yep
}
