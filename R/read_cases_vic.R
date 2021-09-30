
#' Replicate read_cases_nsw for VIC data
#' 
#' @author Logan Wu
#' @export
read_cases_vic <- function(linelist_path) {
  read_excel(linelist_path, guess_max=1048576) %>%
    mutate_at(vars(ends_with("Date")), as.Date) %>%
    rename(swab_date = FirstSpecimenPositiveDate,
           interview_date = InterviewDate,
           notification_date = DiagnosisDate) %>%
    mutate_at(vars(interview_date, swab_date, notification_date),
              function (x) if_else(x > as_date("2020-01-01") & x <= today(),
                                  x,
                                  NA_Date_)) %>%
    rename(
      earliest_detected = swab_date,
      interviewed_date = interview_date,
      earliest_confirmed_or_probable = notification_date
    )
}
