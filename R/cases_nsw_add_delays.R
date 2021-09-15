#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw
#' @return
#' @author Nicholas Tierney
#' @export
cases_nsw_add_delays <- function(cases_nsw) {

  # analyse NSW data to get distributions of these delays
  
  cases_nsw_delays <- 
    cases_nsw %>% 
    select(interview_date,
           swab_date,
           notification_date
           ) %>% 
    mutate(
      test_turnaround_time = swab_date - notification_date,
      time_to_interview = notification_date - interview_date,
      full_contact_delay = swab_date - interview_date
    ) %>% 
    filter(
      between(as.numeric(test_turnaround_time), 0, 14),
      between(as.numeric(time_to_interview), 0, 14),
      between(as.numeric(full_contact_delay), 0, 14)
    ) %>% 
    mutate(
      period = case_when(
        notification_date >= as_date("2020-07-01") &  notification_date <= as_date("2021-02-01") ~ "optimal",
        notification_date >= (today() - 30) ~ "current",
        TRUE ~ "outside" 
        ),
      .before = interview_date
      ) %>% 
    mutate(
      test_to_interview = test_turnaround_time + time_to_interview
    ) 
  
  cases_nsw_delays
  
  ## optimal is this time period from July 2020 to Feb 2021
  
  ## Current is from the last month
  
  ## current + case initiated
  # same as current, but we randomly set the notification to interview to 0
  
  # (assuming the infector isolates on date of swab)
  # (assuming infectees isolate on date of interview)
  
  # difference of 1-2 is test turnaround time
  # simulate random draw from this distribution
  # difference of 2-3 is time to interview ()
  # simulate random draw from this distribution
  
  # sum these random draws together
  # this gives you the full contact tracing delay
  # this ^^ returns what we currently have in the sim_tracing function

}
