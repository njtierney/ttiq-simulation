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
    select(
      interview_date,
      swab_date,
      notification_date,
      test_turnaround_time,
      time_to_interview,
      full_contact_delay
    ) %>%
    mutate(
      across(
        .cols = c(
                  test_turnaround_time,
                  time_to_interview,
                  full_contact_delay
                  ),
        .fns = ~set_na_when_not_between(.x, 0, 14)
      )
    ) %>% 
    mutate(
      scenario = case_when(
        # adjusted dates for VIC
        # notification_date >= as_date("2020-09-01") &
        #   # notification_date <= as_date("2021-08-03") ~ "optimal",
        # notification_date >= as_date("2020-07-19") & notification_date <= as_date("2020-08-19") ~ "2020-07-19 to 2020-08-19",
        notification_date >= as_date("2020-08-01") & notification_date <= as_date("2020-08-07") ~ "2020-08-01 to 2020-08-07",
        notification_date >= (as_date("2021-09-01")) ~ "current",
        TRUE ~ "outside"
      ),
      .before = interview_date
    ) %>%
    mutate(test_to_interview = test_turnaround_time + time_to_interview)
  
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
