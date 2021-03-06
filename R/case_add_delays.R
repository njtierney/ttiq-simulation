#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#'
#' @param case
#' @param swab_date_var
#'
#' @return
#' @author Nicholas Tierney
#' @export
case_add_delays <- function(cases,
                            swab_date_var,
                            interview_date_var,
                            notification_date_var) {
  # analyse NSW data to get distributions of these delays
  
  # for NSW
  # 1. Swab Date (earliest_detected) Yep
  # 2. Date of notification (earliest_confirmed_or_probable)
  # 3. Date of interview (interviewed_date) Yep
  
  case_delays <- cases %>%
    rename(
      swab_date = {{ swab_date_var }},
      interview_date = {{ interview_date_var }},
      notification_date = {{ notification_date_var }}
    ) %>%
    mutate(
      test_turnaround_time = as.numeric(notification_date - swab_date),
      time_to_interview = as.numeric(interview_date - notification_date),
      full_contact_delay = as.numeric(interview_date - swab_date)
    ) %>% 
  select(
    interview_date,
    swab_date,
    notification_date,
    test_turnaround_time,
    time_to_interview,
    full_contact_delay
  ) %>%
    mutate(across(
      .cols = c(
        test_turnaround_time,
        time_to_interview,
        full_contact_delay
      ),
      .fns = ~ set_na_when_not_between(x = .x,
                                       left = 0,
                                       right = 14)
    )) %>%
    # check - does this code need to run after the filter? It shouldn't matter
    # ...right?
    mutate(test_to_interview = test_turnaround_time + time_to_interview) 
  
  case_delays
  
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
