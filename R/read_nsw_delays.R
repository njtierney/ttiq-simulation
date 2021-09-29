#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_path
#' @return
#' @author Nicholas Tierney
#' @export
read_nsw_delays <- function(cases_nsw_path) {
  
  nsw <- read_cases_nsw(cases_nsw_path)
  
  nsw_delays <- nsw %>%
    select(
      date_infection = setting_of_transmission_date,
      date_onset = symptom_onset_date,
      date_isolation = date_isolation_began,
      date_detection = earliest_confirmed_or_probable
    ) %>%
    mutate(
      across(
        everything(),
        as_date
      )
    ) %>%
    mutate(
      date_infection = case_when(
        date_infection > date_onset ~ as_date(NA),
        date_infection > date_isolation ~ as_date(NA),
        date_infection > date_detection ~ as_date(NA),
        date_infection < as.Date("2020-01-01") ~ as_date(NA),
        TRUE ~ date_infection
      )
    ) %>%
    mutate(
      date_onset = date_infection + 5,
      time_to_isolation = as.numeric(date_isolation - date_onset),
      time_to_detection = as.numeric(date_detection - date_onset),
      state = "NSW"
    ) %>%
    mutate(
      time_to_isolation = set_na_when_not_between(
        time_to_isolation,
        -5, 21
      ),
      time_to_detection = set_na_when_not_between(
        time_to_detection,
        -5, 21
      )
    ) %>%
    filter(
      !is.na(time_to_isolation)
    ) 
  
  nsw_delays

}
