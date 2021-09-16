#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_delays
#' @return
#' @author Nicholas Tierney
#' @export
check_cases_independence <- function(cases_nsw_delays) {

  # swab to notification
  # notification to interview
  cases_nsw_delays %>% 
    mutate(
      full_contact_delay_indep = sample(na.omit(test_turnaround_time),
                                        n(),
                                        replace = TRUE) + sample(
                                          na.omit(time_to_interview),
                                          n(),
                                          replace = TRUE
                                        )
    ) %>% 
    ggplot(aes(x = full_contact_delay_indep)) + 
    stat_ecdf(pad = FALSE) + 
    stat_ecdf(aes(x = full_contact_delay,
                  colour = "orange"))

}
