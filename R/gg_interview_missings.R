#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw
#' @return
#' @author Nicholas Tierney
#' @export
gg_interview_missings <- function(cases_nsw) {

  cases_nsw %>% 
    select(
      swab_date,
      interview_date,
      notification_date
    ) %>%
    filter(notification_date > as_date("2021-01-01")) %>% 
    arrange(notification_date) %>% 
    group_by(notification_date) %>% 
    summarise(interview_na = mean(is.na(interview_date)),
              cases = n()) %>% 
    ggplot(aes(x = notification_date,
               size = cases,
               y = interview_na))  + 
    geom_point(alpha = 0.25)

}
