#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_nsw_delays
#' @param lower_date
#' @param upper_date
#' @return
#' @author Nicholas Tierney
#' @export
keep_dates_between <- function(data, 
                               lower_date = "2020-07-01",
                               upper_date = "2021-02-01") {

  data %>% 
    filter(
      between(
        x = notification_date,
        left = as_date(lower_date),
        right = as_date(upper_date)
      )
    ) 

}
