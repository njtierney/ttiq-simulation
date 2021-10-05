#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vic_casual_cases_covid_monthly
#' @return
#' @author Nicholas Tierney
#' @export
gg_casual_cases_covid_monthly <- function(vic_casual_cases_covid_monthly) {

  ggplot(vic_casual_cases_covid_monthly,
         aes(x = date,
             y = proportion)) + 
    geom_line(size = 1) + 
    scale_y_continuous(labels = label_percent()) +
    labs(title = "Percentage of COVID19 cases in Victoria from casual contact",
         subtitle = "Monthly since Jan, 2020",
         x = "Date",
         y = "Percentage") + 
    theme_minimal()

}
