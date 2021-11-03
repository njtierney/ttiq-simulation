#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dim_time_path
#' @return
#' @author Nicholas Tierney
#' @export
read_week_lookup <- function(dim_time_path) {

  read_csv(dim_time_path) %>%
    mutate(
      week = as_date(week_starting)
    ) %>%
    select(
      -week_starting
    )

}
