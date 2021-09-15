#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param test_turnaround_time
#' @param nameme1
#' @param nameme2
#' @return
#' @author Nicholas Tierney
#' @export
set_na_when_not_between <- function(x, left = 0, right = 14) {

    case_when(
      between(x, left, right) ~ x,
      TRUE ~ NA_real_
    )

}
