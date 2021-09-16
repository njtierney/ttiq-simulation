#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dist_test_turnaround_time
#' @param dist_time_to_interview
#' @return
#' @author Nicholas Tierney
#' @export
get_sim_fun <- function (d1, d2) {
  function(n) {
    generate(d1, n) + generate(d2, n)
  }
}