#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sim_tracing_fun
#' @param times
#' @return
#' @author Nicholas Tierney
#' @export
generate_dbl <- function(sim_tracing_fun, times) {

  unlist(
  generate(sim_tracing_fun,
           times = times)
  )

}
