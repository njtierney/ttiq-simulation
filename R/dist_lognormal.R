#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meanlog
#' @param sdlog
#' @return
#' @author Nick Golding
#' @export
dist_lognormal <- function(meanlog, sdlog) {

  dist_transformed(
    dist = dist_normal(
      mu = meanlog,
      sigma = sdlog
    ),
    transform = exp,
    inverse = log
  )

}
