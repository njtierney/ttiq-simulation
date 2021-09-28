#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
gi_cdf_continuous <- function(days) {
  plnorm(days, meanlog = 1.3757381523, sdlog = 0.5665298555)
}
