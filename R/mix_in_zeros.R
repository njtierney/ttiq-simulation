#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param nameme1
#' @return
#' @author Nicholas Tierney
#' @export
mix_in_zeros <- function(pmf, fraction_extra_zeros) {
  pmf[1] <- pmf[1] * (1 - fraction_extra_zeros) + fraction_extra_zeros
  pmf[-1] <- pmf[-1] * (1 - fraction_extra_zeros)
  pmf
}