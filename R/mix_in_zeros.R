#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pmf
#' @param fraction_extra_zeros
#' @return
#' @author Nick Golding
#' @export
mix_in_zeros <- function(pmf, fraction_extra_zeros) {
  pmf[1] <- pmf[1] * (1 - fraction_extra_zeros) + fraction_extra_zeros
  pmf[-1] <- pmf[-1] * (1 - fraction_extra_zeros)
  pmf
}