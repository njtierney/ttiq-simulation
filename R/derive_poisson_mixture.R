#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @return
#' @author Nicholas Tierney
#' @export
derive_poisson_mixture <- function(x) {
  x <- as.numeric(x)
  
  if (all(x == 0)) {
    return(dist_uniform(0, 0))
  }
  
  coefs <- zeroinfl(x ~ 1 | 1)$coefficients
  rate <- exp(coefs$count)
  p_extra_zero <- plogis(coefs$zero)
  n <- length(x)
  dist_mixture(
    dist_uniform(0, 0),
    dist_poisson(lambda = rate),
    weights = c(p_extra_zero, 1 - p_extra_zero)
  )
  
}
