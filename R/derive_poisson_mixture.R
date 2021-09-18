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
    return(dist_degenerate(0))
  }
  
  coefs <- hurdle(x ~ 1)$coefficients
  rate <- exp(coefs$count)
  p_nonzero <- plogis(coefs$zero)
  
  
  zero <- dist_degenerate(0)
  nonzero_poisson <- dist_truncated(
    dist_poisson(lambda = rate),
    lower = 0
  )
  
  dist_mixture(
    zero,
    nonzero_poisson,
    weights = c(1 - p_nonzero, p_nonzero)
  )
  
}
