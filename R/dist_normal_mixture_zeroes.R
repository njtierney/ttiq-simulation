#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mu
#' @param sigma
#' @param weights
#' @return
#' @author Nicholas Tierney
#' @export
dist_normal_mixture_zeroes <- function(mu, sigma, weights) {

  dist_zero <- dist_uniform(min = 0, max = 0)
  
  dist_mixture(dist_normal(mu = ))

}
