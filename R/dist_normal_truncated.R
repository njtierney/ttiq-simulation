#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mu
#' @param sigma
#' @param lower
#' @return
#' @author Nicholas Tierney
#' @export
dist_normal_truncated <- function(mu, sigma, lower = -Inf, upper = Inf) {

  dist_truncated(dist_normal(mu = mu, sigma = sigma), lower = 0)

}
