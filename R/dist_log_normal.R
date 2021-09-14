#' Log normal distribution
#'
#' @param mu mean parameter for normal
#' @param sigma sigma
#'
#' @return log normal distributional function
#' 
#' @examples
#' my_log_dist <- dist_log_normal(1, 0)
#' generate(my_log_dist, 10)
dist_log_normal <- function(mu, sigma){
  dist_transformed(
    dist = dist_normal(mu, sigma), 
    transform = exp,
    inverse = log
    )
}