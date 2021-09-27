#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param delay
#' @return
#' @author Nick Golding
#' @export
gi_pmf_discrete <- function(days, bounds = c(0, 20)) {
  
  n_days <- length(days)
  # set invalid days to -1 (density 0)
  out_of_bounds <- days < bounds[1] | days > bounds[2]
  days[out_of_bounds] <- -1
  
  # get discretised probability, without accounting for truncation  
  p <- gi_cdf_continuous(days + 1) - gi_cdf_continuous(days)
  
  # adjust density for truncation
  upper_bound <- gi_cdf_continuous(bounds[2] + 1)
  lower_bound <- gi_cdf_continuous(bounds[1])
  p <- p / (upper_bound - lower_bound)
  
  p
}
