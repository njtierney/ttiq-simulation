#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param delay_params
#' @param n_samples
#' @return
#' @author dhduncan
#' @export
sim_delay <- function(delay_params, n_samples) {

  sim_basic <- sample(
    x = delay_params$days,
    size = n_samples,
    prob = delay_params$pmf,
    replace = TRUE
  )
  
  sim_nonzeros <- rbinom(
    n_samples,
    1,
    prob = 1 - delay_params$fraction_extra_zero
  )
  
  # return distribution object
  sim_basic * sim_nonzeros
}
