#' Create a function to simulate the time from an infector being isolated to an infectee being isolated
#'
#' @param mu mean parameter
#' @param sigma sd parameter
#'
#' @return a function that takes "n" input - the number of samples to draw
#' @export
#'
#' @examples
#' sim_tracing_default <- build_sim_tracing_default(mu = 1.9, sigma = 2)
#' sim_tracing_default(1)
#' sim_tracing_default(10)
#' sim_tracing_default(100)
build_sim_tracing_default <- function(mu, sigma) {
  function(n) {
    rtrunc(
      n = n,
      spec = "norm",
      a = 0,
      b = Inf,
      mean = mu,
      sd = sigma
    )
  }
}
