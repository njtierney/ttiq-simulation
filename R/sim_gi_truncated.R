#' Simulate the generation interval 
#' 
#' Simulate the generation interval (time from an infector being 
#' infected to an infectee being infected) given the infector is isolated 
#' at time infection_to_isolation
#'
#' @param n 
#' @param infection_to_isolation 
#'
#' @return
#' @export
#'
#' @examples
sim_gi_truncated <- function(n, 
                             infection_to_isolation,
                             meanlog,
                             sdlog) {
  # truncnorm is evaluating control flow in a vector of logicals to check b > a,
  # which is triggering a warning. Suppress, since we know we're never flipping
  # the intervals
  suppressWarnings(
    rtrunc(
      n = n,
      spec = "lnorm",
      a = 0,
      b = infection_to_isolation,
      meanlog = gi_meanlog,
      sdlog = gi_sdlog
    )
  )
}
