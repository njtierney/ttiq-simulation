# simulate the time from an infector being isolated to an infectee being isolated
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
