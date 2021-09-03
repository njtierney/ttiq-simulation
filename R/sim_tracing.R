# simulate the time from an infector being isolated to an infectee being isolated
sim_tracing <- function(n, mu, sigma) {
  rtrunc(
    n = n,
    spec = "norm",
    a = 0,
    b = Inf,
    mean = mu,
    sd = sigma
  )
}
