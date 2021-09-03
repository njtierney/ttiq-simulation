# use a Gibbs Sampler to convert distributions of time from infector isolation
# to infectee isolation into distributions of time from infection to isolation
# of cases, and therefore reduction in transmission potential

# Case 1 is infected at time 0 and isolated at time inf_isol
# Case 2 is infected at time inf_inf, drawn from a lognormal
# representing the naive generation interval distribution, truncated at the date
# of Case 1 isolation.
# Case 2 is isolated at time: inf_isol = inf_isol + isol_isol - inf_inf

# parameters of naive (untruncated) generation interval / infectiousness profile
gi_meanlog <- 1.375738
gi_sdlog <- 0.5665299

# initial time from infection to isolation
n_iterations <- 1000
n_chains <- 50
trace <- matrix(NA, nrow = n_chains, ncol = n_iterations)

# times from infection to isolation for the initial cases in each chain
inf_isol <- abs(rnorm(n_chains, 14, 5))

# Gibbs sample multiple Markov chains in parallel to obtain the distribution of
# times from infection to isolation
for (iteration in seq_len(n_iterations)) {
  # simulate a generation interval
  inf_inf <- sim_gi_truncated(n_chains, inf_isol)
  # simulate the delay from isolation of case to isolation of (infected)
  # contact, which here is independent of the other random variables
  isol_isol <- sim_tracing(n_chains, 1.9, 2)
  # compute the infection to isolation for the (infected) contact, based on the
  # infection to isolation for the case, the generation interval, and the
  # contact tracing time
  inf_isol <- inf_isol + isol_isol - inf_inf 
  # record the time from infection to isolation in this step
  trace[, iteration] <- inf_isol
}

# histogram of times to isolation from simulations
hist(
  trace,
  breaks = 50,
  border = "white",
  xlab = "Time from infection to isolation",
  main = ""
)

# compute the multiplier on TP for this distribution
tp_multiplier <- tp_reduction(trace)

# given a starting R, get the R after contact tracing
R_start <- 7.82
R_start * tp_multiplier

