#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n
#' @param p_a
#' @param p_b
#' @return
#' @author Nick Golding
#' @export
r_if_either <- function(n, p_a, p_b) {
  
  # given a number of samples required, and two independent bernoulli
  # probabilities for two different variables, simulate draws from both, *for only
  # the cases where at least one of them is true*
  
  # fraction that are either a or b
  p_a_or_b <- 1 - (1 - p_a) * (1 - p_b)
  
  # fraction that are a and b
  p_a_and_b <- p_a * p_b
  
  # fractions that are one, but not the other
  p_a_not_b <- p_a - p_a_and_b
  p_b_not_a <- p_b - p_a_and_b
  
  # the probability of both a and b among those that have either
  p_both_if_any <- p_a_and_b / p_a_or_b
  
  # then among those that are either (not both or neither) get the probability of it being a
  p_either_not_both <- p_b_not_a + p_a_not_b
  p_a_if_either <- p_a_not_b / p_either_not_both
  
  # # check all possible combinations sum to 1
  # p_b_not_a + p_a_not_b + p_a_and_b + (1 - p_a_or_b)
  # p_a_if_either + p_b_if_either
  # p_both_if_any + (p_a_if_either + p_b_if_either) * (1 - p_both_if_any)
  
  # simulate cases that should be both
  both <- rbinom(n, 1, p_both_if_any)
  
  # simulate cases that should be a if either (not both or any)
  a_if_either <- rbinom(n, 1, p_a_if_either)
  
  # combine into results object
  data.frame(
    a = both | a_if_either,
    b = both | (1 - a_if_either)
  )

}

# # compare with a rejection sampler to check
# n <- 1e6
# 
# p_a <- 0.8
# p_b <- 0.9
# 
# sims <- r_if_either(n = n,
#                     p_a = p_a,
#                     p_b = p_b)
#   
# # rejection sampler
# a <- rbinom(n, 1, p_a)
# b <- rbinom(n, 1, p_b)
# valid <- a == 1 | b == 1
# 
# sims_check <- tibble(
#   a = a[valid],
#   b = b[valid]
# )
# 
# # should match
# mean(sims_check$a)
# mean(sims$a)
# 
# # should match
# mean(sims_check$b)
# mean(sims$b)
# 
# # should match
# mean(sims_check$a & sims_check$b)
# mean(sims$a & sims$b)
# 
# # should be 1
# mean(sims_check$a | sims_check$b)
# mean(sims$a | sims$b)

