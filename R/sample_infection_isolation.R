#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param infection_ct_delay
#' @param passive_detection_distribution
#' @param max_prob_passive
#' @return
#' @author Nick Golding
#' @export
sample_infection_isolation <- function(infection_ct_delay,
                                       passive_detection_distribution,
                                       max_prob_passive) {

  # vectorisation by recursion!!!
  if (length(infection_ct_delay) > 1) {
    results <- vapply(
      infection_ct_delay,
      passive_detection_distribution = passive_detection_distribution,
      max_prob_passive = max_prob_passive,
      FUN = sample_infection_isolation,
      FUN.VALUE = numeric(1)
    )
    return(results)
  }
  
  # the probability of being found by passive detection (rather than contact
  # tracing) is the probability of having been passively detected before contact
  # tracing detection, multiplied by the probability of ever being passively
  # detected (if not found by contact tracing)
  p_passive <- max_prob_passive * cdf(passive_detection_distribution, infection_ct_delay) 
  
  # compute the distribution of times to isolation based on this - either a
  # sample from the truncated passive detection distribution if found that way,
  # or the tie to being found by contact tracing, otherwise
  infection_isolation_distribution <- dist_mixture(
    dist_truncated(
      dist = passive_detection_distribution,
      lower = 0,
      upper = infection_ct_delay
    ),
    dist_degenerate(infection_ct_delay),
    weights = c(p_passive, 1 - p_passive)
  )
  
  # sample the time from infection to isolation for this individual
  generate(infection_isolation_distribution, 1)[[1]]
  
}
