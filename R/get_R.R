#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param new_transition_matrix
#' @param stable_age
#' @return
#' @author Nicholas Tierney
#' @export
get_R <- function(transition_matrix,
                  stable_age = NULL,
                  tolerance = 0.001,
                  max_iter = 1000,
                  warm_start = TRUE) {
  #Re(eigen(x)$value[1])
  # function from STEPS
  # https://github.com/steps-dev/steps/blob/74c5359dd4470c4056cd799c53ef56d503ba69da/R/growth_transition_functions-class.R#L211
  # compute R from a transition (next generation) matrix
  
  # assign the stab;e age distribution if not known
  if (is.null(stable_age)) {
    # if using warm starts, try to find the recent stashed stable age distribution
    if (warm_start & exists(".ngm_stage_age_warm_start")) {
      stable_age <- .ngm_stage_age_warm_start
      # otherwise just start from scratch
    } else {
      stable_age <- rep(1, ncol(transition_matrix))
    }
  }
  
  old_stages <- stable_age
  converged <- FALSE
  iter <- 0
  old_Rs <- rep(.Machine$double.eps, ncol(transition_matrix))
  
  while (!converged & iter < max_iter) {
    new_stages <- transition_matrix %*% old_stages
    Rs <- new_stages / old_stages
    errors <- abs(1 - (Rs / old_Rs))
    converged <- all(errors < tolerance)
    old_Rs <- Rs
    old_stages <- new_stages
    iter <- iter + 1
  }
  
  if (!converged) {
    warning("estimation of growth rate did not converge in ",
            max_iter,
            " iterations")
  }
  
  # globally assign the stable age distribution if needed
  if (warm_start) {
    .ngm_stage_age_warm_start <<- new_stages / sum(new_stages)
  }
  
  # return the intrinsic growth rate
  Rs[1]
  
}
