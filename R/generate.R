#' Generate samples from the special categorical
#'
#' @param x object of class "dist_categorical_special" created by `create_empirical_dist()`
#' @param times number of samples
#' @param days days to sample - in this case, 0:20 by default
#' @param ... extra arguments to `generate`
#'
#' @return samples from dist_categorical_special
#' @export
generate.dist_categorical_special <- function(x, times, days = 0:20,...){
  
    sample(days,
           size = times,
           replace = TRUE,
           prob = x)
}