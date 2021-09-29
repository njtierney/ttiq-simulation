#' Generic method to turn different cases of distributions into character strings
#' 
#' `if` cases are currently set to the different distributions seen in the data.
#' 
#' @param x Distribution object
#' 
#' @return A customised character string
#' @export
as.character.distribution <- function(x) {
  sapply(x, function(xx) {
    if (inherits(xx, "dist_poisson")) {
      glue::glue("l={xx$l}")
    } else {
      if (inherits(xx$dist[[2]]$dist, "dist_poisson"))
        pois_dist = xx$dist[[2]]$dist
      else {
        pois_dist = xx$dist[[2]]$dist[[2]]$dist
      }
      glue::glue("l={pois_dist$l},p={xx$w[2][[1]]}")
    }
  }) %>%
    as.character()
}

#' Convert a dataframe of distributions into a dataframe of parameters
#' 
#' @param derived_delay_distributions Dataframe with a scenario column and other
#' columns of distributions
#' 
#' @return A dataframe of 
dist_params_to_df <- function(derived_delay_distributions) {
  derived_delay_distributions %>%
    pivot_longer(-scenario, names_to="delay_name", values_to="distribution") %>%
    mutate(text = as.character(distribution)) %>%
    separate(text, into=c("rate", "w"), sep=",", fill="right") %>%
    mutate(rate = as.numeric(str_remove(rate, "l=")),
           w = as.numeric(str_remove(w, "p="))) %>%
    select(-distribution)
}

