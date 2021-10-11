#' Generic method to turn different cases of distributions into character strings
#' 
#' `if` cases are currently set to the different distributions seen in the data.
#' 
#' @param x Distribution object
#' 
#' @return A customised character string
#' @export
as.character.distribution <- function(x) {
  vapply(vec_data(x), function(xx) {
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
  },
  FUN.VALUE = character(1L))
}
