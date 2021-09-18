#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_passive_distribution <- function() {

  passive_distribution <- dist_lognormal(
    meanlog = log(5),
    sdlog = 0.5
  )
  
}
