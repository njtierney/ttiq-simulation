#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param bins
#' @param breaks
#' @return
#' @author Nicholas Tierney
#' @export
estimate_pmf <- function(x,
                         bins = 0:20,
                         breaks = c(bins, max(bins) + 1)- 0.5){
  
  # make sure we get a prob for each day even if the sample lacked them
  hist_info <- hist(x, breaks = breaks, plot = FALSE)
  pmf <- hist_info$density
  names(pmf) <- bins
  pmf
  
}
