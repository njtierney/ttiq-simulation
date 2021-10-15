#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @return
#' @author Nicholas Tierney
#' @export
impute_inf <- function(x, value) {
  
  if (length(value) > 1) {
    stop("value must be length 1. ",
         "But value has length ",
         length(value))
  }

  which_x <- x == value
  x[which_x] <- Inf
  x

}
