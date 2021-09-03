#' Convert samples of the time from infection to isolation into a multiplier 
#' on TP
#'
#' @param inf_isol 
#'
#' @return
#' @export
tp_reduction <- function(inf_isol) {
  reductions <- plnorm(
    inf_isol,
    meanlog = gi_meanlog,
    sdlog = gi_sdlog
  )
  mean(reductions)
}
