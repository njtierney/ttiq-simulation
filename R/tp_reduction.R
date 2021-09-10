#' Convert samples of the time from infection to isolation into a multiplier 
#' on TP
#'
#' @param inf_isol 
#' @param meanlog 
#' @param sdlog 
#'
#' @return
#' @export
tp_reduction <- function(inf_isol,
                         meanlog,
                         sdlog) {
  reductions <- plnorm(
    inf_isol,
    meanlog = meanlog,
    sdlog = sdlog
  )
  mean(reductions)
}
