#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_scenario
#' @return
#' @author David Duncan
#' @export

create_empirical_dist <-  function(x, 
                                   bins = 0:20,
                                   breaks = c(bins, max(bins) + 1)- 0.5,
                                   fraction_extra_zeros = NULL){
  
  # make sure we get a prob for each day even if the sample lacked them
  pmf <- estimate_pmf(x, 
                      bins = bins,
                      breaks = breaks)
  
  dist <- dist_categorical(
    prob = list(pmf),
    outcomes = list(bins)
  )
  
  if (!is.null(fraction_extra_zeros)){
    dist <- dist_inflated(
      dist = dist, 
      prob = fraction_extra_zeros,
      x = 0)
  }
  
  dist
}