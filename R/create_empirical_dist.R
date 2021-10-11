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
  hist_info <- hist(x, breaks = breaks, plot = FALSE)
  pmf <- hist_info$density
  names(pmf) <- bins
  
  if (is.null(fraction_extra_zeros)){
    class(pmf) <- c("dist_categorical_special", class(pmf))
  } else if (!is.null(fraction_extra_zeros)){
    pmf <- mix_in_zeros(pmf = pmf,
                         fraction_extra_zeros = fraction_extra_zeros)
    class(pmf) <- c("dist_categorical_special", class(pmf))
  }
  
  pmf
  
}
