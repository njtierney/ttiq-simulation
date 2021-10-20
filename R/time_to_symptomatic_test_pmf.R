#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param days_since_infection
#' @return
#' @author Nick Golding
#' @export
time_to_symptomatic_test_pmf <- function(days_since_infection) {

  # probability mass function for this being the date of test-seeking (one day
  # after symptom onset)
  mean <- 5.8
  sd <- 2
  # mean 5.8 days from parameters doc
  
  var <- sd ^ 2
  meanlog <- log((mean ^ 2) / sqrt(var + mean ^ 2))
  sdlog <- sqrt(log(1 + var / (mean ^ 2)))
  
  upper <- plnorm(days_since_infection + 1,
                  meanlog, sdlog)
  
  lower <- plnorm(days_since_infection,
                  meanlog, sdlog)
  
  upper - lower
  
}
