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

  # probability mass function for this being the date of test-seeking, for
  # symptomatics that seek a test
  meanlog <- log(5)
  sdlog <- 0.5
  
  upper <- plnorm(days_since_infection + 1,
                  meanlog, sdlog)
  
  lower <- plnorm(days_since_infection,
                  meanlog, sdlog)
  
  upper - lower
  
}
