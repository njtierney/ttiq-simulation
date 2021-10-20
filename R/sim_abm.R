#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param initial_infections
#' @param parameters
#' @return
#' @author Nick Golding
#' @export
sim_abm <- function(
  infections = sim_initial_infections(100),
  parameters = setup_abm()
) {
  
  days <- seq_len(parameters$n_days)
  for (day in days) {
    infections <- infect(infections, parameters, day)
  }
  
  infections
  
}
