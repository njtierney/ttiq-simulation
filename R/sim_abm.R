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
  parameters = setup_abm(),
  max_infections = Inf
) {
  
  # put the parameters and some mutable variables in the global environment, to
  # be accessed anywhere
  .abm_parameters <<- parameters
  .abm_globals <<- list(highest_id = 0)
  
  days <- seq_len(parameters$n_days)
  for (day in days) {
    
    .abm_globals$day <<- day
    
    # infect people
    infections <- infect(infections)
    
    # do passive detection
    infections <- do_screening(infections)
    
    # quit if we hit the maximum total infections
    if (nrow(infections) >= max_infections) {
      break()
    }
    
  }
  
  infections
  
}
