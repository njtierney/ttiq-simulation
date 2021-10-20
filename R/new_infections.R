#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
new_infections <- function(infections, vaccinated = FALSE) {
  
  # infect new people, differently for if
  
  if (vaccinated) {
    infectiousness_multiplier <- .abm_parameters$ve_susceptibility
    fraction <- .abm_parameters$vaccination_coverage
  } else {
    infectiousness_multiplier <- 1
    fraction <- 1 - .abm_parameters$vaccination_coverage
  }
  
  # simulate onward infections
  infectiousness <- infectiousness(infections) * infectiousness_multiplier
  onward_infections <- rpois(
    nrow(infections),
    infectiousness * fraction
  )
  
  n_new <- sum(onward_infections)
  
  if (n_new > 0) {
    new_infections <- data.frame(
      id = .abm_globals$highest_id + seq_len(n_new),
      source_id = rep(infections$id, onward_infections),
      infection_day = .abm_globals$day,
      isolation_day = Inf,
      vaccinated = rbinom(n_new, 1, .abm_parameters$vaccination_coverage),
      symptomatic = rbinom(n_new, 1, .abm_parameters$clinical_fraction)
    )
  } else {
    new_infections <- NULL
  }
  
  
  .abm_globals$highest_id <<- .abm_globals$highest_id + n_new
  
  new_infections
  
}
