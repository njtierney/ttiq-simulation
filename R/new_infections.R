#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
new_infections <- function(infections, vaccinated = FALSE) {
  
  # infect new people, differently for if the new infections are vaccinated
  
  if (vaccinated) {
    susceptibility_multiplier <- 1 - .abm_parameters$ve_susceptibility
    fraction <- .abm_parameters$vaccination_coverage
    clinical_fraction_multiplier <- 1 - .abm_parameters$ve_symptoms
  } else {
    susceptibility_multiplier <- 1
    fraction <- 1 - .abm_parameters$vaccination_coverage
    clinical_fraction_multiplier <- 1
  }
  
  # simulate onward infections
  infectiousness <- infectiousness(infections) * susceptibility_multiplier
  onward_infections <- rpois(
    nrow(infections),
    infectiousness * fraction
  )
  
  p_symptoms <- clinical_fraction_multiplier * .abm_parameters$clinical_fraction
  
  n_new <- sum(onward_infections)
  
  if (n_new > 0) {
    new_infections <- data.frame(
      id = .abm_globals$highest_id + seq_len(n_new),
      source_id = rep(infections$id, onward_infections),
      infection_day = .abm_globals$day,
      isolation_day = Inf,
      case_found_by = NA,
      vaccinated = vaccinated,
      symptomatic = rbinom(n_new, 1, p_symptoms)
    )
  } else {
    new_infections <- NULL
  }
  
  .abm_globals$highest_id <<- .abm_globals$highest_id + n_new
  
  new_infections
  
}
