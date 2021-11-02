#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
setup_abm <- function(
  
  # pre-vaccination R to aim for
  R = 3.62,
  
  # vaccination effects on transmission    
  vaccination_coverage = 0.9,
  # incorporate correction for onward transmission to account for reduction in
  # infectiousness due to being symptomatic
  ve_onward = 0.639, # sensitivity analysis values, lower=0.575, upper=0.7029
  ve_susceptibility = 0.735,
  ve_symptoms = 0.775,
  
  
  # symptomaticity and passive detection
  clinical_fraction = 0.307, #0.307, # was previously - 0.8
  passive_detection_given_symptoms = 0.5,
  asymptomatic_relative_infectiousness = 0.5,
  vaccination_test_seeking_multiplier = 1,
  
  # whether to do screening of symptomatics
  screening = TRUE,
  # probability of an infectee being found by contact tracing from the source
  p_active_detection = 0.95,
  
  # relative probability of active detection for vaccinated individuals
  rel_active_detection_vaccinated_source = 1,
  
  # relative probability of quarantinng/isolating vaccinated contact
  rel_active_detection_vaccinated_contact = 1,
  
  # whether to do downstream contact tracing
  contact_tracing = TRUE,
  # placeholder for delays
  isolation_to_interview_samples = get_optimal_isol_interview_samples(),
  isolation_days_vax=14,
  isolation_start_day=c("isolation", "infection")
) {
  
  isolation_start_day <- match.arg(isolation_start_day)
  
  args <- list(
    R = R,
    vaccination_coverage = vaccination_coverage,
    ve_onward = ve_onward,
    ve_susceptibility = ve_susceptibility,
    ve_symptoms = ve_symptoms,
    clinical_fraction = clinical_fraction,
    passive_detection_given_symptoms = passive_detection_given_symptoms,
    asymptomatic_relative_infectiousness = asymptomatic_relative_infectiousness,
    vaccination_test_seeking_multiplier = vaccination_test_seeking_multiplier,
    screening = screening,
    p_active_detection = p_active_detection,
    rel_active_detection_vaccinated_source = rel_active_detection_vaccinated_source,
    rel_active_detection_vaccinated_contact = rel_active_detection_vaccinated_contact,
    contact_tracing = contact_tracing,
    isolation_to_interview_samples = isolation_to_interview_samples,
    isolation_days_vax=isolation_days_vax,
    isolation_start_day=isolation_start_day
    
  )
  
  # correct R for reduced infectiousness of asymptomatics
  args$R_star <- with(
    args,
    R / ((1 - clinical_fraction) + clinical_fraction * asymptomatic_relative_infectiousness)
  )
  
  args
  
}
