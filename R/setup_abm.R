#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
setup_abm <- function(...) {

  # set up parameters for the abm
  new_args <- list(...)
  
  # defining default arguments
  args <- list(
    
    # pre-vaccination R to aim for, and maximum number of days
    R = 3.62,
    n_days = 365,
    
    # vaccination effects on transmission    
    vaccination_coverage = 0.92,
    # incorporate correction for onward transmission to account for reduction in
    # infectiousness due to being symptomatic
    ve_onward = 0.5 * 0.6,
    ve_susceptibility = 0.73,
    ve_symptoms = 0.72,
    
    # symptomaticity and passive detection
    clinical_fraction = 0.8,
    passive_detection_given_symptoms = 0.5,
    asymptomatic_relative_infectiousness = 0.5,
    vaccination_test_seeking_multiplier = 1,
    
    # whether to do screening of symptomatics
    screening = TRUE,
    # probability of an infectee being found by contact tracing from the source
    p_active_detection = 0.95,
    
    # whether to do downstream contact tracing
    contact_tracing = TRUE,
    # placeholder for delays
    isolation_to_interview_samples = rpois(1e3, 0.5)
    
  )
  
  # check validity of arguments
  set_args <- names(new_args)
  possible_args <- names(args)
  invalid <- !set_args %in% possible_args
  if (any(invalid)) {
    stop (
      "invalid parameters: ",
      paste(dput(set_args[invalid]), collapse = ", ")
    )
  }
  
  # update defaults
  updated <- possible_args %in% set_args
  if (length(updated) > 0) {
    args <- args[!updated]
    args <- c(args, new_args)
  }
  
  # correct R for reduced infectiousness of symptomatics
  args$R_star <- with(
    args,
    R / ((1 - clinical_fraction) + clinical_fraction * asymptomatic_relative_infectiousness)
  )
  
  args
  
}
