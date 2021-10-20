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
    R = 3.62,
    # incorporate correction for onward transmission to account for reduction in
    # infectiousness due to being symptomatic
    ve_onward = 0.5 * 0.6,
    ve_susceptibility = 0.73,
    vaccination_coverage = 0.8,
    asymptomatic_relative_infectiousness = 0.5,
    clinical_fraction = 0.4,
    n_days = 365
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
