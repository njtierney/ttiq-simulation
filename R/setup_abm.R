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
    R = 1.5,
    ve_onward = 0.4,
    ve_susceptibility = 0.8,
    vaccination_coverage = 0.5,
    clinical_fraction = 0.4,
    n_days = 365
  )
  
  set_args <- names(new_args)
  possible_args <- names(args)
  invalid <- !set_args %in% possible_args
  if (any(invalid)) {
    stop (
      "invalid parameters: ",
      paste(dput(set_args[invalid]), collapse = ", ")
    )
  }
  
  
  updated <- possible_args %in% set_args
  
  if (length(updated) > 0) {
    args <- args[!updated]
    args <- c(args, new_args)
  }
  args
  
}
