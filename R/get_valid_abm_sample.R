#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param parameters
#' @return
#' @author Nick Golding
#' @export
get_valid_abm_sample <- function(parameters,
                                 initial_infections = 100,
                                 min_infections = 1000,
                                 min_days = 30,
                                 max_infections = min_infections * 10,
                                 max_days = 365,
                                 exclude_days_start = 14,
                                 exclude_days_end = 14,
                                 max_tries = 10) {
  
  # run the abm multiple times to make sure there are sufficient useful samples to
  # analyse
  
  try <- 0
  successful <- FALSE
  while (!successful & try < max_tries) {
    try <- try + 1
    
    sim <- sim_abm(
      infections = sim_initial_infections(initial_infections),
      parameters = parameters,
      max_infections = max_infections,
      max_days = max_days
    )
    
    # count the number of useful infections and days there are
    sim_valid <- sim %>%
      filter(
        infection_day > exclude_days_start &
          infection_day < (max(infection_day) - exclude_days_end)
      )  
    
    valid_infections <- nrow(sim_valid)
    valid_days <- diff(range(sim_valid$infection_day))
    
    # check if this is enough
    successful <- valid_infections > min_infections &
      valid_days > min_days
    
  }
  
  if (try > max_tries) {
    stop ("insufficient samples")
  }
  
  # return *all* the samples, since new infections in end period count toward
  # denominator of TP
  sim_valid
  
}

