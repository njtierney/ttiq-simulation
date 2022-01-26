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
                                 initial_infections = 500, #100 for 70 and 80%, 1000 for 90%
                                 min_infections = 100,
                                 min_days = 0, #20,
                                 max_infections = Inf, #min_infections * 100,
                                 max_days = 50, #min_days * 5, max_days 70% 20-50 days, 80% 50-80, 90% 10-300 
                                 exclude_days_start = 7, # avoid lag
                                 exclude_days_end = 14, # avoid lead
                                 max_tries = 20) {
  
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
    
    if (valid_infections > 0) {
      valid_days <- diff(range(sim_valid$infection_day))
    } else {
      valid_days <- 0
    }
      
    # check if this is enough
    successful <- valid_infections > min_infections &
      valid_days > min_days
    
  }
  
  if (try >= max_tries) {
    stop ("insufficient samples", valid_infections, " infections", valid_days, " days")
  }
  
  # return *all* the samples, since new infections in end period count toward
  # denominator of TP
  sim
  
}

