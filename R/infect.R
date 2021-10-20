#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param infections
#' @param day
#' @return
#' @author Nick Golding
#' @export
infect <- function(infections, parameters, day) {
  
  # simulate new infections for each of these
  onward_infections <- rpois(
    nrow(infections),
    infectiousness(infections, parameters, day)
  )
  
  n_new <- sum(onward_infections)
  
  if (n_new > 0) {
    new_infections <- data.frame(
      id = max(infections$id) + seq_len(n_new),
      source_id = rep(infections$id, onward_infections),
      infection_day = day,
      isolation_day = Inf,
      vaccinated = rbinom(n_new, 1, parameters$vaccination_coverage),
      symptomatic = rbinom(n_new, 1, parameters$clinical_fraction)
    )
  } else {
    new_infections <- NULL
  }
  
  rbind(
    infections,
    new_infections
  )
  
}
