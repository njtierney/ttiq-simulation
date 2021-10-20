#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
sim_initial_infections <- function(n = 100) {

  infections <- data.frame(
    id = seq_len(n),
    source_id = NA,
    infection_day = ceiling(runif(n, -7, 0)),
    isolation_day = Inf,
    vaccinated = rbinom(n, 1, .abm_parameters$vaccination_coverage),
    symptomatic = rbinom(n, 1, .abm_parameters$clinical_fraction)
  )

}
