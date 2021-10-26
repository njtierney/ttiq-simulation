#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param efficacy_susceptibility
#' @param efficacy_onward
#' @param coverage_any_vaccine
#' @return
#' @author Nicholas Tierney
#' @export
get_stable_state <- function(efficacy_susceptibility,
                             efficacy_onward,
                             coverage_any_vaccine,
                             oz_baseline_matrix) {
  
  # coverage_any_vaccine <- unlist(coverage_any_vaccine)
  # transmission between unvaccinated people, no effect of vaccines and scale down
  # to vaccinated population
  unvax_unvax <- oz_baseline_matrix %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  # transmission between vaccinated people, susceptibility and onward transmission
  # effects and scale down to vaccinated population
  vax_vax <- oz_baseline_matrix %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from unvaccinated to vaccinated people (account for
  # susceptibility effects on rows) and scale down to vaccinated population
  # fraction
  unvax_vax <- oz_baseline_matrix %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from vaccinated to unvaccinated people (account for transmission
  # effects) and scale down to unvaccinated population
  vax_unvax <- oz_baseline_matrix %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  vax_structured_matrix <- rbind(cbind(unvax_unvax, vax_unvax),
                                 cbind(unvax_vax, vax_vax))
  
  # set the names, for no reall reason
  names <- paste(
    rep(c("unvax", "vax"), each = 17),
    rownames(vax_structured_matrix)
  )
  rownames(vax_structured_matrix) <- colnames(vax_structured_matrix) <- names
  
  # compute the stable state
  eigen_decomposition <- eigen(vax_structured_matrix, symmetric = FALSE)
  dominant_eigenvector <- Re(eigen_decomposition$vectors[, 1])
  
  # reorientate negative eigenvectors
  direction <- sign(dominant_eigenvector[abs(dominant_eigenvector) == max(abs(dominant_eigenvector))])
  stable_state <- dominant_eigenvector * direction
  
  return(stable_state)
  
}

