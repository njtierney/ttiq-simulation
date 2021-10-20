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
infect <- function(infections, day) {
  
  # add on new infections for this timesteps, infecting the vaccinated and
  # unvaccinated populations separately
  rbind(
    infections,
    new_infections(infections, day, vaccinated = TRUE),
    new_infections(infections, day, vaccinated = FALSE)
  )
  
}
