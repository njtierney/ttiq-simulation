#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccinations
#' @return
#' @author Nicholas Tierney
#' @export
total_vaccinations <- function(vaccinations) {

  vaccinations %>%
    group_by(
      age_band_id,
      vaccine,
      time_dose_1,
      time_dose_2
    ) %>%
    summarise(
      num_people = sum(num_people),
      .groups = "drop"
    ) 

}
