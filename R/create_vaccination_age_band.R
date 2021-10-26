#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccinations_raw
#' @return
#' @author Nicholas Tierney
#' @export
create_vaccination_age_band <- function(vaccinations) {

  original_age_bands <- c(
    "00-11",
    sort(unique(vaccinations$age_band_id))
  )
  
  vaccinations_age_band <- tibble(
    original_age_bands = original_age_bands,
    lower_age_band = parse_number(str_sub(original_age_bands, 1, 2)),
    upper_age_band = parse_number(str_sub(original_age_bands, 4))
  ) %>% 
    mutate(upper_age_band = coalesce(upper_age_band, Inf))
  
  vaccinations_age_band

}
