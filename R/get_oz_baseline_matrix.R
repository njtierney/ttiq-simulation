#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_limits
#' @return
#' @author Nicholas Tierney
#' @export
get_oz_baseline_matrix <- function(age_limits = c(seq(0, 80, by = 5), Inf)) {

  setting_models <- fit_setting_contacts(
    contact_data_list = get_polymod_setting_data(),
    population = get_polymod_population()
  )
  
  australia_contact_matrix <- conmat::abs_pop_age_lga_2020 %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    predict_setting_contacts(
      contact_model = setting_models,
      population = .,
      age_breaks = age_limits
    )
  
  # apply age-based susceptibility and infectiousness from Davies et al.
  
  australia_ngm_unscaled <- apply_age_contribution(australia_contact_matrix$all)
  
  m <- find_m(
    R_target = 3.6,
    transition_matrix = australia_ngm_unscaled
  )
  
  australia_ngm <- australia_ngm_unscaled * m
  
  australia_ngm

}
