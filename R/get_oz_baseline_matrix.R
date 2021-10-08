#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_limits
#' @return
#' @author Nicholas Tierney
#' @export
get_oz_baseline_matrix <- function(population,
                                   age_limits = c(seq(0, 80, by = 5), Inf)) {

  setting_models <- fit_setting_contacts(
    contact_data_list = get_polymod_setting_data(),
    population = get_polymod_population()
  )
  
  oz_pop <- conmat::abs_pop_age_lga_2020 %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    )
  
  contact_matrices <- predict_setting_contacts(
    contact_model = setting_models,
    population = oz_pop,
    age_breaks = age_limits,
    per_capita_household_size = get_per_capita_household_size()
  )
  
  # apply age-based susceptibility and infectiousness from Davies et al.
  
  # australia_ngm_unscaled <- apply_age_contribution(australia_contact_matrix$all)
  
  # remove the 'all' matrix, keep the other four settings
  contact_matrices <- australia_contact_matrix[c("home", "school", "work", "other")]

  # get setting-specific transmission probability matrices for the same age
  # aggregations
  transmission_matrices <- get_setting_transmission_matrices(
    age_breaks = age_limits
  )
  
  # remove the 'all' matrix, keep the other four settings
  contact_matrices <- contact_matrices[names(transmission_matrices)]
  
  # combine them to get setting-specific (unscaled) next-generation matrices
  next_generation_matrices <- mapply(
    FUN = `*`,
    contact_matrices,
    transmission_matrices,
    SIMPLIFY = FALSE
  )
  # get the all-settings NGM
  ngm_overall <- Reduce("+", next_generation_matrices)
  
  m <- find_m(
    R_target = 3.6,
    transition_matrix = ngm_overall
  )
  
  australia_ngm <- ngm_overall * m
  
  australia_ngm

}
# 
# not sure how to add these next parts from
# https://github.com/njtierney/conmat/blob/master/R/get_setting_transmission_matrices.R#L15-L52
# into this code
# # remove the 'all' matrix, keep the other four settings
# contact_matrices <- contact_matrices[c("home", "school", "work", "other")]
# 
# # get setting-specific transmission probability matrices for the same age
# # aggregations
# transmission_matrices <- get_setting_transmission_matrices(
#   age_breaks = age_breaks
# )
# 
# # combine them to get setting-specific (unscaled) next-generation matrices
# next_generation_matrices <- mapply(
#   FUN = `*`,
#   contact_matrices,
#   transmission_matrices,
#   SIMPLIFY = FALSE
# )
# 
# # get the all-settings NGM
# ngm_overall <- Reduce("+", next_generation_matrices)