#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param R0
#' @param final_age_bin
#' @param type
#' @return
#' @author Nicholas Tierney
#' @export
baseline_matrix <- function() {

  # construct a next generation matrix for Australia with conmat

  # fit contact model to polymod
  model <- fit_setting_contacts(
    get_polymod_setting_data(),
    population = get_polymod_population()
  )
  
  age_breaks <- c(seq(0, 80, by = 5), Inf)
  
  transmission_matrices <- get_setting_transmission_matrices(
    age_breaks = age_breaks
  )
  
  contact_matrices <- abs_pop_age_lga_2020 %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    mutate(
      country = "Australia"
    ) %>%
    nest(
      population = -country
    ) %>%
    rowwise() %>%
    mutate(
      per_capita_household_size = get_per_capita_household_size(),
      setting_matrices = list(
        predict_setting_contacts(
          contact_model = model,
          population = population,
          per_capita_household_size = per_capita_household_size,
          age_breaks = age_breaks
        )
      ),
      contact_matrices = list(
        setting_matrices[c("home", "school", "work", "other")]
      )
    ) %>%
    pull(contact_matrices) %>%
    pluck(1)
  
  setting_ngms <- mapply(
    "*",
    contact_matrices,
    transmission_matrices,
    SIMPLIFY = FALSE
  )
  
  matrix <- Reduce("+", setting_ngms)
  
  # set names
  bin_names <- age_classes(80)$classes
  dimnames(matrix) <- list(
    bin_names,
    bin_names
  )

  matrix
}
