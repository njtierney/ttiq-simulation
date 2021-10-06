#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
get_australian_pop <- function() {

  conmat::abs_pop_age_lga_2020 %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = parse_number(as.character(age_group))
    ) %>%
    mutate(
      country = "Australia"
    )

}
