#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ve_onward_transmission
#' @param ve_susceptibility
#' @param ve_symptoms
#' @param vaccination_coverage
#' @return
#' @author dhduncan
#' @export
get_clinical_fraction_scenarios <- function(...) {

  expand_grid(...) %>%
    rowwise() %>%
    # compute age coverage of vaccination
    mutate(
      # why encode the fractional coverage of vaccine age groups with clear,
      # readable code, when you can use obscure mathematics?
      min_age_bracket = vaccination_age_min %/% 5 + 1,
      vaccination_coverage_vec = list(
        vaccination_coverage * (sign(1:17 - min_age_bracket) + 1) / 2
      )
    )

}
