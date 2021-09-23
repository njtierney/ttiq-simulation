#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
read_susceptibility_clinical_fraction_age <- function() {

  read_csv(
    "data-public/susceptibility_clinical_fraction_age_Davies.csv",
    col_types = cols(
      age_group = col_character(),
      rel_susceptibility_mean = col_double(),
      rel_susceptibility_median = col_double(),
      clinical_fraction_mean = col_double(),
      clinical_fraction_median = col_double()
    )
  )

}
