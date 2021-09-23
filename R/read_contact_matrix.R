#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
read_contact_matrix <- function() {

  read_xlsx(
    path = "data-public/MUestimates_all_locations_1.xlsx",
    sheet = "Australia",
    col_types = rep("numeric", 16)
  ) %>%
    as.matrix()

}
