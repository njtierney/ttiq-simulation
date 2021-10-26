#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Nicholas Tierney
#' @export
extract_pkg_names <- function(pkg_path = "packages.R") {

  pkg_details <- readLines("packages.R")
  library_lines <- str_detect(pkg_details, "library\\([a-zA-Z]")
  str_remove_all(pkg_details[library_lines],
                 "library\\(|\\)")
}
