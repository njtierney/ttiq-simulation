#' write_csv but for targets
write_csv_return_path <- function(x, file, ...) {
  write_csv(x = x, file = file, ...)
  return(here(file))
}