#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param file
#' @return
#' @author Nicholas Tierney
#' @export
write_csv_return_path <- function(x, 
                                  file, 
                                  ...) {
  write_csv(x = x,
            file = file,
            ...)
  
  return(
    here(file)
  )
  
}
