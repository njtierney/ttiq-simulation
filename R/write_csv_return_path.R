#' write_csv but for targets
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
  
  dir <- dirname(file)
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  write_csv(x = x,
            file = file,
            ...)
  
  return(
    here(file)
  )
  
}
