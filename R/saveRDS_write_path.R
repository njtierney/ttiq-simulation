#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param object
#' @param path
#' @param 
#' @return
#' @author Nick Golding
#' @export
saveRDS_write_path <- function(object, path, ...) {

  saveRDS(object, path, ...)

  path
  
}
