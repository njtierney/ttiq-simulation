#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot
#' @param path
#' @return
#' @author dhduncan
#' @export
ggsave_write_path <- function(plot, path, width = 5, height = 5) {

  ggsave(
    plot = plot,
    filename = path,
    bg = "white",
    height = height,
    width = width
  )

  path  
}
