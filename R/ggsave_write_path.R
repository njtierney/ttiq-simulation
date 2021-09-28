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
ggsave_write_path <- function(plot, path) {

  ggsave(
    plot = plot,
    filename = path,
    bg = "white",
    height = 4,
    width = 5
  )

  path  
}
