#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ggplot
#' @param width
#' @param height
#' @param bg
#' @param dpi
#' @return
#' @author Nicholas Tierney
#' @export
ggsave_fig <- function(ggplot, width = 12, height = 5, bg = "white", dpi =
                       300) {

  dir_create("figs")
  
  ggsave(
    filename = here(glue("figs/{ggplot}.png")),
    plot = ggplot,
    width = 12,
    height = 5,
    bg = "white"
  )

}
