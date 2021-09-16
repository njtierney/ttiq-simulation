#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot_nsw_delay_samples_against_data
#' @param plot_nsw_tp_reduction
#' @return
#' @author Nicholas Tierney
#' @export
ggsave_these_plots <- function(plot_nsw_delay_samples_against_data,
                               plot_nsw_tp_reduction) {

  ggsave_fig(ggplot = plot_nsw_delay_samples_against_data,
             width = 12,
             height = 5,
             bg = "white",
             dpi = 300)
  
  ggsave_fig(ggplot = plot_nsw_tp_reduction,
             width = 12,
             height = 5,
             bg = "white",
             dpi = 300)

}
