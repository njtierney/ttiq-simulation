#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run_tp_multiplier
#' @return
#' @author Nicholas Tierney
#' @export
gg_simple_tp <- function(scenario_df_run_tp_multiplier) {

  scenario_df_run_tp_multiplier %>% 
    select(scenario,
           tp_multiplier) %>% 
    mutate(tp_reduction = 1 - tp_multiplier) %>% 
    ggplot(aes(x = tp_reduction,
               y = reorder(scenario, tp_reduction))) + 
    geom_col(width = 0.1,
             colour = "steelblue",
             fill = "steelblue") + 
    geom_point(size = 3,
               colour = "steelblue") +
    scale_x_continuous(labels = label_percent()) + 
    labs(title = "% TP Reduction over all scenarios",
         subtitle = "Ordered by largest % TP reduction",
         x = "% TP reduction",
         y = "Scenario") +
    theme_minimal()

}
