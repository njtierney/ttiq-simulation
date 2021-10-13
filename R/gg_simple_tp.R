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
           tp_multiplier,
           tp_multiplier_if_found) %>% 
    pivot_longer(
      cols = -scenario,
      names_to = "tp_type",
      values_to = "tp_multiplier"
    ) %>% 
    mutate(tp_reduction = 1 - tp_multiplier) %>% 
    ggplot(aes(x = tp_reduction,
               y = reorder(scenario, tp_reduction),
               fill = tp_type)) + 
    geom_col(position = "dodge") + 
    scale_fill_brewer(palette = "Dark2") +
    scale_x_continuous(labels = label_percent()) + 
    labs(title = "% TP Reduction over all scenarios",
         subtitle = "Ordered by largest % TP reduction",
         x = "% TP reduction",
         y = "Scenario",
         fill = "Type") 

}
