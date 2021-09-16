#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples_against_data
#' @return
#' @author Nicholas Tierney
#' @export
gg_nsw_tp_reduction <- function(scenario_df_run_tp_multiplier) {

  cases_tp_reduction <- scenario_df_run_tp_multiplier %>% 
    relocate(time_to_isolation_sims) %>% 
    mutate(time_to_isolation_sims = map(time_to_isolation_sims, c)) %>% 
    unnest(cols = time_to_isolation_sims) %>% 
    mutate(scenario = as_factor(scenario),
           scenario = fct_relevel(scenario,
                                  "optimal",
                                  "current"))
  
  df_annotate <- cases_tp_reduction %>% 
    group_by(scenario) %>% 
    mutate(avg_days = mean(time_to_isolation_sims)) %>% 
    ungroup() %>% 
    mutate(tp_reduction = glue("{percent(1 - tp_multiplier)} reduction"),
           avg_days = glue("{round(avg_days)} day average"),
           message = glue("{tp_reduction}\n{avg_days}")) %>% 
    relocate(tp_reduction) %>% 
    select(tp_reduction,
           scenario,
           avg_days,
           message) %>% 
    distinct() %>% 
    mutate(x = c(13, 2, 2),
           y = c(3200, 100, 100))
  
  ggplot(cases_tp_reduction,
         aes(x = time_to_isolation_sims,
             fill = scenario)) +
    geom_histogram(colour = "white") + 
    facet_wrap(~ scenario,
               ncol = 1,
               scales = "free_y") + 
    theme_bw() + 
    labs(
      fill = "",
      y = "Count",
      x = "# days from isolation to infection"
    ) + 
    scale_fill_brewer(
      palette = "Dark2"
    ) + 
    lims(
      x = c(0,14)
    ) + 
    geom_text(data = df_annotate,
              aes(x = x, y = y, label = message)) +
    theme(legend.position = "none")

}
