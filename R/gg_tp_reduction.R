#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run_tp_multiplier
#' @return
#' @author Nicholas Tierney
#' @export
gg_tp_reduction <- function(scenario_df_run_tp_multiplier) {

  cases_tp_reduction <- scenario_df_run_tp_multiplier %>% 
    relocate(time_to_isolation_sims) %>% 
    mutate(time_to_isolation_sims = map(time_to_isolation_sims, c)) %>%
    unnest(cols = time_to_isolation_sims) %>% 
    mutate(scenario = factor(scenario, 
                             levels = c("optimal",
                                        "current_case_init",
                                        "current"),
                             labels = c("Optimal",
                                        "Current with case-initiated",
                                        "Current without case-initiated")),
          ) %>%
    mutate(time_to_isolation_sims = floor(time_to_isolation_sims)) %>%
    group_by(scenario, time_to_isolation_sims) %>%
    summarise(
      tp_multiplier = first(tp_multiplier),
      count = n(),
        ) %>%
    drop_na %>%
    mutate(
      fraction = count/sum(count)
    ) %>%
    ungroup()
    
  
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
    arrange(scenario) %>% 
    mutate(x = c(13, 1, 13),
           y = c(.15, .15, .15))
  
  ggplot(cases_tp_reduction,
         aes(x = time_to_isolation_sims, y = fraction,
             fill = scenario)) +
    geom_vline(
      xintercept = 5,
      linetype = 3,
      col = grey(0.5)
      ) +
    geom_col() + 
    facet_wrap(~ scenario,
               ncol = 3) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() + 
    labs(
      fill = "",
      y = "Cases isolated",
      # x = "# days from isolation to infection"
      x = "Days since infection"
    ) + 
    scale_fill_brewer(
      palette = "Dark2"
    ) + 
    lims(
      x = c(-1,14)
    ) +
    geom_text(data = df_annotate,
              aes(x = x, y = y, label = message)) +
    theme(legend.position = "none") + 
    ggtitle("Times to isolation from model prediction")

}
