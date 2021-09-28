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
                                        "NSW Current\nwith case-initiated",
                                        "NSW Current\nwithout case-initiated"))
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
    summarise(
      avg_days = weighted.mean(time_to_isolation_sims, fraction),
      tp_multiplier = first(tp_multiplier)
    ) %>% 
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
    mutate(
      x = 14,
      y = 0.12
    )


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
               ncol = 3
               # labeller = label_wrap_gen(width=15)
               ) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_cowplot() +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    ) +
    labs(
      fill = "",
      y = "Cases isolated",
      x = "Days since infection"
    ) + 
    scale_fill_brewer(palette = "Dark2") + 
    lims(
      x = c(-1,14)
    ) +
    geom_text(data = df_annotate,
              aes(x = x, y = y, label = message),
              size = 4,
              hjust = 1,
              vjust = 0
    ) +
    theme(legend.position = "none") + 
    ggtitle("Times to isolation from model")

}
