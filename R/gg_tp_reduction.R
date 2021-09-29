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
    mutate(
      scenario = factor(
        scenario, 
        levels = c("optimal",
                   "current_case_init",
                   "current"),
        labels = c("Optimal",
                   "NSW Current\nwith case-initiated",
                   "NSW Current\nwithout case-initiated")
      )
    ) %>%
    mutate(
      time_to_isolation_sims = floor(time_to_isolation_sims)
    ) %>%
    group_by(scenario, time_to_isolation_sims) %>%
    summarise(
      count = n(),
      tp_multiplier = first(tp_multiplier),
      .groups = "drop"
    ) %>%
    group_by(scenario) %>%
    mutate(
      fraction = count / sum(count)
    ) %>%
    ungroup() %>%
    mutate(
      scenario_colour = case_when(
        scenario == "Optimal" ~ 1,
        TRUE ~ 2
      ),
      scenario_colour = factor(scenario_colour)
    ) %>%
    ungroup()
    
  
  df_annotate <- cases_tp_reduction %>% 
    group_by(scenario) %>% 
    summarise(
      avg_days = weighted.mean(time_to_isolation_sims, fraction),
      tp_multiplier = first(tp_multiplier),
      scenario_colour = first(scenario_colour)
    ) %>% 
    ungroup() %>% 
    mutate(tp_reduction = glue("{percent(1 - tp_multiplier, accuracy = 1)} reduction"),
           avg_days = glue("{round(avg_days)} day average"),
           message = glue("{tp_reduction}\n{avg_days}")) %>% 
    relocate(tp_reduction) %>% 
    select(tp_reduction,
           scenario,
           scenario_colour,
           avg_days,
           message) %>% 
    distinct() %>% 
    arrange(scenario)
  
  ggplot(cases_tp_reduction,
         aes(
           x = time_to_isolation_sims,
           y = fraction,
           fill = scenario_colour
         )) +
    geom_vline(
      xintercept = 5,
      linetype = 3,
      col = grey(0.5)
    ) +
    geom_col() + 
    facet_wrap(
      ~ scenario,
      ncol = 3
    ) + 
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
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
    scale_fill_manual(values = cols) + 
    lims(
      x = c(-1,14)
    ) +
    geom_label(data = df_annotate,
               aes(x = Inf, y = Inf, label=message), hjust="inward", vjust="inward", color="white", label.size=NA) +
    theme(legend.position = "none") + 
    ggtitle("Times to isolation from model")
}
