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
  
  dark2 = brewer.pal(8, "Dark2")
  # Define plotting constants in order
  scenarios = tribble(
    ~name, ~value, ~colour,
    "Optimal (NSW)", "optimal", dark2[1],
    "Partial (VIC)", "partial", dark2[2],
    "NSW Current\nwith case-initiated", "current_nsw_case_init", dark2[3],
    "NSW Current\nwithout case-initiated", "current_nsw", dark2[3],
    "VIC Current\nwith case-initiated", "current_vic_case_init", dark2[4],
    "VIC Current\nwithout case-initiated", "current_vic", dark2[4]
  ) %>%
    mutate(name = fct_inorder(name))
  
  cases_tp_reduction <- scenario_df_run_tp_multiplier %>% 
    relocate(time_to_isolation_sims) %>% 
    mutate(time_to_isolation_sims = map(time_to_isolation_sims, c)) %>%
    unnest(cols = time_to_isolation_sims) %>% 
    left_join(scenarios, by=c("scenario"="value")) %>%
    mutate(
      scenario = name,
      time_to_isolation_sims = floor(time_to_isolation_sims)
    ) %>%
    group_by(scenario, colour, time_to_isolation_sims) %>%
    summarise(
      count = n(),
      tp_multiplier = first(tp_multiplier),
      .groups = "drop"
    ) %>%
    group_by(scenario) %>%
    mutate(
      fraction = count / sum(count)
    ) %>%
    ungroup()
  
  
  df_annotate <- cases_tp_reduction %>% 
    group_by(scenario, colour) %>% 
    summarise(
      avg_days = weighted.mean(time_to_isolation_sims, fraction),
      tp_multiplier = first(tp_multiplier),
      .groups = "drop"
    ) %>% 
    ungroup() %>% 
    mutate(tp_reduction = glue("{percent(1 - tp_multiplier, accuracy = 1)} reduction"),
           avg_days = glue("{round(avg_days)} day average"),
           message = glue("{tp_reduction}\n{avg_days}")) %>% 
    relocate(tp_reduction) %>%
    distinct(tp_reduction,
             scenario,
             avg_days,
             message,
             colour)
  
  ggplot(cases_tp_reduction,
         aes(
           x = time_to_isolation_sims,
           y = fraction,
           fill = colour
         )) +
    geom_vline(
      xintercept = 5,
      linetype = 3,
      col = grey(0.5)
    ) +
    geom_col() + 
    facet_wrap(
      ~ scenario,
      ncol = 2
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
    scale_fill_identity() +
    lims(
      x = c(-1,14)
    ) +
    geom_label(data = df_annotate,
               aes(x = Inf, y = Inf, label=message),
               hjust="inward", vjust="inward", color="white", label.size=NA) +
    theme(legend.position = "none") + 
    ggtitle("Times to isolation from model")
}
