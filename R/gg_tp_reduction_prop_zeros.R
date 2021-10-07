#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run_tp_multiplier
#' @param scenario_parameters
#' @return
#' @author Nicholas Tierney
#' @export
gg_tp_reduction_prop_zeros <- function(scenario_df_run_tp_multiplier,
                                       scenario_parameters) {

  cases_tp_reduction <- scenario_df_run_tp_multiplier %>% 
    # preserve existing behaviour
    relocate(time_to_isolation_sims) %>% 
    mutate(
      across(.cols = c(time_to_isolation_sims,
                       time_to_active,
                       time_to_passive),
             .fns = ~map(.x, c))
    ) %>% 
    unnest(
      cols = c(time_to_isolation_sims,
               time_to_active,
               time_to_passive)
    ) %>% 
    left_join(scenario_parameters, by=c("scenario"="value")) %>%
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
    group_by(scenario) %>% 
    summarise(
      avg_days = weighted.mean(time_to_isolation_sims, fraction),
      tp_multiplier = first(tp_multiplier)
    ) %>% 
    ungroup() %>% 
    mutate(tp_reduction = glue("{percent(1 - tp_multiplier, accuracy = 1)} reduction"),
           avg_days = glue("{round(avg_days)} day average"),
           message = glue("{tp_reduction}\n{avg_days}"))
  
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
      title = "Times to isolation from model",
      y = "Cases isolated",
      x = "Days since infection"
    ) + 
    scale_fill_identity() +
    lims(
      x = c(-1,14)
    ) +
    geom_text(data = df_annotate,
              aes(x = Inf, y = Inf, label = message, fill = NULL),
              hjust="inward", vjust="inward")
  

}
