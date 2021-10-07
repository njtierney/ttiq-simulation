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
  
  new_scenario_colours <- tibble::tribble(
    ~scenario,   ~colour,
    "optimal", "#1B9E77",
    "partial", "#D95F02",
    "current_nsw", "#7570B3",
    "current_nsw_case_init_20_pct_zeros", "#7570B3",
    "current_nsw_case_init_40_pct_zeros", "#7570B3",
    "current_nsw_case_init_60_pct_zeros", "#7570B3",
    "current_nsw_case_init_80_pct_zeros", "#7570B3",
    "current_vic", "#E7298A",
    "current_vic_case_init_20_pct_zeros", "#E7298A",
    "current_vic_case_init_40_pct_zeros", "#E7298A",
    "current_vic_case_init_60_pct_zeros", "#E7298A",
    "current_vic_case_init_80_pct_zeros", "#E7298A",
    "current", "#7570B3"
  )
  
  cases_tp_reduction <- scenario_df_run_tp_multiplier %>% 
    relocate(time_to_isolation_sims) %>% 
    # improve the names of the scenarios
    mutate(scenario = case_when(
      is.na(prop_current_case_zero) ~ scenario,
      
      TRUE ~ as.character(glue(
        "{scenario}_{percent(prop_current_case_zero, suffix = '_pct_zeros')}"
      ))
    )) %>% 
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
    # left_join(scenario_parameters, by=c("scenario"="value"))
    mutate(
      # scenario = name,
      time_to_isolation_sims = floor(time_to_isolation_sims)
    ) %>%
    group_by(scenario, 
             # colour, 
             time_to_isolation_sims) %>%
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
    left_join(
      new_scenario_colours, 
      by = "scenario"
    ) %>% 
    mutate(
      scenario = fct_relevel(
        scenario,
        c(
          "optimal",
          "partial",
          "current_nsw",
          "current_vic",
          "current_nsw_case_init_20_pct_zeros",
          "current_vic_case_init_20_pct_zeros",
          "current_nsw_case_init_40_pct_zeros",
          "current_vic_case_init_40_pct_zeros",
          "current_nsw_case_init_60_pct_zeros",
          "current_vic_case_init_60_pct_zeros",
          "current_nsw_case_init_80_pct_zeros",
          "current_vic_case_init_80_pct_zeros"
        )
      )
    ) 
  
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
  
  
  new_labels <- as_labeller(
    c(
      "optimal" = "NSW Optimal",
      "partial" = "VIC Partial",
      "current_nsw" = "NSW Current\nwithout case initiated",
      "current_vic" = "VIC Current\nwithout case initiated",
      "current_vic_case_init_20_pct_zeros" = "VIC Current\nwith 20% case initiated",
      "current_vic_case_init_40_pct_zeros" = "VIC Current\nwith 40% case initiated",
      "current_vic_case_init_60_pct_zeros" = "VIC Current\nwith 60% case initiated",
      "current_vic_case_init_80_pct_zeros" = "VIC Current\nwith 80% case initiated",
      "current_nsw_case_init_20_pct_zeros" = "NSW Current\nwith 20% case initiated",
      "current_nsw_case_init_40_pct_zeros" = "NSW Current\nwith 40% case initiated",
      "current_nsw_case_init_60_pct_zeros" = "NSW Current\nwith 60% case initiated",
      "current_nsw_case_init_80_pct_zeros" = "NSW Current\nwith 80% case initiated"
    )
  )
  
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
      ncol = 2,
      labeller = new_labels
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
