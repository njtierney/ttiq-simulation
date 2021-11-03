#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
gg_epi_curve_example <- function(sims) {
  
  # compute the TP at the baseline setting
  baseline_tp <- sims %>%
    group_by(scenario) %>%
    rowwise() %>%
    summarise(
      compute_abm_metrics(simulations)
    ) %>%
    filter(
      metric == "TP"
    ) %>%
    arrange(
      statistic,
      scenario
    ) %>%
    filter(
      scenario == "baseline",
      statistic == "mean"
    ) %>%
    pull(
      value
    )
  
  # compute an epi curve for each scenario and simulation
  curves <- sims %>%
    unnest(
      simulations
    ) %>%
    group_by(
      scenario,
      infection_day,
      simulation
    ) %>%
    summarise(
      n_infections = n(),
      .groups = "drop"
    ) %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c("baseline", "+1%", "+5%", "+10%"),
        labels = c(
          paste("TP =", round(baseline_tp, 2)), "+1%", "+5%", "+10%"),
        ),
      infection_day = infection_day - 14
    ) %>%
    filter(
      infection_day >= 0
    )
  
  # # compute a mean epi curves for each scenario
  # means <- curves %>%
  #   group_by(
  #     scenario, infection_day
  #   ) %>%
  #   summarise(
  #     across(
  #       n_infections,
  #       mean
  #     ),
  #     .groups = "drop"
  #   )

  # plot curves in various colours with means overlaid
  curves %>%
    filter(
      simulation %in% paste0("sim_", 1:10)
    ) %>%
    ggplot(
      aes(
        x = infection_day,
        y = n_infections,
      )
    ) +
    geom_line(
      aes(
        colour = simulation
      ),
      size = 0.5
    ) +
    facet_wrap(
      ~ scenario,
      ncol = 2
    ) +
    coord_cartesian(
      xlim = c(0, 100)
    ) +
    # geom_line(
    #   data = means,
    #   aes(
    #     x = infection_day,
    #     y = n_infections
    #   ),
    #   colour = "black",
    #   size = 0.5
    # ) +
    theme_cowplot() +
    ylab("New infections") +
    xlab("Day") +
    scale_colour_brewer(
      type = "qual",
      palette = "Paired"
    ) +
    theme(
      strip.background = element_rect(fill = "white"),
      legend.position = "none"
    ) 
  
}
