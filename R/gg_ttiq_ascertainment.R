#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
gg_ttiq_ascertainment <- function(
  scenario_parameters
) {
  
  optimal_colour <- scenario_parameters %>%
    filter(
      value == "optimal"
    ) %>%
    pull(colour)

  tibble::tibble(
    ascertainment = seq(1, 0.2, by = -0.2),
    ttiq_effect_cases = 0.54
  ) %>%
    mutate(
      ttiq_effect = ttiq_effect_cases * ascertainment,
      ascertainment = factor(
        ascertainment,
        levels = unique(ascertainment),
        labels = paste0(100 * unique(ascertainment), "%")
      )
    ) %>%
    ggplot(
      aes(
        x = ascertainment,
        y = ttiq_effect
      )
    ) +
    scale_x_discrete() +
    scale_y_continuous(
      breaks = c(0, 0.2, 0.42, 0.54),
      labels = scales::percent
    ) +
    geom_hline(
      yintercept = c(0.54, 0.42),
      linetype = 3
    ) +
    geom_col(
      fill = optimal_colour,
      width = 0.8
    ) +
    theme_cowplot() +
    ylab("Reduction in TP") +
    xlab("Proportion of infections detected") +
    annotate(
      "text",
      x = 5, 
      y = c(0.43, 0.54) + 0.02,
      label = c("partial TTIQ", "optimal TTIQ")
    )

}
