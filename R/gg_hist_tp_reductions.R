#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tti_tp_reductions
#' @param tti_distributions
#' @param scenario_parameters Dataframe of scenario (display) name, value in data,
#' and color.
#' @return
#' @author dhduncan
#' @export
gg_hist_tp_reductions <- function(tp_reductions, tti_distributions, scenario_parameters) {
  
  df_annotate = tp_reductions %>%
    left_join(scenario_parameters, by=c("ttiq_effectiveness"="value")) %>%
    mutate(message = glue("{percent(tp_reduction, accuracy = 1)} reduction\n{round(avg_days)} day average"),
           `TTIQ effectiveness` = name)
  
  # plot histograms
  tti_distributions %>%
    pivot_longer(
      cols = -days,
      names_to = "ttiq_effectiveness",
      values_to = "pdf"
    ) %>%
    left_join(scenario_parameters, by=c("ttiq_effectiveness"="value")) %>%
    mutate(
      `TTIQ effectiveness` = name
    ) %>%
    ggplot(
      aes(
        x = days + 5,
        y = pdf,
        fill = colour
      )
    ) +
    facet_wrap(
      facets = vars(`TTIQ effectiveness`),
      ncol = 3
    ) +
    geom_vline(
      xintercept = 5,
      linetype = 3,
      col = grey(0.5)
    ) +
    geom_col() +
    coord_cartesian(xlim = c(-1, 13.5)) +
    scale_x_continuous(
      breaks = seq(0, 15, by = 5),
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_identity() +
    xlab("Days since infection") +
    ylab("Cases isolated") +
    theme_cowplot() +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    ) +
    geom_text(data = df_annotate,
              aes(x = Inf, y = Inf, label=message, fill = NULL),
              hjust="inward", vjust="inward") +
    ggtitle("Times to isolation from case data")

}
