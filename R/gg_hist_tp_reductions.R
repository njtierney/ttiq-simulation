#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tti_tp_reductions
#' @return
#' @author dhduncan
#' @export
gg_hist_tp_reductions <- function(tp_reductions, tti_distributions) {
  
  dark2 = brewer.pal(8, "Dark2")
  # Define plotting constants in order
  scenarios = tribble(
    ~name, ~value, ~colour,
    "Optimal (NSW)", "optimal", dark2[1],
    "Partial (VIC)", "partial", dark2[2],
    "NSW Current", "current", dark2[3]
  ) %>%
    mutate(name = fct_inorder(name))
  
  df_annotate = tp_reductions %>%
    left_join(scenarios, by=c("ttiq_effectiveness"="value")) %>%
    mutate(message = glue("{percent(tp_reduction, accuracy = 1)} reduction\n{round(avg_days)} day average"),
           `TTIQ effectiveness` = name)
  
  # plot histograms
  tti_distributions %>%
    pivot_longer(
      cols = c("current", "partial", "optimal"),
      names_to = "ttiq_effectiveness",
      values_to = "pdf"
    ) %>%
    left_join(scenarios, by=c("ttiq_effectiveness"="value")) %>%
    mutate(
      `TTIQ effectiveness` = name
    ) %>%
    ggplot(
      aes(
        days + 5,
        pdf,
        fill = `TTIQ effectiveness`
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
    xlab("Days since infection") +
    ylab("Cases isolated") +
    theme_cowplot() +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    ) +
    scale_fill_brewer(
      palette = "Dark2"
    ) +
    geom_text(data = df_annotate,
              aes(x = Inf, y = Inf, label=message, fill = NULL),
              hjust="inward", vjust="inward") +
    ggtitle("Times to isolation from case data")

}
