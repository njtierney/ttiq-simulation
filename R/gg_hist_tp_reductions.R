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

  df_annotate <- tp_reductions %>%
    rename(
      `TTIQ effectiveness` = ttiq_effectiveness
    ) %>%
    mutate(
      `TTIQ effectiveness` = paste(
        stringr::str_to_sentence(`TTIQ effectiveness`),
        "TTIQ"
      ),
      `TTIQ effectiveness` = factor(
        `TTIQ effectiveness`,
        levels = c("Optimal TTIQ", "Partial TTIQ", "Current TTIQ"),
        labels = c("Optimal",
                   "Partial",
                   "NSW Current" 
        )
      ),
      x = 12,
      y = 0.1,
      message = glue("{percent(tp_reduction, accuracy = 1)} reduction\n{round(avg_days)} day average")
    )
  
#  cols = scales::hue_pal()(3)[1:3]
  
  # plot histograms
  tti_distributions %>%
    pivot_longer(
      cols = c("current", "partial", "optimal"),
      names_to = "TTIQ effectiveness",
      values_to = "pdf"
    ) %>%
    mutate(
      colour = case_when(
        `TTIQ effectiveness` == "optimal" ~ 1,
        `TTIQ effectiveness` == "partial" ~ 2,
        `TTIQ effectiveness` == "current" ~ 3,
      ),
      `TTIQ effectiveness` = paste(
        stringr::str_to_sentence(`TTIQ effectiveness`),
        "TTIQ"
      )
    ) %>%
    mutate(
      `TTIQ effectiveness` = factor(
        `TTIQ effectiveness`,
        levels = c("Optimal TTIQ", "Partial TTIQ", "Current TTIQ"),
        labels = c("Optimal",
                   "Partial",
                   "NSW Current" 
        )
      )
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
    theme_minimal() +
    theme(
      legend.position = "none"
    ) +
    scale_fill_brewer(
      palette = "Dark2"
    ) +
    geom_text(
      aes(
        x = x,
        y = y,
        label = message
      ),
      data = df_annotate
    )+ 
    ggtitle("Times to isolation from case data")

}
