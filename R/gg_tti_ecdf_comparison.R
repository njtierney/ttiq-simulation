#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tti_distributions
#' @return
#' @author dhduncan
#' @export
gg_tti_ecdf_comparison <- function(tti_distributions) {

  tti_distributions %>%
    mutate(
      days = days + 5 - 0.5,
      across(
        c(optimal, partial, current),
        cumsum
      )
    ) %>%
    filter(
      days <= 27
    ) %>%
    pivot_longer(
      cols = c("current", "optimal"),
      names_to = "TTIQ effectiveness",
      values_to = "cdf"
    ) %>%
    mutate(
      `TTIQ effectiveness` = factor(
        `TTIQ effectiveness`,
        levels = rev(unique(`TTIQ effectiveness`))
      )
    ) %>%
    ggplot(
      aes(
        days, cdf, col = `TTIQ effectiveness`
      )
    ) +
    geom_vline(
      xintercept = 5,
      linetype = 3,
      col = grey(0.5)
    ) +
    annotate(
      geom = "text",
      x = 5 + 0.2,
      y = 0,
      hjust = 0,
      vjust = 0,
      label = "Symptom\nonset",
      col = grey(0.5),
      size = 3
    ) +
    geom_step() +
    geom_text(
      aes(
        x = days + 0.5,
        label = scales::percent(cdf, accuracy = 1),
        vjust = ifelse(
          `TTIQ effectiveness` == "optimal",
          -1, 2)
      ),
      size = 2.5
    ) +
    annotate(
      "text",
      x = 2,
      y = 0.75,
      label = "optimal TTIQ",
      col = scales::hue_pal()(2)[1]
    ) +
    annotate(
      "text",
      x = 8,
      y = 0.5,
      label = "current TTIQ",
      col = scales::hue_pal()(2)[2]
    ) +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 10)) +
    scale_y_continuous(labels = scales::percent) +
    xlab("Days since infection") +
    ylab("Cases isolated") +
    theme_minimal() +
    theme(legend.position = "none")
  
}
