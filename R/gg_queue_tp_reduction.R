#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run_tp_multiplier_queue
#' @return
#' @author Nick Golding
#' @export
gg_queue_tp_reduction <- function(scenario_df_run_tp_multiplier_queue) {

  df_plot <- scenario_df_run_tp_multiplier_queue %>%
    filter(
      capacity != "capacity0pc"
    ) %>%
    mutate(
      priority_type = factor(
        priority_type,
        levels = c("random",
                   "new_swab",
                   "vaccine_new_swab",
                   "new_swab_vaccine"),
        labels = c("Random",
                   "New\ncases",
                   "Unvaccinated\nthen new\n cases",
                   "New cases\nthen\nunvaccinated")
      ),
      capacity = factor(
        capacity,
        levels = c("capacity20pc",
                   "capacity50pc",
                   "capacity80pc"),
        labels = c("20% capacity",
                   "50% capacity",
                   "80% capacity")
        
      ),
      vaccination = factor(
        vaccination,
        levels = c("casesvaccinated25pc", "casesvaccinated50pc"),
        labels = c("25% of cases vaccinated", "50% of cases vaccinated"),
      )
    ) %>%
    select(
      priority_type,
      capacity,
      vaccination,
      tp_multiplier,
      tp_multiplier_if_found
    ) %>%
    mutate(
      tp_reduction = 1 - tp_multiplier,
      tp_reduction_if_found = 1 - tp_multiplier_if_found
    )

  df_plot %>%
    ggplot(
      aes(
        x = priority_type,
        y = tp_reduction
      )
    ) +
    geom_hline(
      yintercept = c(0.54, 0.42),
      linetype = 2
    ) +
    geom_col(
      width = 0.8,
      fill = "skyblue"
    ) +
    scale_y_continuous(
      breaks = c(0, 0.20, 0.42, 0.54),
      label = scales::percent
    ) +
    facet_grid(vaccination ~ capacity) +
    ylab("Reduction in onward transmission due to contact tracing") +
    xlab("Tracing prioritisation strategy") +
    theme_cowplot() +
    theme(
      strip.background = element_rect(fill = "white")
    ) +
    geom_text(
      aes(
        label = line_label
      ),
      data = tibble(
        capacity = "20% capacity",
        vaccination = c("25% of cases vaccinated",
                        "25% of cases vaccinated",
                        "50% of cases vaccinated",
                        "50% of cases vaccinated"),
        priority_type = "Random",
        line_label = c("Partial TTIQ",
                  "Optimal TTIQ",
                  "Partial TTIQ",
                  "Optimal TTIQ"),
        tp_reduction = c(0.42, 0.54, 0.42, 0.54) + 0.03
      ),
      nudge_x = -0.4,
      size = 4,
      hjust = 0
    )
  
}
