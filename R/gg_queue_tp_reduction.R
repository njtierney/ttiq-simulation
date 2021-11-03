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
      capacity != "capacity0pc",
      vaccination == "casesvaccinated45pc",
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
        
      )
    ) %>%
    select(
      priority_type,
      capacity,
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
    geom_col(
      width = 0.8,
      fill = "steelblue2"
    ) +
    scale_y_continuous(
      label = scales::percent
    ) +
    facet_wrap( ~ capacity, nrow = 1) +
    ylab("Reduction in TP") +
    xlab("Tracing prioritisation strategy") +
    theme_cowplot() +
    theme(
      strip.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 9)
    )

  
}
