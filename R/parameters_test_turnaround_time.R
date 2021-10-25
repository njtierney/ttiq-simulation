#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param derived_delay_distributions
#' @return
#' @author Nicholas Tierney
#' @export
parameters_test_turnaround_time <- function(derived_delay_distributions) {

  df <- expand_grid(
    scenario = unique(derived_delay_distributions$scenario),
    days = 0:14
  ) %>%
    full_join(
      derived_delay_distributions,
      by = "scenario"
    ) %>%
    select(
      -prop_current_case_zero,
      -dist_isol_swab
    ) %>%
    mutate(
      dist_isolation_swab = dist_poisson(0.5),
      .after = days
    ) %>%
    mutate(
      dist_interview_isolation = dist_poisson(0.5),
      .after = everything()
    ) %>%
    rowwise() %>%
    summarise(
      across(
        c(scenario, days),
        first
      ),
      across(
        starts_with("dist"),
        ~density(.x, days)
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("dist"),
      names_to = "distribution",
      values_to = "density",
      names_prefix = "dist_"
    ) %>%
    relocate(
      distribution,
      .after = scenario
    ) %>%
    # normalise to account for any days lost in the upper truncation
    group_by(
      scenario,
      distribution
    ) %>%
    mutate(
      density = density / sum(density)
    ) %>%
    ungroup()
  
  # df %>%
  #   ggplot(
  #     aes(
  #       x = days,
  #       y = density
  #     )
  #   ) +
  #   geom_col() +
  #   facet_grid(
  #     scenario ~ distribution
  #   )
  
  df

}
