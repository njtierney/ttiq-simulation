#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage
#' @return
#' @author Nicholas Tierney
#' @export
coverage_milestones <- function(vaccination_coverage) {
  
  vaccination_coverage_milestones <-
    vaccination_coverage %>%
    mutate(
      over_16 = !(age_band_id %in% c("0-11", "12-15")),
      over_12 = !(age_band_id %in% c("0-11"))
    ) %>%
    group_by(age_band_id) %>%
    arrange(time_dose_2,
            .by_group = TRUE) %>%
    mutate(cumulative_n_vac = cumsum(n_vaccinated)) %>%
    group_by(time_dose_2) %>%
    summarise(
      coverage_all = sum(cumulative_n_vac) / sum(population),
      coverage_over_16 = sum(cumulative_n_vac * over_16) / sum(population * over_16),
      coverage_over_12 = sum(cumulative_n_vac * over_12) / sum(population * over_12),
      .groups = "drop"
    ) %>%
    mutate(across(
      .cols = c(starts_with("coverage_")),
      .fns = list(
        milestone = ~ case_when(
          .x >= 0.90 ~ "over_90_pct",
          .x >= 0.80 ~ "over_80_pct",
          .x >= 0.70 ~ "over_70_pct",
          .x >= 0.60 ~ "over_60_pct",
          .x >= 0.50 ~ "over_50_pct",
          .x >= 0.40 ~ "over_40_pct",
          .x >= 0.30 ~ "over_30_pct",
          TRUE ~ NA_character_
        )
      )
    )) %>%
    pivot_longer(
      cols = ends_with("milestone"),
      names_to = "eligible_group",
      values_to = "milestone",
      names_prefix = "coverage_"
    ) %>%
    mutate(
      eligible_group = str_remove_all(eligible_group, "_milestone")
    ) %>%
    mutate(
      milestone = case_when(
        !is.na(milestone) ~ as.character(glue("{eligible_group}_{milestone}")),
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(milestone = case_when(
      time_dose_2 == max(time_dose_2, na.rm = TRUE) ~ "terminal",
      TRUE ~ milestone
    )) %>%
    group_by(milestone) %>%
    arrange(
      time_dose_2,
      .by_group = TRUE
    ) %>%
    slice(1) %>%
    filter(
      !is.na(milestone)
    ) %>%
    ungroup() %>%
    rename(
      date = time_dose_2
    ) %>%
    select(
      -eligible_group
    ) 
  
  vaccination_coverage_milestones
}
