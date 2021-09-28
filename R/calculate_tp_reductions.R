#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tti_distributions
#' @return
#' @author dhduncan
#' @export
calculate_tp_reductions <- function(tti_distributions) {

  # calculate tp reductions
  tp_reductions <- tti_distributions %>%
    mutate(
      gi_pmf = gi_pmf_discrete(
        days + 5
      )
    ) %>%
    pivot_longer(
      cols = c(partial, optimal, current),
      names_to = "ttiq_effectiveness",
      values_to = "delay_pmf"
    ) %>%
    group_by(
      ttiq_effectiveness
    ) %>%
    arrange(
      ttiq_effectiveness,
      days
    ) %>%
    summarise(
      avg_days = weighted.mean(days + 5, delay_pmf),
      tp_reduction = round(1 - weighted.mean(1 - cumsum(delay_pmf), gi_pmf), 2)
    )
  
  tp_reductions

}
