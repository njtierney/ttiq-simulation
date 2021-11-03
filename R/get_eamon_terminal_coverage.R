#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage_age_group_at_milestone
#' @return
#' @author Nick Golding
#' @export
get_eamon_terminal_coverage <- function(vaccination_coverage_age_group_at_milestone) {

  output <- vaccination_coverage_age_group_at_milestone %>%
    filter(
      milestone %in% c(
        "over_16_over_70_pct",
        "over_16_over_80_pct",
        "terminal"
      )
    ) %>%
    ungroup() %>%
    select(
      milestone,
      age_band = age_band_id,
      any_vaccine,
      ends_with("coverage")
    )
  
  # check that the coverages sum to any_vaccine
  output %>%
    rowwise() %>%
    mutate(
      check_sum = abs(sum(across(ends_with("coverage"))) - any_vaccine) < 1e-3,
      .before = everything()
    ) %>%
    ungroup() %>%
    summarise(
      across(
        check_sum,
        all
      )
    ) %>%
    pull(check_sum) %>%
    stopifnot()
      
    output
          
}
