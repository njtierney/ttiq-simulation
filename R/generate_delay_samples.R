#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param derive_nsw_delay_distributions
#' @return
#' @author Nicholas Tierney
#' @export
generate_delay_samples <- function(derive_nsw_delay_distributions) {
  generated_samples <- derive_nsw_delay_distributions %>%
    mutate(across(
      .cols = c(starts_with("dist_")),
      .fns = ~ generate(.x, times = 10000),
      .names = "samples_{.col}"
    )) %>%
    rename_with(
      .cols = starts_with("samples_"),
      .fn = ~ str_remove_all(.x, "dist_")
    ) %>%
    select(-starts_with("dist_")) %>%
    unnest(cols = starts_with("samples_"))

  generated_samples %>%
  # add delay 1 and 2 together
    mutate(
      samples_test_to_interview = 
        samples_test_turnaround_time + samples_time_to_interview
      )
}
