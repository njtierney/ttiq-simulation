#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n
#' @return
#' @author Nick Golding
#' @export
get_optimal_isol_interview_samples <- function() {

  read_csv(
    "outputs/optimal_delay_samples.csv",
    col_types = cols(
      time_to_swab = col_double(),
      test_turnaround_time = col_double(),
      time_to_interview = col_double(),
      time_to_isolation = col_double()
    )
  ) %>%
    # drop the time from source interview to isolation, since that is different
    # for each contact, and sampled independently
    select(
      -time_to_isolation
    ) %>%
    mutate(
      tracing_delay = Reduce(
        "+",
        across(
          everything()
        )
      )
    ) %>%
    pull(tracing_delay)

}
