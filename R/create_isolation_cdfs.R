#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delays
#' @param surveillance_cdfs
#' @param surveillance
#' @return
#' @author Nicholas Tierney
#' @export
create_isolation_cdfs <- function(nsw_delays, surveillance_cdfs, surveillance) {

  cutoff_date <- as_date("2021-02-01")
  
  # compute overall ecdf from before cutoff
  optimal_ecdf <- nsw_delays %>%
    filter(
      date_infection < cutoff_date
    ) %>%
    pull(time_to_isolation) %>%
    ecdf
  
  # convert surveillance effect to weights (to represent how the effectiveness
  # of the contact tracing system changed over time) and compute a weighted
  # time-to-isolation cdf for each date and state
  isolation_cdfs <- surveillance_cdfs %>%
    ungroup() %>%
    rename(
      surveillance_cdf = ecdf
    ) %>%
    mutate(
      surveillance_effect = c(t(surveillance)),
      optimal_ecdf = list(optimal_ecdf),
      isolation_weight = 1 - surveillance_effect,
      isolation_weight = isolation_weight / max(isolation_weight),
      isolation_ecdf = mapply(
        FUN = weight_ecdf,
        surveillance_cdf,
        optimal_ecdf,
        1 - isolation_weight,
        SIMPLIFY = FALSE
      )
    ) %>%
    select(
      date,
      state,
      ecdf = isolation_ecdf
    )
  
  isolation_cdfs
  

}
