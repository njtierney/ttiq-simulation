#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df_run_queue
#' @return
#' @author Nick Golding
#' @export
calculate_tp_multiplier_queue <- function(scenario_df_run_queue, ve_onward) {

  scenario_df_run_queue %>%
    rowwise() %>%
    mutate(
      tp_multiplier_if_found_individual = list(
        plnorm(
          time_to_isolation_sims,
          meanlog = gi_meanlog,
          sdlog = gi_sdlog
        )
      ),
      fraction_extra_cases_missed = mean(person_missed)
    ) %>%
    # adjust tp multiplier for the probability of missing a case entirely
    
    mutate(
      # need to adjust for increase in cases missed by contact tracing, since
      # the fraction biases down the active detection, and biases up the passive
      # detection
      p_active_detection_corrected = p_active_detection * (1 - fraction_extra_cases_missed),
      p_passive_detection_corrected = p_passive_detection * fraction_extra_cases_missed,
      p_missed = list((1 - p_active_detection_corrected) * (1 - p_passive_detection_corrected)),
      # adjust for the vaccinated cases
      vaccination_multiplier = list(ifelse(samples$vaccinated, 1 - ve_onward, 1)),
      tp_multiplier_individual = list((tp_multiplier_if_found_individual * (1 - p_missed) + 1 * p_missed) * vaccination_multiplier),
      tp_multiplier = mean(tp_multiplier_individual),
      tp_multiplier_if_found = mean(tp_multiplier_if_found_individual)
    ) %>%
    relocate(
      tp_multiplier, 
      tp_multiplier_if_found,
      .before = everything()
    )

}
