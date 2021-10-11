#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param trace_object
#' @param gi_meanlog
#' @param gi_sdlog
#' @param r_start
#' @return
#' @author Nicholas Tierney
#' @export
create_scenario_df <- function(n_iterations,
                               n_chains,
                               sim_tracing_funs,
                               passive_detection_given_symptoms,
                               pr_symptoms,
                               p_active_detection,
                               p_passive_detection,
                               passive_distribution) {
  # parameters of naive (untruncated) generation interval / infectiousness
  # profile
  sim_tracing_funs %>%
    mutate(
      n_iterations = n_iterations,
      n_chains = n_chains,
      gi_meanlog = 1.375738,
      gi_sdlog = 0.5665299,
      passive_detection_given_symptoms = passive_detection_given_symptoms,
      pr_symptoms = pr_symptoms,
      p_active_detection = p_active_detection,
      p_passive_detection = p_passive_detection,
      passive_distribution = passive_distribution,
      r_start = 7.82
      
    )
  
}
