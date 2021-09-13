#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_df
#' @return
#' @author Nicholas Tierney
#' @export
run_ttiq_scenario <- function(scenario_df) {

  # use a Gibbs Sampler to convert distributions of time from:
  ## "infector isolation" to "infectee isolation" into 
  ## distributions of time from "infection" to "isolation" of cases
  ## and therefore reduction in transmission potential
  # Case 1 is infected at time 0 and isolated at time "inf_isol"
  # Case 2 is infected at time "inf_inf", drawn from a lognormal
  ## Case 1 is infected at time 0 and isolated at time "infect_isolate"
  ## Case 2 is infected at time "infect_infect", drawn from a lognormal
  # representing the naive generation interval distribution, truncated at 
  # the date of Case 1 isolation.
  # Case 2 is isolated at time: inf_isol = inf_isol + isol_isol - inf_inf
  ## Case 2 is isolated at time: 
  ## infect_isolate = infect_isolate + isolate_isolate - infect_infect
  
  scenario_df %>% 
    mutate(time_to_isolation_sims = pmap(
      .l = list(
        n_chains = n_chains,
        n_iterations = n_iterations,
        gi_meanlog = gi_meanlog,
        gi_sdlog = gi_sdlog,
        sim_tracing_fun = sim_tracing_fun
      ),
      .f = time_to_isolation
    ),
    tp_multiplier = pmap_dbl(
      .l = list(
        inf_isol = time_to_isolation_sims,
        meanlog = gi_meanlog,
        sdlog = gi_sdlog
      ),
      .f = tp_reduction
    ),
  # given a starting R, get the R after contact tracing
    r_after_contact_tracing = r_start * tp_multiplier
    ) %>% 
    relocate(
      r_after_contact_tracing,
      tp_multiplier,
      .before = gi_meanlog
    )

}
