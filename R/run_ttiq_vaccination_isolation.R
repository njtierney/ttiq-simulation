#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_vaccination_isolation
#' @return
#' @author Nicholas Tierney
#' @export
run_ttiq_vaccination_isolation <- function(scenario_vaccination_isolation) {

  
  # some notes on the variables
  
  # isolation_stringency
    # vary and give combinations of
    # measurement of how stringent people are with their isolation.
    # isolation_stringency of 1
      # Someone does full isolation, obeying rules 100% time
    # isolation_stringency of 0.5
      # Someone does full isolation, obeying rules 50% of time
  # tp 
    # transmission potential. This is kind of like reff (effectively)
  # tp_multiplier
    # constant (estimated?)
    # How much transmission is reduced
    # tp_multiplier of 0.25 implies a reduction of 75%
    # tp_multiplier of 1 implies a reduction of 0
  # vaccination_multiplier
    # relative probability of onward transmission for vaccinated people (constant)
  # p_passive_detection_vaccinated
    # constant
  # tp_multiplier

  # vaccination_coverage
    # vary and give combinations of
  # for measuring vaccinated
  # vaccinatedness vs foundness
  # we've got to compute weights of all 4 of these:
  # vacc & found = tp * vaccination_multiplier * (isolation_stringency * tp_multiplier)
  # !vacc & !found = tp * 1
  # vacc & !found = tp * vaccination_multiplier
  # !vacc & found = tp * tp_multiplier
  
  scenario_vaccination_isolation %>% 
    mutate(
      tp_mult_vacc_found = tp * vaccination_multiplier * (isolation_stringency * tp_multiplier),
      tp_mult_not_vacc_found = tp * 1,
      tp_mult_vacc_not_found = tp * vaccination_multiplier,
      tp_mult_not_vacc_not_found = tp * tp_multiplier,
      pr_found_given_vacc = 1 - (1 - p_active_detection) * (1 - p_passive_detection_vaccinated),
      pr_found_given_not_vacc = (1 - (1 - p_active_detection) * (1 - p_passive_detection)),
      weight_vacc_found = pr_found_given_vacc * pr_vaccination_cases,
      weight_not_vacc_found = ,
      weight_vacc_not_found = ,
      weight_not_vacc_not_found = ,
    )
  
  # so above is the tp_multiplier for each group
  # to then work out the population fraction of each of these groups, we do 
  # (something like) the following, for each category:
  # Say, to calculate fraction of cases that are vaccinated vs unvaccinated 
  # Nick G has a function that takes the inputs of vaccination coverage for the population
  # and returns the vaccination coverage for cases
  # then, conditional on being vaccinated, we have 
  # Pr(being_found | vaccinated) = (1 - (1 - p_active_detection) * (1 - p_passive_detection_vaccinated))
  # Pr(being_found | !vaccinated) = (1 - (1 - p_active_detection) * (1 - p_passive_detection))
  # weight for vacc & found = Pr(being_found | vaccinated) * Pr(vaccination coverage for cases)
  
  # we then 
  # so then out of this we'll get a tp reduction for these scenarios, 
  # for each of these groups, output a table and graphic
  
  # Repeat this for each of the scenarios
  

}
