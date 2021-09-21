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

  
  scenario_vaccination_isolation
  
  # make fake values
  
  # for measuring vaccinated
  # vaccinatedness vs foundness
  # vacc & found = tp * vaccination_multiplier * (isolation_stringency * tp_multiplier)
  # full isolatiopn = isolation_stringency of 1
  # tp is reff (effecticely)
  # a tp_multiplier of 0.25 implies a reduction of 75%
  # a tp_multiplier of 1 implies a reduction of 0
  # !vacc & !found = tp * 1
  # vacc & !found = tp * vaccination_multiplier
  # !vacc & found = tp * tp_multiplier
  # we've got to compute weights of all 4 of these ^^
  
  
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
  ### vaccination_multiplier - relative probability of onward transmission for vaccinated people (constant)
  ### p_passive_detection_vaccinated - constant
  ### tp_multiplier - constant (estimated?)
  ### isolation_stringency - vary and give combinations of
  ### vaccination_coverage - vary and give combinations of
  # so then out of this we'll get a tp reduction for these scenarios, 
  # for each of these groups, output a table and graphic
  
  # Repeat this for each of the scenarios
  

}
