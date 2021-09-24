#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scenario_vaccination_isolation
#' @return
#' @author Nicholas Tierney
#' @export
run_ttiq_vaccination_isolation <- function(scenario_vaccination_isolation, baseline_matrix) {

  
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
  
  results <- scenario_vaccination_isolation %>% 
    # compute the vaccination coverage among cases
    rowwise() %>%
    mutate(
      pr_vaccination_cases = fraction_cases_unvaccinated(
        efficacy_susceptibility = 0.9,
        efficacy_onward = 0.8,
        coverage_any_vaccine = vaccination_coverage,
        baseline_matrix = baseline_matrix
      )
    ) %>%
    ungroup() %>%
    # compute tp mutlitpliers for all combinations people who are vaccianted or
    # not, and detected or not
    mutate(
      # no reduction if vaccinated and unfound
      tp_mult_not_found_not_vacc = 1,
      # only the vaccination reduction if vaccinated and unfound
      tp_mult_not_found_vacc = vaccination_multiplier,
      # usual reduction if unvaccinated and found
      tp_mult_found_not_vacc = tp_multiplier,
      # if unvaccinated and found, get the vaccination reduction and the tp
      # multiplier - scaled by isolation stringency. If isolation stringency is
      # 1, should just be the tp multiplier, if isolation stringency is 0,
      # should be 1
      tp_mult_found_vacc = 
        vaccination_multiplier * (1 - isolation_stringency * (1 - tp_multiplier)),
      pr_found_given_vacc = 
        1 - (1 - p_active_detection) * (1 - p_passive_detection_vaccinated),
      pr_found_given_not_vacc = 
        (1 - (1 - p_active_detection) * (1 - p_passive_detection)),
      weight_vacc_found = pr_found_given_vacc * pr_vaccination_cases,
      weight_not_vacc_found = 
        pr_found_given_not_vacc * (1 - pr_vaccination_cases),
      weight_vacc_not_found = (1 - pr_found_given_vacc) *  pr_vaccination_cases,
      weight_not_vacc_not_found = 
        (1 - pr_found_given_not_vacc) * (1 - pr_vaccination_cases)
    ) %>% 
    # check that they sum to 1
    mutate(sanity_check = 
             weight_vacc_found + 
             weight_not_vacc_found + 
             weight_vacc_not_found + 
             weight_not_vacc_not_found,
           # check they sum to 1
           sanity_check_all = sanity_check == 1,
      .before = everything()
    ) %>% 
    # pull(sanity_check) %>% all()
    mutate(
      weighted_tp_mult_found_vacc = 
        tp_mult_found_vacc * weight_vacc_found,
      weighted_tp_mult_found_not_vacc = 
        tp_mult_found_not_vacc * weight_not_vacc_found,
      weighted_tp_mult_not_found_vacc = 
        tp_mult_not_found_vacc * weight_vacc_not_found,
      weighted_tp_mult_not_found_not_vacc = 
        tp_mult_not_found_not_vacc * weight_not_vacc_not_found,
      weighted_tp_mutliplier_popn = 
        weighted_tp_mult_found_vacc + 
        weighted_tp_mult_found_not_vacc + 
        weighted_tp_mult_not_found_vacc + 
        weighted_tp_mult_not_found_not_vacc
    )
  
  # get baseline tp multiplier for each vaccination coverage
  baseline <- results %>%
    filter(
      isolation_stringency == 1
    ) %>%
    rename(
      baseline_tp_multiplier = weighted_tp_mutliplier_popn
    ) %>%
    select(
      vaccination_multiplier,
      p_passive_detection_vaccinated,
      p_active_detection,
      p_passive_detection,
      tp_multiplier,
      vaccination_coverage,
      baseline_tp_multiplier
    )

  # join on and rescale the TP reductions  
  results %>%
    left_join(
      baseline,
      by = c(
        "vaccination_multiplier",
        "p_passive_detection_vaccinated",
        "p_active_detection",
        "p_passive_detection",
        "tp_multiplier",
        "vaccination_coverage"
      )
    ) %>%
    mutate(
      weighted_tp_mutliplier_popn_normalised = weighted_tp_mutliplier_popn / baseline_tp_multiplier,
      weighted_tp_reduction_scaled = 1 - (weighted_tp_mutliplier_popn_normalised * tp_multiplier)
    )
  # normalise the TP reduction so that it is 'tp_multiplier' when isolation is 1
  
     # then multiply and sum them together.
  # 
  
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
