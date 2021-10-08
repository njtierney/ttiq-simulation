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
  # tp_multiplier
    # constant (estimated?)
    # How much transmission is reduced
    # tp_multiplier of 0.25 implies a reduction of 75%
    # tp_multiplier of 1 implies a reduction of 0
  # ve_onward_transmission
    # relative probability of onward transmission for vaccinated people (constant)
  # p_passive_detection_vaccinated
    # constant
  # tp_multiplier

  # vaccination_coverage
    # vary and give combinations of
  # for measuring vaccinated
  # vaccinatedness vs foundness
  # we've got to compute weights of all 4 of these:
  # vacc & found = tp * ve_onward_transmission * (isolation_stringency * tp_multiplier)
  # !vacc & !found = tp * 1
  # vacc & !found = tp * ve_onward_transmission
  # !vacc & found = tp * tp_multiplier
  
  results <- scenario_vaccination_isolation %>% 
    # compute the vaccination coverage among cases
    rowwise() %>%
    mutate(
      pr_vaccination_cases = fraction_cases_unvaccinated(
        efficacy_susceptibility = ve_susceptibility,
        efficacy_onward = ve_onward_transmission,
        coverage_any_vaccine = vaccination_coverage,
        baseline_matrix = baseline_matrix
      )
    ) %>%
    ungroup() %>%
    # compute tp multipliers for all combinations people who are vaccinated or
    # not, and detected or not
    mutate(
      
      # no reduction if vaccinated and unfound
      tp_mult_not_found_not_vacc = 1,
      # only the vaccination reduction if vaccinated and unfound
      tp_mult_not_found_vacc = ve_onward_transmission,
      # usual reduction if unvaccinated and found
      tp_mult_found_not_vacc = tp_multiplier,
      
      # if unvaccinated and found, get the vaccination reduction and the tp
      # multiplier - scaled by isolation stringency. If isolation stringency is
      # 1, should just be the tp multiplier, if isolation stringency is 0,
      # should be 1.
      
      # Now we split these by high vs low risk settings
      
      # compute the TP multipliers for high-risk vs low-risk settings
      high_risk_multiplier = 1 / ((1 - fraction_vaccinated_low_risk) * (vacc_setting_risk_ratio - 1) + 1),
      low_risk_multiplier = high_risk_multiplier * vacc_setting_risk_ratio,
      
      # check these balance out to a multiplier of 1 across all the vaccinated
      # found cases
      check_risk_multiplier_balances = (
        high_risk_multiplier * fraction_vaccinated_low_risk +
          low_risk_multiplier * (1 - fraction_vaccinated_low_risk)
      ) == 1,
      check_risk_multiplier_matches_ratio = (
        low_risk_multiplier / high_risk_multiplier == vacc_setting_risk_ratio
      ) == 1,
      
      # split the found and vaccinated into two sets - the low risk and the high risk
      tp_mult_found_vacc_low_risk =
        low_risk_multiplier * ve_onward_transmission *
        (1 - isolation_stringency_vaccinated * (1 - tp_multiplier)),
      
      tp_mult_found_vacc_high_risk = high_risk_multiplier * ve_onward_transmission *
        tp_multiplier,
      
      pr_found_given_vacc = 
        1 - (1 - p_active_detection) * (1 - p_passive_detection_vaccinated),
      pr_found_given_not_vacc = 
        (1 - (1 - p_active_detection) * (1 - p_passive_detection)),
      weight_not_vacc_found = 
        pr_found_given_not_vacc * (1 - pr_vaccination_cases),
      weight_vacc_not_found = (1 - pr_found_given_vacc) *  pr_vaccination_cases,
      weight_not_vacc_not_found = 
        (1 - pr_found_given_not_vacc) * (1 - pr_vaccination_cases),
      
      # for the cases vaccinated and found, split between low- and high-risk settings
      # weight_vacc_found = pr_found_given_vacc * pr_vaccination_cases,
      weight_vacc_found_low_risk = pr_found_given_vacc * pr_vaccination_cases *
        fraction_vaccinated_low_risk,
      
      weight_vacc_found_high_risk = pr_found_given_vacc * pr_vaccination_cases *
        (1 - fraction_vaccinated_low_risk),
      
      # check the weights sum to 1
      check_weights = (
        weight_not_vacc_found + 
          weight_vacc_not_found + 
          weight_not_vacc_not_found +
          weight_vacc_found_low_risk +
          weight_vacc_found_high_risk
      ) == 1,
      
      # check that all the numerical checks pass
      all_checks = all(
        across(
          starts_with("check")
        )
      )
    
    ) %>% 
    # pull(sanity_check) %>% all()
    mutate(
      weighted_tp_mult_found_not_vacc = 
        tp_mult_found_not_vacc * weight_not_vacc_found,
      weighted_tp_mult_not_found_vacc = 
        tp_mult_not_found_vacc * weight_vacc_not_found,
      weighted_tp_mult_not_found_not_vacc = 
        tp_mult_not_found_not_vacc * weight_not_vacc_not_found,
      weighted_tp_mult_found_vacc_low_risk = 
        tp_mult_found_vacc_low_risk * weight_vacc_found_low_risk,
      weighted_tp_mult_found_vacc_high_risk = 
        tp_mult_found_vacc_high_risk * weight_vacc_found_high_risk,
      tp_multiplier_output = 
        weighted_tp_mult_found_not_vacc + 
        weighted_tp_mult_not_found_vacc + 
        weighted_tp_mult_not_found_not_vacc +
        weighted_tp_mult_found_vacc_low_risk +
        weighted_tp_mult_found_vacc_high_risk
    ) %>%
    select(
      -starts_with("weight"),
      -starts_with("check"),
      -starts_with("pr"),
      -starts_with("tp_mult_"),
      ends_with("risk_multiplier")
    )
  
  # get baseline tp multiplier for each vaccination coverage
  baseline <- results %>%
    filter(
      isolation_stringency_vaccinated == 1
    ) %>%
    rename(
      baseline_tp_multiplier = tp_multiplier_output
    ) %>%
    select(
      ve_onward_transmission,
      ve_susceptibility,
      ve_symptoms,
      rel_test_seeking_vaccinated,
      no_passive_detection_vaccinated,
      p_active_detection,
      p_passive_detection,
      p_passive_detection_vaccinated,
      tp_multiplier,
      fraction_vaccinated_low_risk,
      vacc_setting_risk_ratio,
      vaccination_coverage,
      baseline_tp_multiplier
    )

  # join on and rescale the TP reductions  
  results %>%
    left_join(
      baseline,
      by = c(
        "ve_onward_transmission",
        "ve_susceptibility",
        "ve_symptoms",
        "rel_test_seeking_vaccinated",
        "no_passive_detection_vaccinated",
        "p_active_detection",
        "p_passive_detection",
        "p_passive_detection_vaccinated",
        "tp_multiplier",
        "fraction_vaccinated_low_risk",
        "vacc_setting_risk_ratio",
        "vaccination_coverage"
      )
    ) %>%
    mutate(
      tp_multiplier_output_normalised = tp_multiplier_output / baseline_tp_multiplier,
      weighted_tp_reduction_scaled = 1 - (tp_multiplier_output_normalised * tp_multiplier)
    )
  # normalise the TP reduction so that it is 'tp_multiplier' when isolation is 1
  
  # then, conditional on being vaccinated, we have 
  # Pr(being_found | vaccinated) = (1 - (1 - p_active_detection) * (1 - p_passive_detection_vaccinated))
  # Pr(being_found | !vaccinated) = (1 - (1 - p_active_detection) * (1 - p_passive_detection))
  # weight for vacc & found = Pr(being_found | vaccinated) * Pr(vaccination coverage for cases)
  

}
