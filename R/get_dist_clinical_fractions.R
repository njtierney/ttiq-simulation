#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ve_symptoms
#' @param ve_susceptibility
#' @param ve_onward_transmission
#' @param vaccination_coverage
#' @param baseline_matrix
#' @return
#' @author dhduncan
#' @export
get_dist_clinical_fraction <- function(ve_symptoms = ve_symptoms, 
                                  ve_susceptibility = ve_susceptibility,
                                  ve_onward_transmission = ve_onward_transmission,           vaccination_coverage = vaccination_coverage,
                                  baseline_matrix = baseline_matrix()) {


  stable_state <- get_stable_state(
    efficacy_susceptibility = ve_susceptibility,
    efficacy_onward = ve_onward_transmission,
    coverage_any_vaccine = vaccination_coverage,
    baseline_matrix = baseline_matrix
    )
  
  # sum((n_cases_per_age/total_N_cases * pr_symptomatic_per_age)...)
  age_weight <- stable_state/sum(stable_state[1:34])

  davies_clinical_fraction <- read_susceptibility_clinical_fraction_age() %>% 
    select(age_group, clinical_fraction_mean) %>% 
    left_join(age_group_10y_5y(), 
              by=c("age_group" = "age_group_10y")) %>% 
    pull(clinical_fraction_mean)
  
  clinical_fractions <- rep(davies_clinical_fraction, times = 2)
  
  age_weighted_clinical_fractions <- age_weight * clinical_fractions
  
  # multiply the RHS vaccinated cases by 1 - vaccine effectiveness (around 0.78? - taking midpoint of AZ and mRNA) to reduce the age-weighted clinical fraction for vaccinated
  
  ve_reduction_factor <- rep(c(1, 1 - ve_symptoms), each = 17)
  # multiply the RHS vaccinated cases by 1 - vaccine effectiveness (around 0.78? - taking midpoint of AZ and mRNA) to reduce the age-weighted clinical fraction for vaccinated  
 
  dist_age_vaccine_adjusted_clinical_fraction <- age_weighted_cases * ve_reduction_factor

  # return the age-weighted clinical fraction of cases
  return(dist_age_vaccine_adjusted_clinical_fraction)

}
