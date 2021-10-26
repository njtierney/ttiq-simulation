#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param prepared_infections_vax_symp
#' @return
#' @author Nick Golding
#' @export
get_mini_abm_parameters <- function(prepared_infections_vax_symp, oz_baseline_matrix, vaccination_coverage_age_group_at_milestone, populations) {
  
  naive_stable_state <- Re(eigen(oz_baseline_matrix)$vectors[, 1])
  
  clinical_fraction_unvax <- read_susceptibility_clinical_fraction_age() %>% 
    select(
      age_group,
      clinical_fraction_mean
    ) %>% 
    left_join(
      age_group_10y_5y(), 
      by = c("age_group" = "age_group_10y")
    ) %>% 
    select(
      age_5_year = age_group_5y,
      clinical_fraction_unvax = clinical_fraction_mean
    )
  
  symptomatic_fraction_unvaccinated <- weighted.mean(clinical_fraction_unvax$clinical_fraction_unvax, naive_stable_state)
  
  unvax_symptomatics <- prepared_infections_vax_symp %>%
    filter(
      which == "All infections",
      !vaccinated
    ) %>%
    select(milestone, symptomatic, fraction) %>%
    mutate(
      symptomatic = ifelse(
        symptomatic,
        "symptomatic",
        "asymptomatic"
      )
    ) %>%
    pivot_wider(
      names_from = symptomatic,
      values_from = fraction
    ) %>%
    mutate(
      fraction_symptomatic = symptomatic / (symptomatic + asymptomatic)
    ) %>%
    select(
      milestone, fraction_symptomatic
    ) %>%
    bind_rows(
      tibble(
        milestone = "no vaccination",
        fraction_symptomatic = symptomatic_fraction_unvaccinated
      ),
      .
    )
  
  fraction_vaccinated <- prepared_infections_vax_symp %>%
    filter(
      which == "All infections"
    ) %>%
    select(milestone, vaccinated, symptomatic, fraction) %>%
    mutate(
      vaccinated = ifelse(
        vaccinated,
        "vaccinated",
        "unvaccinated"
      )
    ) %>%
    group_by(
      milestone, vaccinated
    ) %>%
    summarise(
      across(
        fraction,
        sum
      ),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = vaccinated,
      values_from = fraction
    ) %>%
    mutate(
      fraction_vaccinated = vaccinated / (vaccinated + unvaccinated)
    ) %>%
    select(
      milestone, fraction_vaccinated
    )
  
  age_populations <- tibble::tribble(
    ~pop_age_lower, ~pop_age_upper, ~vac_age_lower, ~vac_age_upper, ~age_5_year,
    0,             4,               0,              11,              "0-4",
    5,             9,               0,              11,              "5-9",
    10,            11,              0,              11,              "10-14",
    12,            14,              12,              15,             "10-14",
    15,            15,              12,              15,             "15-19",
    16,            17,              16,              19,             "15-19",
    18,            19,              16,              19,             "15-19",
    20,            24,              20,              29,             "20-24",
    25,            29,              20,              29,             "25-29",
    30,            34,              30,              39,             "30-34",
    35,            39,              30,              39,             "35-39",
    40,            44,              40,              49,             "40-44",
    45,            49,              40,              49,             "45-49", 
    50,            54,              50,              59,             "50-54", 
    55,            59,              50,              59,             "55-59", 
    60,            64,              60,              69,             "60-64", 
    65,            69,              60,              69,             "65-69", 
    70,            74,              70,              79,             "70-74", 
    75,            79,              70,              79,             "75-79", 
    80,            84,              80,              NA,             "80+", 
    85,            89,              80,              NA,             "80+",
    90,            94,              80,              NA,             "80+",
    95,            99,              80,              NA,             "80+",
    100,           999,             80,              NA,             "80+"
  ) %>% 
    mutate(age_band_id = case_when(
      is.na(vac_age_upper) ~ "80+",
      TRUE ~ as.character(
        glue(
          "{vac_age_lower}-{vac_age_upper}"
        )
      )
    )
    ) %>% 
    left_join(
      populations,
      by = c(
        "pop_age_lower" = "age_lower",
        "pop_age_upper" = "age_upper"
      )
    ) %>%
    group_by(age_band_id) %>%
    summarise(
      across(
        population,
        sum
      )
    )
  
  
  whole_population <- sum(age_populations$population)
  
  whole_population_coverages <- vaccination_coverage_age_group_at_milestone %>%
    left_join(
      age_populations,
      by = "age_band_id"
    ) %>%
    mutate(
      n_doses = population * any_vaccine
    ) %>%
    group_by(
      milestone
    ) %>%
    summarise(
      across(
        n_doses,
        sum
      )
    ) %>%
    mutate(
      whole_population_coverage = n_doses / whole_population
    ) %>%
    select(
      -n_doses
    )
  
  # combine these
  unvax_symptomatics %>%
    left_join(
      fraction_vaccinated,
      by = "milestone"
    ) %>%
    left_join(
      whole_population_coverages,
      by = "milestone"
    ) %>%
    mutate(
      across(
        c(fraction_vaccinated, whole_population_coverage),
        ~ replace_na(.x, 0)
      )
    )
  
}
