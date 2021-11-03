#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage
#' @param vaccination_coverage_milestones
#' @return
#' @author Nicholas Tierney
#' @export
age_group_coverage_at_milestones <- function(vaccinations,
                                             vaccination_coverage_milestones) {
  
  
  vaccination_coverage_age_group_at_milestone <- 
  vaccinations %>% 
    pivot_longer(
      cols = starts_with("time_"),
      names_to = "dose",
      values_to = "date",
      names_prefix = "time_dose_"
    ) %>% 
    group_by(age_band_id,
             vaccine,
             dose,
             date) %>% 
    summarise(n_vaccinated = sum(n_vaccinated),
              population = first(population),
              .groups = "drop") %>% 
    pivot_wider(
      names_from = c(
        vaccine, 
        dose
      ),
      values_from = n_vaccinated,
      values_fill = 0
    ) %>% 
    arrange(date) %>% 
    group_by(age_band_id) %>% 
    mutate(across(
      .cols = c(starts_with("Astra"),
                starts_with("Pfizer")),
      .fns = cumsum
    )) %>% 
    full_join(
      vaccination_coverage_milestones,
      by = "date"
    ) %>% 
    select(
      -starts_with("coverage")
    ) %>% 
    filter(!is.na(milestone)) %>% 
    arrange(
      milestone,
      age_band_id
    ) %>% 
    mutate(
      AstraZeneca_1 = AstraZeneca_1 - AstraZeneca_2,
      Pfizer_1 = Pfizer_1 - Pfizer_2,
      across(
        .cols = c(starts_with("Astra"),
                  starts_with("Pfizer")),
        .fns = list(
          coverage = ~.x / population
        ),
      ),
      any_vaccine = Reduce("+", 
                           across(
                             .cols = ends_with("coverage")
                             )
      ),
      .before = date
    ) %>% 
    select(
      age_band_id,
      date,
      milestone,
      any_vaccine,
      ends_with("coverage")
    )
  
  vaccination_coverage_age_group_at_milestone
  
}
