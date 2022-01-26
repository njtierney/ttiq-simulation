#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param res
#' @return
#' @author Nick Golding
#' @export
compute_abm_metrics <- function(abm_result) {
# analyse multiple ABM simulations and pull out relevant metrics
  
  # compute the numbers of onward transmissions for all sources
  transmissions <- abm_result %>%
    group_by(
      simulation,
      source_id
    ) %>%
    summarise(
      transmissions = n(),
      .groups = "drop"
    )
  
  # find source infections that we can trust onward infection for, and join on
  # their number of transmissions 
  sources <- abm_result %>%
    filter(
      # find sources to consider (exclude those during burn in and last two weeks
      # due to truncation of onward infection)
      !is.na(source_id) &
        infection_day > 20 & # 70% 20-50, 80% 50-80, 90% 10-300
        infection_day < (max(infection_day) - 14)
      # find infections to keep - the sources and those infected by these sources
    ) %>%
    select(
      -source_id
    ) %>%
    left_join(
      transmissions,
      by = c(id = "source_id", "simulation" )
    ) %>%
    mutate(
      transmissions = replace_na(transmissions, 0),
      found = !is.na(case_found_by),
      traced = found & case_found_by == "contact_tracing",
      screened = found & case_found_by == "workplace_screening",
      symptomatic = found & case_found_by == "passive_surveillance",
      infection_to_isolation = isolation_day - infection_day
    )
  
  sources %>%
    group_by(simulation) %>%
    summarise(
      TP = mean(transmissions),
      ascertainment = mean(found),
      tracing = mean(traced),
      screening = mean(screened),
      surveillance = mean(symptomatic)
    ) %>%
    summarise(
       across(
         c(TP, ascertainment, tracing, screening, symptomatic),
         .fns = list(
           mean = ~mean(.x),
           variance = ~var(.x)
         )
       )
     ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("metric", "statistic"),
      values_to = "value",
      names_sep = "_"
    )
  
}

compute_abm_metrics_full <- function(abm_result) {
  # analyse multiple ABM simulations and pull out relevant metrics
  
  # compute the numbers of onward transmissions for all sources
  transmissions <- abm_result %>%
    group_by(
      simulation,
      source_id
    ) %>%
    summarise(
      transmissions = n(),
      .groups = "drop"
    )
  
  # find source infections that we can trust onward infection for, and join on
  # their number of transmissions 
  sources <- abm_result %>%
    filter(
      # find sources to consider (exclude those during burn in and last two weeks
      # due to truncation of onward infection)
      !is.na(source_id) &
        infection_day > 20 & # 70% 20-50, 80% 50-80, 90% 10-300
        infection_day < (max(infection_day) - 14)
      # find infections to keep - the sources and those infected by these sources
    ) %>%
    select(
      -source_id
    ) %>%
    left_join(
      transmissions,
      by = c(id = "source_id", "simulation" )
    ) %>%
    mutate(
      transmissions = replace_na(transmissions, 0),
      found = !is.na(case_found_by),
      traced = found & case_found_by == "contact_tracing",
      screened = found & case_found_by == "workplace_screening",
      symptomatic = found & case_found_by == "passive_surveillance",
      infection_to_isolation = isolation_day - infection_day
    )
  
  sources %>%
    group_by(simulation) %>%
    summarise(
      TP = mean(transmissions)
    )
}





  

