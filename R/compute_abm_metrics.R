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
  transmissions <- res %>%
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
  sources <- res %>%
    filter(
      # find sources to consider (exclude those during burn in and last two weeks
      # due to truncation of onward infection)
      !is.na(source_id) &
        infection_day > 14 &
        infection_day < (max(infection_day) - 20)
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
      infection_to_isolation = isolation_day - infection_day
    )
  
  sources %>%
    group_by(simulation) %>%
    summarise(
      TP = mean(transmissions),
      ascertainment = mean(found)
    ) %>%
    summarise(
      across(
        c(TP, ascertainment),
        .fns = list(
          mean = ~mean(.x),
          variance = ~ var(.x)
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
