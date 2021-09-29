#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param isolation_cdfs
#' @return
#' @author dhduncan
#' @export
create_tti_distributions <- function(isolation_cdfs) {

  ttiq_ecdfs <- isolation_cdfs %>%
    mutate(
      which = case_when(
        state == "NSW" & date == as_date("2021-01-14") ~ "optimal",
        state == "VIC" & date == as_date("2020-08-04") ~ "partial",
        state == "NSW" & date == as_date("2021-08-15") ~ "current",
        TRUE ~ NA_character_
      ) 
    ) %>%
    filter(
      !is.na(which)
    ) %>%
    select(which, ecdf) %>%
    pivot_wider(
      names_from = which,
      values_from = ecdf
    )
  
  tti_distributions <- tibble(
    days = environment(ttiq_ecdfs$partial[[1]])$x,
    partial = environment(ttiq_ecdfs$partial[[1]])$y
  ) %>%
    left_join(
      tibble(
        days = environment(ttiq_ecdfs$optimal[[1]])$x,
        optimal = environment(ttiq_ecdfs$optimal[[1]])$y
      ),
      by = "days"
    ) %>%
    left_join(
      tibble(
        days = environment(ttiq_ecdfs$current[[1]])$x,
        current = environment(ttiq_ecdfs$current[[1]])$y
      ),
      by = "days"
    ) %>%
    arrange(
      days
    ) %>%
    mutate(
      across(
        c(partial, optimal, current),
        ~diff(c(0, .x))
      ),
      across(
        c(partial, optimal, current),
        ~replace_na(.x, 0)
      ),
      across(
        c(partial, optimal, current),
        ~.x / sum(.x)
      )
    )

tti_distributions

}
