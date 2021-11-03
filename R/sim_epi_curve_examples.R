#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
sim_epi_curve_examples <- function() {

  future::plan(multisession(workers = 8))
  # future::plan(sequential, split = TRUE)
  
  # run simulations for various TPs
  expand_grid(
    scenario = c(
      "baseline",
      "+1%",
      "+5%",
      "+10%"
    ),
    starting_R = 4.72
  ) %>%
    mutate(
      R = case_when(
        scenario == "baseline" ~ starting_R,
        scenario == "+1%" ~ starting_R * 1.01,
        scenario == "+5%" ~ starting_R * 1.05,
        scenario == "+10%" ~ starting_R * 1.1
      )
    ) %>%
    rowwise() %>%
    mutate(
      parameters = list(
        setup_abm(
          R = R
        )
      )
    ) %>%
    mutate(
      simulations = list(
        get_valid_abm_samples(
          parameters, 
          max_days = 114,
          n_samples = 100
        )
      )
    )

}
