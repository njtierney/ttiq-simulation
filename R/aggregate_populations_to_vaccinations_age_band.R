#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param populations_raw
#' @return
#' @author Nicholas Tierney
#' @export
aggregate_populations_to_vaccinations_age_band <- function(
  populations_raw,
  vaccination_age_band
  ) {
  
  vaccination_age_band
  
  populations_raw %>% 
    left_join(vaccination_age_band,
              by = c("age_lower" = "lower_age_band"))
  
  age_lookup <- tibble::tribble(
    ~age_lower, ~age_upper, ~age_band_quantium, ~age_band_5y,
    0,           4,          "0-9",              "0-4",
    5,           9,          "0-9",              "5-9",
    10,          10,         "10-19",            "10-14",
    11,          11,         "10-19",            "10-14",
    12,          12,         "10-19",            "10-14",
    13,          13,         "10-19",            "10-14",
    14,          14,         "10-19",            "10-14",
    15,          15,         "10-19",            "15-19",
    16,          16,         "10-19",            "15-19",
    17,          17,         "10-19",            "15-19",
    18,          19,         "10-19",            "15-19",
    20,          24,         "20-29",            "20-24",
    25,          29,         "20-29",            "25-29",
    30,          34,         "30-39",            "30-34",
    35,          39,         "30-39",            "35-39",
    40,          44,         "40-49",            "40-44",
    45,          49,         "40-49",            "45-49",
    50,          54,         "50-59",            "50-54",
    55,          59,         "50-59",            "55-59",
    60,          64,         "60-69",            "60-64",
    65,          69,         "60-69",            "65-69",
    70,          74,         "70-79",            "70-74",
    75,          79,         "70-79",            "75-79",
    80,          84,         "80+",              "80+",
    85,          89,         "80+",              "80+",
    90,          94,         "80+",              "80+",
    95,          99,         "80+",              "80+",
    100,         999,        "80+",              "80+"
  ) 

}
