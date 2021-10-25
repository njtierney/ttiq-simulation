#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_coverage_age_group_at_milestone
#' @return
#' @author Nicholas Tierney
#' @export
aggregate_5_years <- function(vaccination_coverage_age_group_at_milestone,
                              populations) {

  age_lookup <- tibble::tribble(
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
  )
  
  vaccination_coverage_age_group_at_milestone
  
  age_lookup %>% 
    left_join(populations,
              by = c("pop_age_lower" = "age_lower",
                     "pop_age_upper" = "age_upper"))
  
  # use all oz popn data
  #10-14 age bin - weigt of coverages on 0-11s, and 12-14s
  # we need to know what fraction of population falls in each one
  # then aggregating the population at the 5 year age bin
  # then we do the same for the 15-19 year olds,
  

}
