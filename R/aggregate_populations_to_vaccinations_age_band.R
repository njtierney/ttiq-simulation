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
  populations
  ) {
  
  age_lookup <- tibble::tribble(
  ~pop_age_lower, ~pop_age_upper, ~vac_age_lower, ~vac_age_upper,
   0,             4,               0,              11,
   5,             9,               0,              11,
   10,            11,              0,              11,
   12,            14,              12,              15,
   15,            15,              12,              15,
   16,            17,              16,              19,
   18,            19,              16,              19,
   20,            24,              20,              29,
   25,            29,              20,              29,
   30,            34,              30,              39,
   35,            39,              30,              39,
   40,            44,              40,              49,
   45,            49,              40,              49,
   50,            54,              50,              59,
   55,            59,              50,              59,
   60,            64,              60,              69,
   65,            69,              60,              69,
   70,            74,              70,              79,
   75,            79,              70,              79,
   80,            84,              80,              NA,
   85,            89,              80,              NA,
   90,            94,              80,              NA,
   95,            99,              80,              NA,
   100,           999,             80,              NA
  )
  
  df_sa4_popn <- populations %>% 
    left_join(age_lookup,
              c("age_lower" = "pop_age_lower",
                "age_upper" = "pop_age_upper")) %>% 
    mutate(vac_age_group = case_when(
      is.na(vac_age_upper) ~ "80+",
      TRUE ~ as.character(
      glue(
        "{vac_age_lower}-{vac_age_upper}"
        )
      )
    )
    ) %>% 
    group_by(ste_name16,
             vac_age_group) %>% 
    mutate(population = sum(population)) %>% 
    ungroup() %>% 
    select(ste_name16,
           age_lower,
           age_upper,
           population,
           vac_age_group)
  
  # sanity check
  # df_sa4_popn %>% 
  #   distinct(vac_age_group, 
  #            sa4_code16,
  #            .keep_all = TRUE) %>% 
  #   pull(population) %>% 
  #   sum() %>% 
  #   comma()
  #> "25,698,093"

  df_sa4_popn

}
