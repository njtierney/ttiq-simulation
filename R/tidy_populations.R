#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param populations_raw
#' @param sa2_lookup
#' @return
#' @author Nicholas Tierney
#' @export
tidy_populations <- function(populations_raw) {

  oz_population <- populations_raw %>% 
    group_by(age_lower,
             age_upper) %>% 
    summarise(population = sum(population))
  
  oz_population
    
  # populations_state %>% 
  #   group_by(ste_name16) %>% 
  #   summarise(population = sum(population)) %>% 
  #   pull(population) %>% 
  #   sum() %>% 
  #   comma()
  # unique(partial_df$age_band_id)
  # populations


}
