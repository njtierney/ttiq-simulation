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
tidy_populations <- function(populations_raw, sa2_lookup) {

  populations_df <- populations_raw %>% 
    left_join(sa2_lookup,
              by = "sa2_5dig16") %>% 
    select(-sa2_5dig16,
           -sa3_code16,
           -gcc_name16)
  
  # TODO
  # check that this table length seems about right
  populations_state <- populations_df %>% 
    group_by(ste_name16,
             age_lower,
             age_upper) %>% 
    summarise(population = sum(population),
              .groups = "drop") 
  
  populations_state
    
  # populations_state %>% 
  #   group_by(ste_name16) %>% 
  #   summarise(population = sum(population)) %>% 
  #   pull(population) %>% 
  #   sum() %>% 
  #   comma()
  # unique(partial_df$age_band_id)
  # populations


}
