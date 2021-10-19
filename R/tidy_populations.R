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
  
  populations_df 
  
  # unique(partial_df$age_band_id)
  # populations


}
