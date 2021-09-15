#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nsw_delay_samples_against_data
#' @return
#' @author Nicholas Tierney
#' @export
gg_nsw_delay_samples_against_data <- function(nsw_delay_samples_against_data) {

  # plot these against the data
  # also plot them as an ecdf
  # we wanted to know the "all" (total delay time) distribution looks the same 
  # as the expected all, which is the sum of 1 and 2
  
  nsw_delay_samples_against_data %>% 
    pivot_longer(
      cols = -c(data_type, period),
      names_to = "delay_type",
      values_to = "days"
    ) %>% 
    mutate(delay_type = as_factor(delay_type),
           delay_type = fct_relevel(delay_type,
                                    "test_turnaround_time",
                                    "time_to_interview",
                                    "test_to_interview",
                                    "full_contact_delay"
                                    )) %>% 
  ggplot(aes(x = days,
             colour = data_type)) + 
    # Don't go to positive/negative infinity
    stat_ecdf(geom = "step", pad = FALSE) + 
    facet_wrap(~ delay_type) +
    theme(aspect.ratio = 1) + 
    scale_colour_brewer(palette = "Dark2")
         

}
