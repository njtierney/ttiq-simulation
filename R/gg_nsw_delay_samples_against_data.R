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
    rename(
      other_delays = isol_swab,
      swab_to_notification = test_turnaround_time,
      notification_to_interview = time_to_interview
    ) %>% 
    mutate(
      data_type = case_when(
        data_type == "samples" ~ "model",
        data_type == "data" ~ "data"
      )
    ) %>% 
    pivot_longer(
      cols = -c(data_type, scenario),
      names_to = "delay_type",
      values_to = "days"
    ) %>% 
    filter(delay_type != "full_contact_delay") %>% 
    filter(delay_type != "test_to_interview") %>% 
    mutate(delay_type = as_factor(delay_type),
           delay_type = fct_relevel(delay_type,
                                    "swab_to_notification",
                                    "notification_to_interview",
                                    "other_delays"
                                    )) %>% 
  ggplot(aes(x = days,
             colour = data_type)) + 
    # Don't go to positive/negative infinity
    stat_ecdf(geom = "step", pad = FALSE) + 
    facet_grid(scenario ~ delay_type) +
    theme_bw() +
    theme(aspect.ratio = 1) + 
    scale_colour_brewer(palette = "Dark2") + 
    labs(
      title = "Assumed contact tracing delays for model and data",
      x = "Days",
      y = "Proportion processed by",
      colour = ""
    )
         

}
