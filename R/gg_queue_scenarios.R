#' @title
#' @param sim_tracing_output_list
#' @return
#' @author Chris Baker/Logan Wu
#' @export
#Diagnostic plots
gg_queue_scenarios = function (queue_scenarios) {
   plot = function(x) {
      scenario_samples = queue_scenarios[[x]] %>%
         head(1) %>%
         unnest(cols = starts_with("samples_"))
      capacity_ratio = scenario_samples$capacity_ratio[1]
      xmax = max(scenario_samples$samples_time_to_interview)
      p1 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
         geom_bar() +
         coord_cartesian(xlim=c(-2, NA)) +
         scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax)) +
      labs(title = x %>%
              paste("Priority function:", .) %>%
              str_replace("\\.rate", ", \u03bb="),
           subtitle = glue("Interview capacity of {capacity_ratio} \u00d7 interviewable case rate"),
           fill = "Interviewed\n(to show NAs)")
      
      p2 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
         geom_bar() +
         facet_wrap(vars(samples_priority_group)) +
         coord_cartesian(xlim=c(-2, NA)) +
         scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax)) +
      labs(subtitle = glue("Faceted by priority group"),
           fill = "Interviewed\n(to show NAs)")
      
      p3 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
         geom_bar() +
         facet_wrap(vars(samples_vaccinated)) +
         coord_cartesian(xlim=c(-2, NA)) +
         scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax)) +
         labs(subtitle = glue("Faceted by vaccination status"),
              fill = "Interviewed\n(to show NAs)")
      
      p1 / p2 / p3
   }
   plots = lapply(names(queue_scenarios), plot) %>%
      setNames(names(queue_scenarios))
}