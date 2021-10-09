#' @title
#' @param sim_tracing_output_list
#' @return
#' @author Chris Baker/Logan Wu
#' @export
   #Diagnostic plots
plot_sim_tracing = function (experiment_result) {
  scenario_index = 15
     scenario_samples = experiment_result[[scenario_index]] %>%
       filter(scenario == "current_nsw") %>%
       unnest(cols = starts_with("samples_"))
     xmax = max(scenario_samples$samples_time_to_interview)
     p1 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
       geom_bar() +
       coord_cartesian(xlim=c(-2, NA)) +
       scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax)) +
       ggtitle(names(sim_tracing_output_list)[scenario_index])# +
       #labs(title = glue("Interview capacity of {capacity/rate} x rate"),
       #     fill = "Interviewed\n(to show NAs)")

     p2 = ggplot(scenario_samples, aes(x=samples_time_to_interview, fill=samples_time_to_interview >= 0)) +
       geom_bar() +
       facet_wrap(vars(samples_priority_group)) +
       coord_cartesian(xlim=c(-2, NA)) +
       scale_x_continuous(breaks = c(-2, 0:xmax), labels=c("Missed", 0:xmax))# +
       #labs(title = glue("Facet by priority group"),
       #     fill = "Interviewed\n(to show NAs)")

     p1 / p2
     }