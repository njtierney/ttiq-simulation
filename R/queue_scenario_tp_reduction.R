queue_scenario_tp_reduction <- function(queue_scenarios) {
  
  
  # Define TP reductions
  csv_path = "data-public/testing_delay_kretzschmar_table_2_extended.csv"
  kretzschmar_tp_reduction = read_kretzschmar_tp_reduction(csv_path)
  vaccine_tp_reduction = tribble(
    ~vaccine, ~vaccine_tp_reduction,
    "AZ", 0.63,
    "Pfizer", 0.82,
    "None", 0
  )
  
  processed = lapply(queue_scenarios, function(x) {
    x %>%
      unnest() %>%
      # Calculate TP reductions
      mutate(
        samples_vaccinated = ifelse(samples_vaccinated, "Pfizer", "None")
      ) %>%
      left_join(vaccine_tp_reduction,
                by = c("samples_vaccinated" = "vaccine")) %>%
      left_join(kretzschmar_tp_reduction,
                by = c("samples_test_turnaround_time" = "Testing.delay",
                       "samples_time_to_interview" = "contact_tracing_delay")) %>%
      mutate(tp_reduction = 1 - (1-vaccine_tp_reduction) * (1-kretzschmar_tp_reduction))
  })
  
  mean = lapply(processed, function(x) {
    mean(x$tp_reduction)
  })
  mean
}