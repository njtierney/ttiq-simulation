read_kretzschmar_tp_reduction <- function(csv_path) {
  read.csv(csv_path) %>%
    pivot_longer(-Testing.delay,
                 names_to = "contact_tracing_delay",
                 values_to = "kretzschmar_tp_reduction") %>%
    mutate(contact_tracing_delay = contact_tracing_delay %>%
             str_remove_all("[^[0-9]]") %>%
             as.numeric() %>%
             replace_na(-2))
}
