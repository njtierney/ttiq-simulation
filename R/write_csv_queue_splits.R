#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param queue_splits
#' @return
#' @author Nicholas Tierney
#' @export
write_csv_queue_splits <- function(queue_splits) {
  
  # save those as two separate files (vaccinated/unvaccinated) for each scenario
  file_paths <- glue("outputs/{names(queue_splits)}.csv.gz")

  walk2(
    .x = queue_splits,
    .y = file_paths,
    .f = ~write_csv(x = .x, file = .y)
  )
  
  return(
    here(file_paths)
  )
  
}
