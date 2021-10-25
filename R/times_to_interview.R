#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param source_infections
#' @return
#' @author Nick Golding
#' @export
times_to_interview <- function(source_infections) {

  # sample times from isolation to interview for a subset of infections - assume
  # independent of vaccination/symptom status for now
  
  delays <- .abm_parameters$isolation_to_interview_samples
  
  index <- sample.int(
    length(delays),
    nrow(source_infections),
    replace = TRUE
  )
  
  delays[index]
  
}
