#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param infections
#' @return
#' @author Nick Golding
#' @export
do_contact_tracing <- function(infections) {

  # for all infections put into isolation today, assign their infectees an
  # isolation date
  
  # sample an interview day for all infections
  source_time_to_interview <- times_to_interview(infections)
  
  # sample a probability of detection for all infections
  contact_would_be_identified <- rbinom(
    nrow(infections),
    1,
    .abm_parameters$p_active_detection
  )
  
  # sample a different time to isolation for all infections
  contact_interview_to_isolation <- rpois(nrow(infections), 0.5)
  
  
  # which infections were isolated today?
  source_isolated_today <- infections$isolation_day == .abm_globals$day
  traced_source_ids <- infections$id[source_isolated_today]
  
  # index from each infection to these sources (many will be NA)
  traced_source_idx <- match(infections$source_id, traced_source_ids)
  
  # get the day of isolation for the contacts that are found, using the delay
  # from their source
  contact_isolation_day <- .abm_globals$day +
    source_time_to_interview[traced_source_idx] +
    contact_interview_to_isolation
  
  # determine who will be identified by contact tracing today
  contact_isolated <- infections$source_id %in% traced_source_ids &
    !is.finite(infections$isolation_day) &
    contact_would_be_identified
  
  # only use the isolation days for those isolated today
  infections$isolation_day <- ifelse(
    contact_isolated,
    contact_isolation_day,
    infections$isolation_day
  )
  
  # set their reason for detection
  infections$case_found_by <- ifelse(
    contact_isolated,
    "contact_tracing",
    infections$case_found_by
  )
    
  infections  

}
