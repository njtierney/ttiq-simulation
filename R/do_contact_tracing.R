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
  
  # which infections were isolated today?
  source_isolated_today <- infections$isolation_day == .abm_globals$day
  source_ids <- infections$id[source_isolated_today]
  
  # sample an interview day for each of these
  source_time_to_interview <- times_to_interview(infections[source_isolated_today, ])
  
  # find the unisolated infectees of these infections
  
  # sample whether they would be identified in the interview
  contact_would_be_identified <- rbinom(
    nrow(infections),
    1,
    .abm_parameters$p_active_detection
  )
  
  # determine who is isolated
  contact_isolated <- infections$source_id %in% source_ids &
    !is.finite(infections$isolation_day) &
    contact_would_be_identified
  
  n_contacts_isolated <- sum(contact_isolated)
  # look up their date of isolation
  
  # index from the contacts to the sources
  source_idx <- match(infections$source_id[contact_isolated], source_ids)
  
  # sample a different time to isolation for each (infected) contact
  contact_interview_to_isolation <- rpois(n_contacts_isolated, 0.5)
  
  # get the day of isolation for the contacts that are found
  contact_isolation_day <- .abm_globals$day +
    source_time_to_interview[source_idx] +
    contact_interview_to_isolation

  infections$isolation_day <- ifelse(
    contact_isolated,
    contact_isolation_day,
    infections$isolation_day
  )
  
  infections$case_found_by <- ifelse(
    contact_isolated,
    "contact_tracing",
    infections$case_found_by
  )
    
  infections  

}
