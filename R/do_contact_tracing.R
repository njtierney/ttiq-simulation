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

  # get the source id of each infection
  source_idx <- match(infections$source_id, infections$id)
  
  # get the vaccination status of each source 
  source_vaccinated <- infections$vaccinated[source_idx]
  
  source_vaccinated <- ifelse(is.na(source_vaccinated), 
                              0, 
                              source_vaccinated)
  
  contact_vaccinated <- infections$vaccinated
  
  
  # which infections were isolated today?
  source_isolated_today <- infections$isolation_day == .abm_globals$day
  traced_source_ids <- infections$id[source_isolated_today]
  
  # index from each infection to these sources (many will be NA)
  traced_source_idx <- match(infections$source_id, traced_source_ids)
  
  # sample an interview day for all infections
  source_time_to_interview <- times_to_interview(infections)
  
  # enables different probabilities of detection of contacts of vaccinated/unvaccinated source
  #p_active_detection_unvaccinated_source <- .abm_parameters$p_active_detection
  #p_active_detection_vaccinated_source <- p_active_detection_unvaccinated_source*.abm_parameters$rel_active_detection_vaccinated_source
  
  p_active_detection_vaccinated_source_multiplier <- ifelse(source_vaccinated,
                                                            .abm_parameters$rel_active_detection_vaccinated_source,
                                                            1)
  
  p_active_detection_vaccinated_contact_multiplier <- ifelse(contact_vaccinated,
                                                            .abm_parameters$rel_active_detection_vaccinated_contact,
                                                            1)
  
  # probability that contacts (infectees) of the source (infector) are identified
  # source_prob_contact_identified <- ifelse(source_vaccinated, 
  #                                   p_active_detection_vaccinated_source, 
  #                                   p_active_detection_unvaccinated_source)
    
  source_prob_contact_identified <- .abm_parameters$p_active_detection*
    p_active_detection_vaccinated_source_multiplier*
    p_active_detection_vaccinated_contact_multiplier
                                     
  # sample a probability of detection for all infections
  contact_would_be_identified <- rbinom(
    nrow(infections),
    1,
    source_prob_contact_identified
  )
  
  # sample a different time to isolation for all infections
  contact_interview_to_isolation <- rpois(nrow(infections), 0.5)
  
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
  
  infections$isolated <- ifelse(
    contact_isolated,
    TRUE,
    infections$isolated
  )
  
  # set their reason for detection
  infections$case_found_by <- ifelse(
    contact_isolated,
    "contact_tracing",
    infections$case_found_by
  )
    
  infections  

}
