#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param parameters
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
get_valid_abm_samples <- function(parameters,
                                 n_samples = 10,
                                 ...) {
  
  # for some reason replicate was wigging out about the dots

  params_list <- replicate(n_samples, parameters, simplify = FALSE)
  
  samples_list <- future.apply::future_lapply(
    X = params_list,
    FUN = get_valid_abm_sample,
    ...,
    future.seed = TRUE
  )

  names(samples_list) <- paste0("sim_", seq_len(n_samples))
  
  do.call(
    bind_rows,
    c(
      samples_list,
      list(.id = "simulation")
    )
  )
  
}
