#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param parameter
#' @param fraction_az_dose_1
#' @param fraction_az_dose_2
#' @param fraction_pfizer_dose_1
#' @param fraction_pfizer_dose_2
#' @return
#' @author Nicholas Tierney
#' @export
ve <-
  function(parameter = c("susceptibility", "onward", "symptoms"),
           fraction_az_dose_1,
           fraction_az_dose_2,
           fraction_pfizer_dose_1,
           fraction_pfizer_dose_2) {
    # compute vaccine efficacy against susceptibility or onward transmission, for
    # any mix of product and number of doses
    total <- fraction_az_dose_1 +
      fraction_az_dose_2 +
      fraction_pfizer_dose_1 +
      fraction_pfizer_dose_2
    if (abs(total - 1) > 1e6) {
      stop("fractions do not sum to one")
    }
    parameter <- match.arg(parameter)
    ve_estimates <- list(
      susceptibility = list(
        az = list(dose_1 = 0.46,
                  dose_2 = 0.67),
        pfizer = list(dose_1 = 0.57,
                      dose_2 = 0.80)
      ),
      onward = list(
        az = list(dose_1 = 0.02,
                  dose_2 = 0.36),
        pfizer = list(dose_1 = 0.13,
                      dose_2 = 0.65)
      ),
      symptoms = list(
        az = list(dose_1 = 0.40,
                  dose_2 = 0.71),
        pfizer = list(dose_1 = 0.58,
                      dose_2 = 0.84)
      )
    )
    ve_estimates[[parameter]]$az$dose_1 * fraction_az_dose_1 +
      ve_estimates[[parameter]]$az$dose_2 * fraction_az_dose_2 +
      ve_estimates[[parameter]]$pfizer$dose_1 * fraction_pfizer_dose_1 +
      ve_estimates[[parameter]]$pfizer$dose_2 * fraction_pfizer_dose_2
  }
