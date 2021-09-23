#' Load Lab Results
#'
#' @param groups A vector containing at least one of `cases`, `primary-close-contacts`, `secondary-close-contacts`, `casual`.
#'
#' @return
#' @export
#'
#' @examples
loadLabResults <- function(groups = c("cases", "primary-close-contacts", "secondary-close-contacts", "casual")) {
  require(readxl)
  require(dplyr)

  lapply(groups, function(c) {
    if (c == "cases") {
      df <- read_excel("data/lab-results_Cases20210917.xlsx", guess_max = 1048576)
    }
    else if (c == "primary-close-contacts") {
      df <- read_excel("data/lab-results_PCC20210917.xlsx", guess_max = 1048576)
    }
    else if (c == "secondary-close-contacts") {
      df <- read_excel("data/lab-results_SCC20210917.xlsx", guess_max = 1048576)
    }
    else if (c == "casual") {
      df <- read_excel("data/lab-results_casual20210917.xlsx", guess_max = 1048576)
    }
    else {
      stop(paste0("Invalid group '", c, "' passed to `group` argument."))
    }

    setDT(df)

    df[["CreateDate"]] = as.Date(df[["CreateDate"]], "%d/%m/%Y")
    df[["TestDate"]] = as.Date(df[["TestDate"]], "%d/%m/%Y")
    df[["CollectionDate"]] = as.POSIXct(df[["CollectionDate"]], "%d/%m/%Y %H:%M:%S", tz="UTC")
    df[["SignatureDate"]] = as.POSIXct(df[["SignatureDate"]], "%d/%m/%Y %H:%M:%S", tz="UTC")

    df
  }) %>%
    rbindlist()
}
