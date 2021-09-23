#' Load Victorian DH linelist from path
#'
#' Loads in the linelist (diagnosis info) and person (demographic info) and
#' returns one row per case.
.join_linelist_persons <- function(linelist_path, persons_path) {
  require(data.table)
  require(dplyr)
  require(readxl)

  linelist = read_excel(linelist_path, guess_max=1048576) %>%
    mutate_at(vars(ends_with("Date")), as.Date)
  persons = read_excel(linelist_path, guess_max=1048576) %>%
    mutate_at(vars(ends_with("Date")), as.Date)
  setDT(linelist)
  setDT(persons)

  person_vars = c("RecordID", names(persons)[!names(persons) %in% names(linelist)])

  persons[, ..person_vars][linelist, on="RecordID"]
}

#' Load Victorian DH linelist from names
#'
#' Loads in the linelist (diagnosis info) and person (demographic info) and
#' returns one row per case.
loadLinelist <- function(groups = c("cases", "primary-close-contacts", "secondary-close-contacts", "casual")) {
  require(dplyr)
  require(data.table)
  require(readxl)

  ret <- lapply(groups, function(c) {
    if (c == "cases") {
      df <- .join_linelist_persons("data/Linelist_Cases_20210917.xlsx", "data/Linelist_Cases_20210917.xlsx")
    }
    else if (c == "primary-close-contacts") {
      df <- .join_linelist_persons("data/Linelist_PCC_20210917.xlsx", "data/Linelist_PCC_20210917.xlsx")
    }
    else if (c == "secondary-close-contacts") {
      df <- .join_linelist_persons("data/Linelist_SCC_20210917.xlsx", "data/Linelist_SCC_20210917.xlsx")
    }
    else if (c == "casual") {
      df <- .join_linelist_persons("data/Linelist_casual_20210917.xlsx", "data/Linelist_casual_20210917.xlsx")
    }
    else {
      stop(paste0("Invalid group '", c, "' passed to `group` argument."))
    }
  }) %>% rbindlist()

  riskFactors = read_excel("data/Risk_factors_cases_contacts20210917.xlsx", guess_max = 1048576)[, c("RecordID", "CountryIfTravelOverseas", "StateIfTravelWithinAustralia")]
  setDT(riskFactors)

  ret %>% left_join(riskFactors, by="RecordID")
}
