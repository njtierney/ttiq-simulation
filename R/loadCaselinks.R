#' Load Vic DH Caselinks data
#'
#' Loads in caselinks (person-person and site-person) links from Victorian Department of Health
#'
#' @return a `data.table` containing the following columns:
#'
#'  * CreateDate: The date the link is created
#'  * `SourceRecordID`: Identifier of individual of link
#'  * `SourceCaseNumber`: Identifier of the individual of link - has a 1 to 1 corespondence with `TargetRecordID`.
#'  * `TargetRecordID`: Identifier of individual of link
#'  * `TargetCaseNumber`: Identifier of the individual of link - has a 1 to 1 corespondence with `TargetRecordID`.
#'  * `LinkBetween`: Whether the link is person to person or site to person
#'  * `Type`: Close contact, casual contact are the primary types of links, others used sparingly.
#'
#' @export
#'
loadCaselinks <- function() {
  caselinks <- readxl::read_xlsx(path = "data/Case_links_20210917.xlsx")
  setDT(caselinks)
  caselinks[["CreateDate"]] = as.Date(caselinks[["CreateDate"]], "%d/%m/%Y")

  caselinks
}

