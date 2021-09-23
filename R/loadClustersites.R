#' Load Cluster Sites
#'
#' Loads in the Vic DH cluster sites, which are the physical locations of outbreaks.
#'
#' @return a `data.table` containing the following columns:
#'
#' * `City`: Suburb of the site
#' * `ClusterID`: Unique identifier of the site (persists across datasets)
#' * `ParentClusterID`: Some outbreaks are 'sub-outbreaks' of a major outbreak. This field points to their parent.
#' * `ExposureCaseNumber`: The case that
#' * `ClusterType`: Outbreak (2 or more linked cases), complex case (1 case), placeholder, exposure site
#' * `DeclaredDate`: Date at which outbreak was declared
#' * `Division`: Public health division
#' * `Latitude`, `Longitude`
#' * `LGA`: Local government area of the site
#' * `Postcode`: Postcode of the site
#' * `PublicPrivateType`: Indicates if the site is a private setting or a public setting
#' * `Region`: DHHS Region
#' * `SiteCategory1`: High level categorisation of the outbreak site
#' * `SiteCategory2`: Sub-category of `SiteCategory1`
#' * `State`: State of site
#' * `ClusterClosedDate`: Date at which the outbreak was officially declared closed
#' @export
#'
loadClustersites <- function() {

  clustersites <- readxl::read_xlsx(path = "data/Cluster_site20210917.xlsx")
  setDT(clustersites)
  clustersites[["DeclaredDate"]] = as.Date(clustersites[["DeclaredDate"]], "%d/%m/%y")
  clustersites[["ClusterClosedDate"]] = as.Date(clustersites[["ClusterClosedDate"]], "%d/%m/%y")

  clustersites
}
