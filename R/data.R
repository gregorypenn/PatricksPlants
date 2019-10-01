#' Plants observed at locations and dates
#'
#' A dataset containing Patrick Alexander's observations of plant occurance within a 10m radius
#' of his landscape photo locations. The data are coded as presence-only, but presence-absence
#' may be infered and functions are provided for the conversion on a species-specific basis.
#'
#' @format A data frame with 90880 rows and 37 variables:
#' \describe{
#'   \item{plant_id}{Unique identifier for taxon}
#'   \item{survey_id}{Unique identifier for site obervation}
#'   \item{date}{Date of site observation}
#'   \item{date_verbatim}{Date as character data-type}
#'   ...
#' }
#' @source \url{http://polyploid.net/PDPs/PJA_PDPs_3Sep19.xlsx}
"patricks_plants"
