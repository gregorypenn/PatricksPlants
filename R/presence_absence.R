# Requires patricks_plants.df, produced by
# Note: Indicates only whether a plant has ever or never been seen at a site.

#' Convert presence-only data to presence-absence for a species
#'
#' \code{presence_absence} converts from presence-only to presence-absence.
#'
#' The presence-absence may be infered from Patrick's presence-only data.
#' This function takes a presence-only data frame and a species name and
#' returns a presence-absence data frame.
#'
#' @param plant_id A character string corresponding to a plant_id
#' @param data A presence-only data frame.
#' @return  A data frame (tibble) with presence-absence data
#' @examples
#' presence_absence("BOER", patricks_plants)
#' @export
#' @importFrom magrittr %>%

presence_absence <- function (plant_id, data = patricks_plants) {

  plants_colnames <- c("family", "species", "usda_code", "plant_id", "common_name", "taxon_notes")
  id <- plant_id
  sites <- dplyr::select(data, -plants_colnames) %>%
    dplyr::distinct()

  presence <- dplyr::filter(data, plant_id == id) %>%
    dplyr::mutate(presence = TRUE)

  presence_absence <- dplyr::left_join(sites, presence)
  presence_absence$presence <- tidyr::replace_na(presence_absence$presence, FALSE)

  return(presence_absence)
}
