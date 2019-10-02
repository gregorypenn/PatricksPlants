#' Automated mapping of presence-absence data
#'
#' \code{map_presence_absence} creates a map of presence-absence data.
#'
#' Presence-absence data are mapped over a Stamen basemap. Overplotting
#' is addressed to some degree with transparence, by default alpha = 0.5.
#' Coordinate datum is assumed to be WGS84.
#'
#' @param data A dataframe with columns: presence, latitude, longitude.
#' @param maptype A Stamen maptype: terrain, terrain-background,
#'     terrain-labels, terrain-lines, toner, toner-2010, toner-2011,
#'     toner-background, toner-hybrid, toner-labels, toner-lines, toner-lite,
#'      or watercolor.
#' @param alpha A transparency value between 0 and 1.
#' @return  A plot of location data on a map.
#' @examples
#' boer4 <- presence_absence("BOER4", patricks_plants)
#' map_presence_absence(boer4)
#' @export
#' @import ggplot2
map_presence_absence <- function (df, maptype = "toner-lite", alpha = 0.5) {
  df$presence <-  factor(df$presence, labels = c("Absent", "Present"))

  bounding_box <- ggmap::make_bbox(lon = longitude, lat = latitude,
                            data = df)

  basemap <- ggmap::get_stamenmap(bounding_box,
                                  ggmap::calc_zoom(bounding_box),
                                  maptype = maptype,
                                  crop = FALSE)

  map <- ggmap::ggmap(basemap) +
    geom_point(aes(x = longitude, y = latitude, color = presence),
               alpha = alpha, data = df) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    guides(color = guide_legend(title = NULL))
  return(map)
}

#' Automated mapping of point locations
#'
#' \code{map_points} creates a map of longitude-latitude loction data.
#'
#' Point location data are mapped over a Stamen basemap. Overplotting
#' is addressed to some degree with transparence, by default alpha = 0.5.
#'
#' @param data A dataframe with columns: longitude, latitude.
#' @param maptype A Stamen maptype: terrain, terrain-background,
#'     terrain-labels, terrain-lines, toner, toner-2010, toner-2011,
#'     toner-background, toner-hybrid, toner-labels, toner-lines, toner-lite,
#'      or watercolor.
#' @param alpha A transparency value between 0 and 1.
#' @return  A plot of location data on a map.
#' @examples
#' map_points(patricks_plants)
#'
#' boer4 <- filter(patricks_plants, plant_id == "BOER4") # Requires library(dplyr)
#' map_points(boer4)
#' @export
#' @import ggplot2
map_points <- function (data, maptype = "toner-lite", alpha = 0.5) {

  bounding_box <- ggmap::make_bbox(lon = longitude, lat = latitude,
                                   data = data)

  basemap <- ggmap::get_stamenmap(bounding_box,
                                  ggmap::calc_zoom(bounding_box),
                                  maptype = maptype,
                                  crop = FALSE)

  map <- ggmap::ggmap(basemap) +
    geom_point(aes(x = longitude, y = latitude),
               alpha = alpha, data = data) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    guides(color = guide_legend(title = NULL))
  return(map)
}
