#' Change projection of coordinates within a data frame
#'
#' @param df the data frame containing the coordinates
#' @param xcol the name of the column containing the longitude coordinate
#' @param ycol the name of the column containing the latitude coordinate
#' @param crs_from the crs projection to convert from
#' @param crs_to the crs projection to convert to
#'
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#'
#' @return the original data frame with the coordinates converted
change_projection <- function(df, xcol, ycol, crs_from, crs_to) {

  df <- sf::st_as_sf(df, coords = c(xcol, ycol), crs = crs_from)

  df <- sf::st_transform(df, crs_to)

  df <- cbind(df, sf::st_coordinates(df))

  df <- sf::st_drop_geometry(df)

  names(df)[names(df) == "X"] <- xcol
  names(df)[names(df) == "Y"] <- ycol

  df
}

#' Derive the UTM EPSG coordinate system code for a given location
#'
#' @param lonlat a named numeric vector with `lon` and `lat` fields
#'
#' @return an integer reflecting the UTM EPSG code
lonlat_to_utm = function(lonlat = c("lon" = 0, "lat" = 52)) {
  utm = setNames((floor((lonlat["lon"] + 180) / 6) %% 60) + 1, nm = NULL)
  if(lonlat["lat"] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}
