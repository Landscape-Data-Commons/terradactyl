### Plot Characterization and Observation

## Plot Coordinates
coordinates <- function(dsn) {
  # Read in the coordinates
  coordinates <- sf::st_read(dsn,
    layer = "POINTCOORDINATES"
  )

  # Rename the variables
  coordinates <- dplyr::rename(coordinates,
    PrimaryKey = PLOTKEY,
    Latitude = FIELD_LATITUDE,
    Longitude = FIELD_LONGITUDE
  )

  # Make the latitudes negative (???)
  coordinates$Longitude <- -1 * coordinates$Longitude

  # Create a spatial points data frame from the coordinates where the data frame is the coordinates
  plots <- sp::SpatialPointsDataFrame(
    data = coordinates,
    coords = cbind(
      y = coordinates$Longitude,
      x = coordinates$Latitude
    ),
    proj4string = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  )

  return(plots)
}
