###Plot Characterization and Observation

##Plot Coordinates
coordinates<-function (dsn){
  coordinates<-sf::st_read(dsn, layer="POINTCOORDINATES")

  coordinates<-dplyr::rename(coordinates, PrimaryKey=PLOTKEY, Latitude=FIELD_LATITUDE, Longitude=FIELD_LONGITUDE)

  coordinates$Longitude<--1*coordinates$Longitude
  plots<-sp::SpatialPointsDataFrame(data=coordinates,
                                    coords=cbind(y=coordinates$Longitude,x=coordinates$Latitude),
                                    proj4string = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))

}
