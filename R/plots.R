#' @param data Output from a gather function
#' @param plots Output from gather.plots()
#' @subset.by List of plots to subset or spatial  feature to subset by
#' @sp Logical. Indicates if this is a spatial query.

gather.coordinates<-function(dsn){

  ##Read plot data in
  #Try the TerrADat format
  try(plots<-sf::st_read(dsn=dsn,layer="tblPlots", quiet=TRUE), silent=TRUE )
  #Try the LMF Format
  if(!exists("plots")){
    try(plots<-sf::st_read(dsn=dsn,layer="POINTCOORDINATES"), silent=TRUE )
  }
  #Try the NRI format
  if(!exists("plots")){
    try(plots<-read.csv(paste(dsn, "pointcoordinates.txt"), sep="|"), silent=TRUE)
  }
  #If no valid plot file provided, send error
  if(!exists("plots")){
    stop("No valid plot coordinate filepath provided")
  }

  ##Clean Up LMF and NRI data

  if(!"PrimaryKey" %in%colnames(plots)){
    #Build PrimaryKey
    plots<-terradactyl::build.PK(plots)
    #Correct Longitude
    plots$FIELD_LONGITUDE<-plots$FIELD_LONGITUDE*-1
    #Select and Rename relevant fields
    plots<-select(plots, PrimaryKey, Latitude=FIELD_LATITUDE, Longitude=FIELD_LONGITUDE)
  }else{
    #extract just PrimaryKey and coordinates
    plots<-select(plots, PrimaryKey, Latitude, Longitude)
  }

  ##Make the layer spatial
  plots.sp<-sf::st_as_sf(plots, coords=c("Longitude", "Latitude"), crs=("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

  return(plots.sp)

}

#' @export
#' @rdname plots

build.PK<-function(df){
  df$PrimaryKey<-paste(df$SURVEY, df$STATE, df$COUNTY, df$PSU, df$POINT, sep="")
  return(df)
}
#' @export
#' @rdname plots
plot.query<-function(gathered.data,plots=NULL,extent, sp=TRUE){
  #Treat the query differently if it is spatial or aspatial
if(sp){
  #Get everything on same projection
  extent<-sf::st_transform(extent, st_crs(plots))
  #Subset all plots by extent
  plot.subset<-sf::st_intersection(plots, extent)
  #Subset gathered data by plots subset
  data.subset<-subset(gathered.data, PrimaryKey %in% plot.subset$PrimaryKey )
}else{
  #Aspatial subset
  data.subset<-subset(gathered.data, PrimaryKey%in%extent$PrimaryKey)
}

  return(data.subset)
}



