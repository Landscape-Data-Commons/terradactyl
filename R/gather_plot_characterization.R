#' Convert plot data into a tall, tidy data frame
#'
#' @description Given wide format plot data, create a tall format data frame 
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file 
#' extension) of the geodatabase or text file containing the table of interest. 
#' This field is unnecessary if you provide either tblPlots (AIM/DIMA/TerrADat) 
#' or POINT (LMF/NRI).
#' @param source Character string. The data source format, 
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblPlots Dataframe of the data structure tblPlots from the 
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use when data
#' source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param POINT Dataframe of the data structure PINTERCEPT from the LMF/NRI 
#' database with the addition of PrimaryKey and DBKey fields. Use when source 
#' is LMF or NRI; alternately provide dsn.
#' @param POINTCOORDINATES Dataframe of the data structure POINTCOORDINATES from the LMF/NRI 
#' database with the addition of PrimaryKey and DBKey fields. Use when source 
#' is LMF or NRI; alternately provide dsn.
#' @param GPS Dataframe of the data structure GPS from the LMF/NRI 
#' database with the addition of PrimaryKey and DBKey fields. Use when source 
#' is LMF or NRI; alternately provide dsn.
#' #' @param file_type Character string that denotes the source file type of the 
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for 
#' AIM/DIMA/TerrADat, or if POINT, POINTCOORDINATES, and GPS are provided.
#' @importFrom magrittr %>%
#' @name gather_plot_characterization
#' @family <gather>
#' @return A tall data frame containing plot characterization data
#' @examples
#' gather_plot_characterization(dsn = "Path/To/AIM_Geodatabase.gdb", 
#'                              source = "AIM")
#' gather_plot_characterization(dsn = "Path/To/LMF_Geodatabase.gdb", 
#'                              source = "LMF")
#' 
#' aim_plots <- read.csv("Path/To/tblPlots.csv")
#' gather_plot_characterization(source = "AIM", 
#'                              tblPlots = aim_plots)
#' 
#' lmf_pintercept <- read.csv("Path/To/PINTERCEPT.csv")
#' lmf_pointcoords <- read.csv("Path/To/POINTCOORDINATES.csv")
#' lmf_gps <- read.csv("Path/To/GPS.csv")
#' gather_plot_characterization(source = "LMF", 
#'                              PINTERCEPT = lmf_pintercept,
#'                              POINTCOORDINATES = lmf_pointcoords,
#'                              GPS = lmf_gps)

#' @export gather_plot_characterization_terradat
#' @rdname gather_plot_characterization
gather_plot_characterization_terradat <- function(dsn = NULL, 
                                                  tblPlots = NULL){
  if(!is.null(tblPlots)){
    plot_raw <- tblPlots
  } else if(!is.null(dsn)){
    plot_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblPlots", stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("Supply either tblPlots or the path to a GDB containing that table")
  }
  
  plot_tall <- plot_raw %>%
    dplyr::select(
      PrimaryKey, DBKey,
      SpeciesState, 
      Latitude_NAD83 = Latitude, Longitude_NAD83 = Longitude, Zone, Datum,
      State, County,
      EcolSite, ParentMaterial, Slope, Aspect, SlopeShape = ESD_SlopeShape,
      LandscapeType, LandscapeTypeSecondary, HillslopeType, 
      SoilSeries = ESD_Series,
      EstablishDate
    ) %>% dplyr::mutate(
      SlopeShapeVertical = dplyr::case_when(
        SlopeShape %in% c("CC", "CV", "CL", "concave concave", "concave convex", "concave linear") ~ "concave",
        SlopeShape %in% c("LC", "LV", "LL", "linear concave", "linear convex", "linear linear") ~ "linear",
        SlopeShape %in% c("VC", "VV", "VL", "convex concave", "convex convex", "convex linear") ~ "convex"
      ),
      SlopeShapeHorizontal = dplyr::case_when(
        SlopeShape %in% c("CC", "LC", "VC", "concave concave", "linear concave", "convex concave") ~ "concave",
        SlopeShape %in% c("CL", "LL", "VL", "concave linear", "linear linear", "convex linear") ~ "linear",
        SlopeShape %in% c("CV", "LV", "VV", "concave convex", "linear convex", "convex convex") ~ "convex"
      ),
      Aspect = suppressWarnings(as.numeric(Aspect)),
    ) %>% dplyr::select(-SlopeShape)
  
  return(plot_tall)
}


#' @export gather_plot_characterization_lmf
#' @rdname gather_plot_characterization
gather_plot_characterization_lmf <-   function(dsn = NULL, 
                                               POINT = NULL,  
                                               POINTCOORDINATES = NULL,
                                               GPS = NULL,
                                               file_type = NULL
                                               ) {
  ### input ####
  ## update to have coord and gps
  if (!is.null(POINT) & !is.null(POINTCOORDINATES) & !is.null(GPS)){
    point_lmf_raw <- POINT
    coord_lmf_raw <- POINTCOORDINATES
    gps_lmf_raw   <- GPS
  } else if(!is.null(dsn)){
    point_lmf_raw <-
        sf::st_read(dsn = dsn, layer = "POINT", stringsAsFactors = FALSE, quiet = T)

    coord_lmf_raw <-
        sf::st_read(dsn = dsn, layer = "POINTCOORDINATES", stringsAsFactors = FALSE, quiet = T)

    gps_lmf_raw <-
        sf::st_read(dsn = dsn, layer = "GPS", stringsAsFactors = FALSE, quiet = T)

  } else{
    stop("Supply either POINT, POINTCOORDINATES, and GPS, or the path to a GDB containing those tables")
  }  

  # get slope shape from POINT
  point_lmf <- point_lmf_raw %>% dplyr::select(
    DBKey, PrimaryKey, 
    CountyNo = COUNTY, StateNo = STATE,
    SlopeShapeVertical = VERTICAL_SLOPE_SHAPE, 
    SlopeShapeHorizontal = HORIZONTAL_SLOPE_SHAPE,
    Slope = SLOPE_PERCENT, Aspect = SLOPE_ASPECT
  ) %>% dplyr::mutate(
    # reclass aspect into degrees
    Aspect = suppressWarnings(as.numeric(dplyr::recode(Aspect,
                                                "N" = "0", 
                                                "NE" = "45", 
                                                "E" = "90", 
                                                "SE" = "135", 
                                                "S" = "180", 
                                                "SW" = "225", 
                                                "W" = "270",
                                                "NW" = "315"))))
  
  # get gis data from POINTCOORDINATES
  coord_lmf <- coord_lmf_raw %>% dplyr::select(
   PrimaryKey, DBKey,
   Latitude_NAD83 = REPORT_LATITUDE, # is this the datum?
   Longitude_NAD83 = REPORT_LONGITUDE, # is this the datum?
  )
  
  # get gis from GPS
  gps_lmf <- gps_lmf_raw %>% dplyr::select(
    PrimaryKey, DBKey,
    Elevation = ELEVATION
  )
  
  # join GIS
  gis_lmf  <- dplyr::full_join(coord_lmf, gps_lmf, by = c("PrimaryKey", "DBKey"))
  
  # join gis and plot data
  plot_lmf <- dplyr::left_join(point_lmf, gis_lmf, by = c("PrimaryKey", "DBKey"))
  
  # last drop
  plot_lmf <- plot_lmf %>% dplyr::select(
    -c(Shape)
  )
  
  return(plot_lmf)
}

#' @export gather_plot_characterization
#' @rdname gather_plot_characterization
gather_plot_characterization <- function(dsn = NULL,
                                         source,
                                         tblPlots = NULL,
                                         POINT = NULL,
                                         POINTCOORDINATES = NULL,
                                         GPS = NULL,
                                         file_type = "gdb"){

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    plotchar <- gather_plot_characterization_terradat(dsn = dsn,
                                                      tblPlots = tblPlots)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    plotchar <- gather_plot_characterization_lmf(dsn = dsn,
                                                 file_type = file_type,
                                                 POINT = POINT,
                                                 POINTCOORDINATES = POINTCOORDINATES,
                                                 GPS = GPS)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }
  
  plotchar$source <- toupper(source)  
  
  if("sf" %in% class(plotchar)) plotchar <- sf::st_drop_geometry(plotchar)
  
  if (any(class(plotchar) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(plotchar)[do.call(rbind, vapply(plotchar, 
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    plotchar <- dplyr::mutate_at(plotchar, dplyr::vars(change_vars), 
                            dplyr::funs(as.character))
  }
  
  
  return(plotchar)
}
