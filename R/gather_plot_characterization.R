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
#' @importFrom magrittr %>%
#' @name gather_plot_characterization
#' @family <gather>
#' @return A tall data frame containing plot characterization data

#' @export gather_plot_characterization_terradat
#' @rdname gather_plot_characterization
gather_plot_characterization_terradat <- function(dsn = NULL, tblPlots = NULL){
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
      EcolSite, ParentMaterial, Slope, Aspect, SlopeShape = ESD_SlopeShape,
      LandscapeType, LandscapeTypeSecondary, HillslopeType, 
      SoilSeries = ESD_Series,
      EstablishDate, State, County,
    ) %>% mutate(
      SlopeShapeVertical = case_when(
        SlopeShape %in% c("CC", "CV", "CL", "concave concave", "concave convex", "concave linear") ~ "concave",
        SlopeShape %in% c("LC", "LV", "LL", "linear concave", "linear convex", "linear linear") ~ "linear",
        SlopeShape %in% c("VC", "VV", "VL", "convex concave", "convex convex", "convex linear") ~ "convex"
      ),
      SlopeShapeHorizontal = case_when(
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
                                               file_type = file_type
                                               ) {
  ### input ####
  if (!is.null(POINT)){
    point_lmf_raw <- POINT
  } else if(!is.null(dsn)){
    point_lmf_raw <-
      switch(source, LMF = {
        sf::st_read(dsn = dsn, layer = "POINT", stringsAsFactors = FALSE, quiet = T)
      }, NRI = {
        readRDS(dsn)
      })
  } else{
    stop("One or more necessary inputs missing")
  }  

  # get slope shape from POINT
  point_lmf <- point_lmf_raw %>% dplyr::select(
    DBKey, PrimaryKey, PlotKey = PLOTKEY, SlopeShapeVertical = VERTICAL_SLOPE_SHAPE, 
    SlopeShapeHorizontal = HORIZONTAL_SLOPE_SHAPE,
    Slope = SLOPE_PERCENT, Aspect = SLOPE_ASPECT
  ) %>% dplyr::mutate(
    # reclass aspect into degrees
    Aspect = suppressWarnings(as.numeric(recode(Aspect,
                                                "0" = "N", "45" = "NE","90" = "E", "135" = "SE", 
                                                "180" = "S", "225" = "SW", "270" = "W","315" = "NW"))))
  return(soil_lmf)
}

#' @export gather_plot_characterization
#' @rdname gather_plot_characterization
gather_plot_characterization <- function(dsn = NULL,
                                         source,
                                         tblPlots = NULL,
                                         POINT = NULL,
                                         file_type = "gdb"){

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    plotchar <- gather_plot_characterization_terradat(dsn = dsn,
                                                      tblPlots = tblPlots
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    plotchar <- gather_plot_characterization_lmf(dsn = dsn,
                                                 #file_type = file_type,
                                                 POINT = POINT)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }
  
  plotchar$Source <- toupper(source)  
  
  if("sf" %in% class(plotchar)) plotchar <- sf::st_drop_geometry(plotchar)
  
  return(plotchar)
}

