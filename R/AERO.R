#' Calculate AERO Inputs
#' @description  Function for creating inputs to the AERO wind erosion model, requires height, line-point intercept, canopy gap, and soil texture observations for each plot.
#' @param gap_tall Table. Gap data in tall format
#' @param height_tall Table. Height data in tall format
#' @param lpi_tall Table. Line-point intercept data in tall format
#' @param header Table. Contains PrimaryKey, Latitude, and Longitude
#' @param texture_file Raster or csv. Soil texture raster(as Rdata file) with sand and clay percentages or CSV which provides soil texture classes from 12 USDA classes.
#' @param folder_location Character. Location for function to save AERO input files
#' @return AERO input files and an input_summary table which summarizes all input values in a single place.
#'
#' @export aero
#' @rdname AERO

aero<- function (lpi_tall,
                 gap_tall,
                 height_tall,
                 header,
                 texture_file,
                 folder_location){

  # Remove NAs from coordinates
  header <- header %>% subset(!is.na(Longitude_NAD83) &
                                !is.na(Latitude_NAD83))

  if (grepl(x = texture_file,
            pattern = ".csv$")){
    texture <- read.csv(texture_file) %>% dplyr::select(PrimaryKey, SoilTexture)
    plots_texture <- texture %>% dplyr::left_join(header) %>%
      dplyr::left_join(terradactyl::texture_class)


  } else if (grepl(x = texture_file,
                   pattern = ".Rdata$")) {
    texture_raster <- readRDS(texture_file)
    plots<-sp::SpatialPointsDataFrame(data=header,
                                      coords=cbind(y=header$Longitude_NAD83,
                                                   x=header$Latitude_NAD83),
                                      proj4string = texture_raster@crs)


    #extract soil texture values to plots
    plots_texture <- raster::extract( x=texture_raster,y=plots, df=TRUE, sp=TRUE)

    # Remove any plots without sand texture
    plots_texture <- subset(plots_texture,!is.na(sand))

    # Convert texture to fraction
    plots_texture$sand <- plots_texture$sand/100
    plots_texture$clay <- plots_texture$clay/100

    #AERO requires WGS84
    plots_texture<-sp::spTransform(plots_texture,
                                   CRSobj=sp::CRS("+proj=longlat +datum=WGS84"))

    #Add a SoilTexture field, just as an identifier
    plots_texture$SoilTexture <- NA

    plots_texture <- plots_texture@data

  } else {
    stop("Invalid texture file provided. Make sure it is either a raster (stored in Rdata) or a csv.")
  }

    # Calculate mean maximum height for each plot
  max_height <- mean_height(
    height_tall = height_tall,
    method = "max",
    omit_zero = TRUE,
    by_line = FALSE,
    tall = TRUE
  ) %>%
    # convert to meters
    dplyr::mutate(max_height = max_height/100)

  # Calculate bare soil from LPI data
  bare_soil<-pct_cover_bare_soil(lpi_tall = lpi_tall,
                                 tall = FALSE,
                                 by_line = FALSE)

  # subset gap_tall to only Canopy gaps
  canopy_gap <- subset(gap_tall, RecType == "C")

  # Find out which plots have bare soil, gap,  and height data
  common_PK <- Reduce(intersect, (list(
    unique(canopy_gap$PrimaryKey),
     unique(plots_texture$PrimaryKey),
    unique(max_height$PrimaryKey),
    unique(bare_soil$PrimaryKey)
  )))

  # because there may be multiple textures per plot, make a new identifier of common_pk + SoilTexture
  plots_texture <- plots_texture %>%
    dplyr::mutate(
      SoilTexture = SoilTexture %>% stringr::str_replace(" ", "_"),
      PK_texture = paste(PrimaryKey,SoilTexture, sep = "_")) %>%
    subset(PrimaryKey %in% common_PK)


  # Write Gap txt files of the raw gap observations
  # Create the gap folder location
  gap_location <- paste(folder_location, "gap/", sep = "")
  dir.create(gap_location)

  # Convert gaps to meters
  canopy_gap <- canopy_gap %>% dplyr::mutate(Gap = Gap/100)
  # Write files to gap location
  lapply(
    plots_texture$PK_texture,
    function(X) write.table(canopy_gap[canopy_gap$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X], "Gap"],
                            file = paste(folder_location, "gap/", X, ".txt", sep = ""),
                            col.names = F, row.names = F, sep = "\t"
    )
  )



  # Write the ini files out to folder and compile the list of files for the combo .bat files
  lapply(
    X = plots_texture$PK_texture,
    function(X) {
      cat(
        file = paste(folder_location, X, ".ini", sep = ""),
        "[INPUT_VALUES]",
                paste("wind_location:",
                      plots_texture$Latitude[plots_texture$PK_texture == X] %>% unique(),
                      plots_texture$Longitude[plots_texture$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X]]%>% unique(),
                  sep = " "
                ),
        paste("soil_sand_fraction: ",
              plots_texture$sand[plots_texture$PK_texture == X] %>% unique(),
              sep = ""),
        paste("soil_clay_fraction: ",
              plots_texture$clay[plots_texture$PK_texture == X] %>% unique(),
              sep = ""),
        paste("veg_cover_fraction: ",
              (100 - bare_soil$S[bare_soil$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X]]) %>% unique() / 100,
              sep = ""),
        paste("veg_mean_height: ",
              max_height$max_height[max_height$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X]] %>% unique(),
              sep = ""),
        paste("gap_obsv: ", "./gap/", X, ".txt", sep = ""),
        sep = "\n", append = FALSE

      )
    }
  )
  ## write combined input data to single file
  input_data <- dplyr::left_join(plots_texture, bare_soil) %>%
    dplyr::left_join(max_height) %>%
    dplyr::left_join( canopy_gap)

  write.csv(input_data, file = paste(folder_location, "input_data.csv", sep = ""))

}


