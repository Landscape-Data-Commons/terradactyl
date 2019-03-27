#' Calculate AERO Inputs
#' @param gap_tall Table. Gap data in tall format
#' @param height_tall Table. Height data in tall format
#'
#' #' @export aero_gap
#' #' @rdname AERO
#' #'
#' #'
#' aero_gap <- function(gap_tall) {
#'   # Calculate gaps by breaks
#'
#'
#'  # Calculate the total number of gaps in each plot
#'   gap_probability <- gap %>%
#'     dplyr::group_by(PrimaryKey) %>%
#'     dplyr::summarise(total_n = sum(n)) %>%
#'     dplyr::left_join(gap, ., by = "PrimaryKey") %>%
#'     # Calculate the probability of gaps for each gap interval within each plot
#'     dplyr::group_by(PrimaryKey, gap_class) %>%
#'     dplyr::summarise(probability = n / total_n)
#'
#'   # Rename gap intervals so they are numeric
#'   gap$interval_value <- gsub(pattern = "\\[|\\,.+$", x = gap$gap_class, replacement = "") %>% as.numeric()
#'
#'   # Merge the gap cover and gap probability
#'   gap_output <- dplyr::left_join(gap, gap_probability, by = c("PrimaryKey", "gap_class"))
#' }
#' #

aero<- function (lpi_tall,
                 gap_tall,
                 height_tall,
                 header,
                 texture_raster,
                 folder_location="~/DataCommons/AERO/"){

  # Get the texture info for the plots
  # Remove NAs from coordinates
  header <- header %>% subset(!is.na(Longitude) &
                                !is.na(Latitude))
  plots<-sp::SpatialPointsDataFrame(data=header,
                                    coords=cbind(y=header$Longitude,
                                                 x=header$Latitude),
                                    proj4string = texture_raster@crs)


  #extract soil texture values to plots
  plots_texture <- raster::extract(y=plots, x=texture_raster, df=T, sp=T)

  # Remove any plots without sand texture
  plots_texture <- subset(plots_texture,!is.na(sand))

  # Convert texture to fraction
  plots_texture$sand <- plots_texture$sand/100
  plots_texture$clay <- plots_texture$clay/100

  #AERO requires WGS8f
  plots_texture<-sp::spTransform(plots_texture,
                                 CRSobj=sp::CRS("+proj=longlat +datum=WGS84"))


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
                                 by_year = FALSE,
                                 by_line = FALSE)


  # Find out which plots have bare soil and height data
  common_PK <- Reduce(intersect, (list(
    unique(gap_tall$PrimaryKey),
    unique(plots_texture$PrimaryKey),
    unique(max_height$PrimaryKey),
    unique(bare_soil$PrimaryKey)
  )))

  # Write Gap txt files of the raw gap observations
  # Create the gap folder location
  gap_location <- paste(folder_location, "gap/", sep = "")
  dir.create(gap_location)

  # Convert gaps to meters
  gap_tall <- gap_tall %>% dplyr::mutate(Gap = Gap/100)
  # Write files to gap location
  lapply(
    common_PK,
    function(X) write.table(gap_tall[gap_tall$PrimaryKey == X, "Gap"],
                            file = paste(folder_location, "gap/", X, ".txt", sep = ""),
                            col.names = F, row.names = F, sep = "\t"
    )
  )



  # Write the ini files out to folder and compile the list of files for the combo .bat files
  lapply(
    X = common_PK,
    function(X) {
      cat(
        file = paste(folder_location, X, ".ini", sep = ""),
        "[INPUT_VALUES]",
                paste("wind_location:",
                      plots_texture$Latitude[plots_texture$PrimaryKey == X] %>% unique(),
                      plots_texture$Longitude[plots_texture$PrimaryKey == X]%>% unique(),
                  sep = " "
                ),
        paste("soil_sand_fraction: ",
              plots_texture$sand[plots_texture$PrimaryKey == X] %>% unique(),
              sep = ""),
        paste("soil_clay_fraction: ",
              plots_texture$clay[plots_texture$PrimaryKey == X] %>% unique(),
              sep = ""),
        paste("veg_cover_fraction: ",
              (100 - bare_soil$S[bare_soil$PrimaryKey == X]) %>% unique() / 100,
              sep = ""),
        paste("veg_mean_height: ",
              max_height$max_height[max_height$PrimaryKey == X] %>% unique(),
              sep = ""),
        paste("gap_obsv: ", "./gap/", X, ".txt", sep = ""),
        sep = "\n", append = FALSE

      )
    }
  )

}


