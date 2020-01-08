#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @param header Dataframe. Plot header containing plot metadata
#' @param source String. Specifies data source, \code{"AIM", "LMF"}
#' @param ... Query in grepl format that subsets plots.
#' @return A \code{tbl} of indicators of either tall or wide format.


# Build the header portion of the terradat table
#' @export header_build_terradat
#' @rdname aim_gdb
header_build_terradat <- function(dsn, ...) {
  # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # tblPlots provides the link between species tables
  # (LPI, Height, Species Richness) and tblStateSpecies
  header <- sf::st_read(dsn = dsn, layer = "tblPlots") %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%

    # Select the field names we need in the final feature class
    dplyr::select(PrimaryKey, SpeciesState, PlotID, PlotKey, DBKey,
      EcologicalSiteId = EcolSite, Latitude_NAD83 =Latitude, Longitude_NAD83 = Longitude, State,
      County, DateEstablished = EstablishDate, DateLoadedInDb,
      ProjectName
    ) %>%

    # If there are any Sites with no PrimaryKeys, delete them
    subset(!is.na(PrimaryKey))

  # Return the header file
  return(header)
}

# Build the header portion of the LMF table
#' @export header_build_lmf
#' @rdname aim_gdb
header_build_lmf <- function(dsn, ...) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # Get the LMF points
  point <- sf::read_sf(dsn = dsn,
                       layer = "POINT") %>%
    # remove spatial attributes
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(
      PrimaryKey, SpeciesState,
      COUNTY, STATE, DBKey
    )

  # County and State are referred to by number codes, let's use the name
  point <- sf::st_read(dsn, layer = "COUNTYNM") %>%
    dplyr::select(COUNTY, COUNTYNM, STATE) %>%
    dplyr::left_join(point, .,
                     by = c("COUNTY", "STATE")) %>%
    dplyr::distinct() %>%


    # Add state
    dplyr::left_join(sf::st_read(dsn, layer = "STATENM"),
                     by = "STATE") %>%

    # pair down to needed fields
    dplyr::select(
      PrimaryKey = PrimaryKey.x,
      SpeciesState,
      DBKey = DBKey.x,
      County = COUNTYNM,
      State = STABBR
    ) %>%

    # add PLOTKEY
    # TODO I'm still not convinced we need this
    dplyr::mutate(PLOTKEY = PrimaryKey) %>%

    dplyr::distinct() %>%

    # Populate DateLoadedInDb
    dplyr::mutate(DateLoadedInDb = DBKey)

  # Get the field coordinates
  point_coordinate <- sf::st_read(dsn = dsn, layer = "POINTCOORDINATES") %>%
    as.data.frame() %>%
    dplyr::select(PrimaryKey,
                  Latitude_NAD83 = REPORT_LATITUDE,
                  Longitude_NAD83 = REPORT_LONGITUDE,
      LocationType
    ) %>%
    dplyr::left_join(point, .,
                     by = "PrimaryKey")

  # Add elevation data
  point_elevation <- sf::read_sf(
    dsn = dsn,
    layer = "GPS"
  ) %>%
    dplyr::select(PrimaryKey,
      DateVisited = CAPDATE, # The GPS capture date is the best approx
      ELEVATION
    ) %>%
    dplyr::left_join(point_coordinate, .,
      by = "PrimaryKey"
    ) %>%

    # Convert elevation to meters
    dplyr::mutate(ELEVATION = ELEVATION * 0.3048)

  # Add Ecological Site Id
  point_ESD <- sf::st_read(dsn, layer = "ESFSG") %>%
    dplyr::left_join(point_elevation, ., by = "PrimaryKey") %>%

    # If the ESD coverage !=all, figure what portion of the plot the dominant ESD
    # is ion the plot by taking the End_Mark-Start_Mark and dividng by the line length
    dplyr::mutate(
      ESD_coverage =
        dplyr::if_else(
          condition = COVERAGE == "all",
          true = as.integer(300),
          false = (END_MARK - START_MARK)
        ),
      EcologicalSiteId =
        paste(ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE, sep = "")
    ) %>%

    # Add up the coverage on each plot and get the percent coverage
    dplyr::group_by(PrimaryKey, EcologicalSiteId) %>%
    dplyr::summarise(PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) %>%

    # Arrange by ESD_coverage and find the dominant ecological site
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::arrange(dplyr::desc(PercentCoveredByEcoSite), .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

    # Join to point.elevation to build the final header
    dplyr::left_join(point_elevation, ., by = "PrimaryKey")


  # Return the point_ESD as the header file
  return(point_ESD)
}

# Build the header portion of the LMF table
#' @export header_build_nri
#' @rdname aim_gdb
header_build_nri <- function(dsn, ...) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # Get the LMF points
  point <- read.csv(file.path(dsn, "POINT.csv"), stringsAsFactors = FALSE) %>%
    # remove spatial attributes
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(
      PrimaryKey,
      COUNTY, STATE, DBKey
    )

  # County and State are referred to by number codes, let's use the name
  point <-  read.csv(file.path(dsn, "COUNTYNM.csv"), stringsAsFactors = FALSE) %>%
    dplyr::select(COUNTY, COUNTYNM, STATE) %>%
    dplyr::left_join(point, .,
                     by = c("COUNTY", "STATE")) %>%
    dplyr::distinct() %>%


    # Add state
    dplyr::left_join(read.csv(file.path(dsn, "STATENM.csv"), stringsAsFactors = FALSE),
                     by = "STATE") %>%

    # pair down to needed fields
    dplyr::select(
      PrimaryKey,
      DBKey,
      County = COUNTYNM,
      State = STABBR
    ) %>%

   dplyr::distinct() %>%

    # Populate DateLoadedInDb
    dplyr::mutate(DateLoadedInDb = DBKey)

  # Get the field coordinates
  point_coordinate <-  read.csv(file.path(dsn, "POINTCOORDINATES.csv"),
                                stringsAsFactors = FALSE) %>%
    dplyr::mutate(Latitude_NAD83 = dplyr::coalesce(FIELD_LATITUDE,
                                                   TARGET_LATITUDE),
                  Longitude_NAD83 = dplyr::coalesce(FIELD_LONGITUDE,
                                                    TARGET_LONGITUDE),
                  LocationType = dplyr::if_else(Latitude_NAD83 == TARGET_LATITUDE, "Target", "Field")) %>%
    dplyr::select(PrimaryKey,
                  Latitude_NAD83,
                  Longitude_NAD83,
                  LocationType) %>%
    dplyr::left_join(point, .,
                     by = "PrimaryKey")

  # Add elevation data
  point_elevation <- read.csv(file.path(dsn, "GPS.csv"),
                              stringsAsFactors = FALSE) %>%
    dplyr::select(PrimaryKey,
                  DateVisited = CAPDATE, # The GPS capture date is the best approx
                  ELEVATION,
                  DBKey
    ) %>%
    dplyr::left_join(point_coordinate, .,
                     by = c("PrimaryKey", "DBKey")
    ) %>%

    # Convert elevation to meters
    dplyr::mutate(ELEVATION = ELEVATION * 0.3048)

  # Add Ecological Site Id
  point_ESD <- read.csv(file.path(dsn, "ESFSG.csv"),
                        stringsAsFactors = FALSE) %>%
    dplyr::left_join(point_elevation, ., by = c("PrimaryKey", "DBKey")) %>%
    dplyr::distinct() %>%

    # If the ESD coverage !=all, figure what portion of the plot the dominant ESD
    # is ion the plot by taking the End_Mark-Start_Mark and dividng by the line length
    dplyr::mutate(
      ESD_coverage =
        dplyr::if_else(
          condition = COVERAGE == "all",
          true = as.integer(300),
          false = (END_MARK - START_MARK)
        ),
      EcologicalSiteId =
        paste(ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE, sep = "")
    ) %>%

    # Add up the coverage on each plot and get the percent coverage
    dplyr::group_by(PrimaryKey, DBKey,EcologicalSiteId) %>%
    dplyr::summarise(PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) %>%

    # Arrange by ESD_coverage and find the dominant ecological site
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey, DBKey) %>%
    dplyr::arrange(dplyr::desc(PercentCoveredByEcoSite), .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

    # Join to point.elevation to build the final header
    dplyr::left_join(point_elevation, ., by = c("PrimaryKey", "DBKey")) %>% dplyr::distinct()


  # Return the point_ESD as the header file
  return(point_ESD)
}
# Build the header wrapper
#' @export header_build
#' @rdname aim_gdb
# Header build wrapper function
header_build <- function(dsn, source, ...) {
  # Error check
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
  } )

  # Apply appropriate header function

  header <- switch(toupper(source),
    "LMF" = header_build_lmf(dsn = dsn, ...),
    "NRI" = header_build_lmf(dsn = dsn, ...),
    "TERRADAT" = header_build_terradat(dsn = dsn, ...),
    "AIM" = header_build_terradat(dsn = dsn, ...),
    "DIMA" = header_build_terradat(dsn = dsn, ...)
  )

  header$source <- source

  return(header)
}


#' @export lpi_calc
#' @rdname aim_gdb
# Calculate the LPI indicators
lpi_calc <- function(header,
                     lpi_tall,
                     species_file,
                     source) {

  # Join the lpi data to the header PrimaryKeys and add the StateSpecies Key
  lpi_tall_header <- readRDS(lpi_tall) %>%
    dplyr::left_join(dplyr::select(
      header,
      "PrimaryKey",
      "DBKey",
      "SpeciesState"
    ),
    .,
    by = c("PrimaryKey", "DBKey")
    )

  # Join to the state species list via the SpeciesState value
  lpi_species <- species_join(
    data = lpi_tall_header,
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else(source == "TerrADat",
                                               TRUE,
                                               FALSE)
  ) %>%
    dplyr::distinct()

  # Correct the Non-Woody to NonWoody
  lpi_species$GrowthHabit[grepl(
    pattern = "Non-woody|Nonwoody|Non-Woody",
    x = lpi_species$GrowthHabit
  )] <- "NonWoody"

  # If non-vascular in GrowthHabitSub, indicate that in GrowthHabit
  lpi_species$GrowthHabit[grepl(
    pattern = "NonVascular|Nonvascular|Non-vascular|Succulent",
    x = lpi_species$GrowthHabitSub
  )] <- "NA"

  # For the purposes of cover calcs, Non-Woody==Forb & Grass != Sedge, so we need to remove sedges
  lpi_species$GrowthHabit[lpi_species$GrowthHabitSub == "Sedge"] <- NA

  # Correct the Sub-shrub to SubShrub
  lpi_species$GrowthHabitSub[grepl(
    pattern = "Sub-Shrub|subshrub|Sub-shrub|Subshrub",
    x = lpi_species$GrowthHabitSub
  )] <- "SubShrub"

  # Calculate Total Foliar Cover ----
  total_foliar <- pct_cover_total_foliar(
    lpi_tall = lpi_species,
    tall = TRUE
  )

  # Calculate between plant cover (includes bare soil) ----
  between.plant.cover <- pct_cover_between_plant(
    lpi_tall = lpi_species,
    by_year = FALSE,
    by_line = FALSE,
    tall = TRUE
  )

  # Clean up indicator names so they are compatible with the AIM.gdb schema

  # Assign string replacements
  between.plant.replace <- c(
    "\\bL\\b" = "HerbLitter",
    "HL" = "HerbLitter",
    "AM" = "HerbLitter",
    "DN" = "HerbLitter",
    "ER" = "HerbLitter",
    "HT" = "NonVegLitter",
    "NL" = "NonVegLitter",
    "DS" = "DepSoil",
    "\\bD\\b" = "Duff",
    "LC" = "Lichen",
    "\\bM\\b" = "Moss",
    "WL" = "WoodyLitter",
    "CY" = "Cyanobacteria",
    "EL" = "EmbLitter",
    "\\bW\\b" = "Water",
    "WA" = "Water",
    "RF" = "Rock",
    "\\bR\\b" = "Rock",
    "GR" = "Rock",
    "CB" = "Rock",
    "ST" = "Rock",
    "BY" = "Rock",
    "VL" = "VagrLichen",
    "AG" = "BareSoil",
    "CM" = "BareSoil",
    "LM" = "BareSoil",
    "FG" = "BareSoil",
    "BR" = "Bedrock",
    "\\bS\\b" = "BareSoil",
    "[[:punct:]]" = ""
  )

  # Perform replacements
  between.plant.cover <- between.plant.cover %>%
    # Substitute the field names for those that are human readable
    dplyr::mutate(indicator = indicator %>%
      stringr::str_replace_all(., between.plant.replace)) %>%

    # Add FH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = "")) %>%

    # Remove "FH_" from the BareSoilCover indicator
    dplyr::mutate(indicator = indicator %>%
      stringr::str_replace(., "FH_BareSoilCover", "BareSoilCover"))

  # Because the renaming processing lumps categories,
  # we need to get a summed value (e.g., Soil =S+FG+LM_CM+AG)
  between.plant.cover <- between.plant.cover %>%
    dplyr::group_by(PrimaryKey, indicator) %>%
    dplyr::summarise(percent = sum(percent))

  # Add a Total Litter Indicator
  between.plant.cover <- between.plant.cover %>%
    # Filter Litter Indicators
    dplyr::filter(grepl(pattern = "Litter", x = indicator)) %>%
    # Sum all indicator hits
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(
      indicator = "FH_TotalLitterCover",
      percent = sum(percent)
    ) %>%
    # Add back to the rest of the between plant cover indicators
    dplyr::bind_rows(between.plant.cover, .)


  # Species Group Cover ----
  # Set the replacement values for valid indicator names ----
  spp.cover.replace <- c(
    "NONWOODY" = "ForbGrass",
    "NON" = "Non",
    "^NO\\." = "NonNox",
    "NO$" = "NonNox",
    "^YES" = "Nox",
    "ANNUAL" = "Ann",
    "PERENNIAL" = "Peren",
    "[[:punct:]]" = "",
    "GRAMINOID" = "Grass",
    "FORB" = "Forb",
    "NON" = "No",
    "SUBSHRUB" = "SubShrub",
    "SHRUB" = "Shrub",
    "SUCCULENT" = "Succulent",
    "TREE" = "Tree",
    " " = "",
    "STATURE" = "",
    "SAGEBRUSH" = "Sagebrush",
    "GRASS" = "Grass",
    "SHORT" = "Short",
    "TALL" = "Tall",
    "0" = "Live",
    "1" = "Dead",
    "PREFERRED" = "Preferred"

  )


  # Any hit cover ----
  ah_spp_group_cover <- dplyr::bind_rows(
    # cover by Noxious, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      Noxious, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      Noxious, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      Noxious
    ) %>% dplyr::mutate(indicator = paste(indicator, ".", sep = "")),

    # Cover by Noxious, Duration, GrowthHabit status
   test <- pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      Noxious, Duration, GrowthHabit
    ),

    # Sage Grouse Groups
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      SG_Group
    ),

    # Cover Duration and GrowthHabit
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "any",
      by_year = FALSE,
      by_line = FALSE,
      Duration, GrowthHabit
    )
  )


  if (source %in% c("TerrADat", "Survey123", "AIM")) {
    # Sagebrush live or dead
    ah_spp_group_cover <- dplyr::bind_rows(
      ah_spp_group_cover,
      pct_cover(lpi_species,
        tall = TRUE,
        hit = "any",
        by_year = FALSE,
        by_line = FALSE,
        SG_Group, chckbox
      )
    ) }


  # Fix to indicator names so they are valid for AIM.gdb
  ah_spp_group_cover <- ah_spp_group_cover %>%
    # Substitute "NonNox" for "NO
    dplyr::mutate(indicator = indicator %>%
      stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("AH_", indicator, "Cover", sep = "") %>%
      # Change the Sagebrush Live indicator sine it's slightly different
      stringr::str_replace_all(
        string = .,
        pattern = "AH_SagebrushLiveCover",
        replacement = "AH_SagebrushCover_Live"
      ))


  # First hit cover ----
  fh_spp_group_cover <- rbind(
    # cover by Noxious, Duration, and GrowthHabitSub combination
   test <- pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      Noxious, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      Noxious, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      Noxious
    ),
    # Cover by Noxious, Duration, GrowthHabit status
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      Noxious, Duration, GrowthHabit
    ),
    # Sage Grouse Groupings
    pct_cover(lpi_species,
      tall = TRUE,
      hit = "first",
      by_year = FALSE,
      by_line = FALSE,
      SG_Group
    )
  )

  fh_spp_group_cover <- fh_spp_group_cover %>%
    # Substitute for Field friendly names
    dplyr::mutate(indicator = indicator %>%
      stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add FH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = ""))




  # Combine  all LPI based cover indicators----
  lpi_cover <- dplyr::bind_rows(
    ah_spp_group_cover,
    fh_spp_group_cover,
    total_foliar,
    between.plant.cover) %>%

    dplyr::distinct() %>%

    # Spread to a wide format
    tidyr::spread(key = indicator, value = percent, fill = 0)


  #   SageBrush Shape, this is dependent on Shrub shape existing ----
  # TODO Need to check this with sagebrush state data


  sagebrush_shape_calc <- sagebrush_shape(
    lpi_tall = lpi_species,
    # NRI and LMF don't collect live v. dead
    live = dplyr::if_else(source %in% c("LMF", "NRI"),
      FALSE, TRUE
    )
  )

  lpi_indicators <- dplyr::left_join(lpi_cover,
    sagebrush_shape_calc,
    by = "PrimaryKey"
  )

  # For TerrADat only, get the data visited from the first line in LPI
  if (source %in% c("TerrADat", "AIM")) {
    lpi_indicators <- lpi_species %>%
      dplyr::select(PrimaryKey, FormDate) %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(DateVisited = dplyr::first(FormDate,
        order_by = FormDate
      ) %>%
        as.POSIXct()) %>%
      # Join to the lpi.cover data
      dplyr::left_join(lpi_indicators, ., by = "PrimaryKey")
  }

  # Return lpi_indicators
  return(lpi_indicators
         )
}


#' @export gap_calc
#' @rdname aim_gdb
# Calculate the Gap indicators for AIM
gap_calc <- function(header, gap_tall) {
  # tidy gap
  gap_tall <- readRDS(gap_tall) %>%

    # Subset to PrimaryKeys in the header
    subset(PrimaryKey %in% header$PrimaryKey)

  # Calculate indicators
  gap_calc <- gap_cover(
    gap_tall = gap_tall,
    tall = FALSE
  )$percent %>%
    dplyr::rowwise() %>%
    dplyr::select(PrimaryKey,
      GapCover_25_50 = "[25,51)",
      GapCover_51_100 = "[51,100)",
      GapCover_101_200 = "[100,200)",
      GapCover_200_plus = "[200,1e+05)"
    ) %>%

    # Calculate the summation indicator
    dplyr::mutate(GapCover_25_plus = sum(c(
      GapCover_25_50,
      GapCover_51_100,
      GapCover_101_200,
      GapCover_200_plus
    )))


  # Return
  return(gap_calc)
}


#' @export height_calc
#' @rdname aim_gdb
# Calculate the Height indicators for AIM
height_calc <- function(header, height_tall,
                        species_file = species_file,
                        source) {
  # gather tall height
  height <- readRDS(height_tall) %>%

    # subset by PK and add the SpeciesState from the header
    dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .)

  # Join to species list
  height_species <- species_join(
    data = height,
    data_code = "Species",
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else(source == "TerrADat",
                                               TRUE,
                                               FALSE)
  )

  # Correct the Non-Woody to NonWoody
  height_species$GrowthHabit[grepl(
    pattern = "Non-woody|Nonwoody|Non-Woody",
    x = height_species$GrowthHabit
  )] <- "NonWoody"

  # For any unresolved height errors, change height to "0" so
  # they are omitted from the calculations
  height_species$Height[toupper(height_species$GrowthHabit_measured)
  != toupper(height_species$GrowthHabit)] <- 0

  # Add a forb and grass category
  height_species$pgpf[height_species$Duration == "Perennial" &
    height_species$GrowthHabitSub %in%
      c("Forb/herb", "Forb", "Graminoid", "Grass")] <- "PerenForbGrass"

  # Height calculations----
  height_calc <- rbind(
    # Woody and Herbaceous heights
  mean_height(
      height_tall = height_species,
      method = "mean",
      omit_zero = TRUE, # remove zeros from average height calcs
      by_line = FALSE,
      tall = TRUE,
      type
    ) %>% subset(indicator %in% c("woody", "herbaceous")),

    # Forb or Grass
    mean_height(
      height_tall = height_species,
      method = "mean",
      omit_zero = TRUE, # remove zeros from average height calcs
      by_line = FALSE,
      tall = TRUE,
      GrowthHabitSub
    ) %>% subset(indicator %in% c("Forb", "Graminoid")),

    # Perennial Forb or Grass
    mean_height(
      height_tall = height_species,
      method = "mean",
      omit_zero = TRUE, # remove zeros from average height calcs
      by_line = FALSE,
      tall = TRUE,
      Duration, GrowthHabitSub
    ) %>% subset(indicator %in% c("Perennial.Forb", "Perennial.Graminoid")),

    # Perennial Forb and Grass as a single category
    mean_height(
      height_tall = height_species,
      method = "mean",
      omit_zero = TRUE, # remove zeros from average height calcs
      by_line = FALSE,
      tall = TRUE,
      pgpf
    ) %>% subset(indicator == "PerenForbGrass"),

    # Perennial grass by Noxious/NonNoxious
    mean_height(
      height_tall = height_species,
      method = "mean",
      omit_zero = TRUE, # remove zeros from average height calcs
      by_line = FALSE,
      tall = TRUE,
      Noxious, Duration, GrowthHabitSub
    ) %>% subset(indicator %in% c(
      "NO.Perennial.Graminoid",
      "YES.Perennial.Graminoid"
    )),


    # SG_Group heights
    mean_height(
      height_tall = height_species,
      method = "mean",
      omit_zero = TRUE, # remove zeros from average height calcs
      by_line = FALSE,
      tall = TRUE,
      SG_Group
    ) %>% subset(indicator != "NA")
  )

  # For TerrADat only
  if (source %in% c("TerrADat", "AIM")) {
    # Live sagebrush heights
    height_calc <- rbind(
      height_calc,
      mean_height(
        height_tall = height_species,
        method = "mean",
        omit_zero = TRUE, # remove zeros from average height calcs
        by_line = FALSE,
        tall = TRUE,
        SG_Group, Chkbox
      ) %>% subset(indicator == "Sagebrush.0")
    )
  }

  # Reformat for Indicator Field Name ----
  height_calc <- height_calc %>%
    dplyr::mutate(indicator = indicator %>%
      stringr::str_replace_all(c(
        "woody" = "Woody",
        "herbaceous" = "Herbaceous",
        "\\bYES\\b" = "Nox",
        "\\bNO\\b" = "NonNox",
        "Forb/herb" = "Forb",
        "Graminoid" = "Grass",
        "0" = "_Live",
        "\\." = "",
        " " = "",
        "Perennial" = "Peren",
        "Stature" = ""
      )) %>%
      paste("Hgt_", ., "_Avg", sep = ""))

  # Spread to wide format and return
  height_calc_wide <- height_calc %>% tidyr::spread(
    key = indicator,
    value = mean_height,
    fill = NA
  )

  return(height_calc_wide)
}


#################
#' @export spp_inventory_calc
#' @rdname aim_gdb
# Calculate species inventory
spp_inventory_calc <- function(header, spp_inventory_tall, species_file, source) {
  # tidy.species
  spp_inventory_tall <- readRDS(spp_inventory_tall) %>%
    # Join to the header to get the relevant PrimaryKeys and SpeciesSate
    dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .,
      by = "PrimaryKey"
    )

  # Join to State Species List
  spp_inventory_species <- species_join(
    data = spp_inventory_tall,
    data_code = "Species",
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else(source == "TerrADat",
                                               TRUE,
                                               FALSE)
  )

  # Count the number of species present in each group
  spp_inventory <- rbind(
    # Noxious & Non-Noxious
    species_count(spp_inventory_species, Noxious) %>%
      dplyr::mutate(indicator = indicator %>%
        stringr::str_replace_all(c(
          "YES" = "NoxPlant",
          "\\bNO\\b" = "NonNoxPlant"
        )) %>%
        stringr::str_replace_na(
          string = .,
          replacement = "NonNoxPlant"
        )),

    # Preferred Forb
    species_count(spp_inventory_species, SG_Group) %>%
      # Subset to only Preferred Forb
      subset(indicator == "PreferredForb") %>%
      dplyr::mutate(indicator = indicator %>%
        stringr::str_replace_all(c(" " = "")))
  ) %>%
    # Format for appropriat indicator name
    dplyr::mutate(indicator = paste("NumSpp_", indicator, sep = ""))

  # Spread to wide
  spp_inventory_wide <- spp_inventory %>% tidyr::spread(
    key = indicator,
    value = n,
    fill = 0
  )

  # Get the list of species that fall into a category (e.g., Preferred Forb)
  spp_list_sg <- spp_inventory_species %>%
    dplyr::select(PrimaryKey, Species, SG_Group) %>%
    dplyr::distinct() %>%
    dplyr::group_by(PrimaryKey, SG_Group) %>%
    dplyr::summarize(list = toString(Species) %>%
      stringr::str_replace_all(
        pattern = ",",
        replacement = ";"
      )) %>%
    # Format field names
    subset(!is.na(SG_Group)) %>%
    dplyr::mutate(indicator = SG_Group %>%
      stringr::str_replace_all(c(
        "Perennial" = "Peren",
        " " = "",
        "Stature" = ""
      )) %>%
      paste("Spp_", ., sep = "")) %>%
    dplyr::select(-SG_Group) %>%
    # Output in wide format
    tidyr::spread(key = indicator, value = list, fill = NA)

  spp_list_nox <- spp_inventory_species %>%
    dplyr::select(PrimaryKey, Species, Noxious) %>%
    dplyr::distinct() %>%
    dplyr::group_by(PrimaryKey, Noxious) %>%
    dplyr::summarize(list = toString(Species) %>%
      stringr::str_replace_all(
        pattern = ",",
        replacement = ";"
      )) %>%
    # Format field names
    subset(!is.na(Noxious)) %>%
    dplyr::mutate(indicator = Noxious %>%
      stringr::str_replace_all(c(
        "NO" = "NonNox",
        "YES" = "Nox",
        " " = ""
      )) %>%
      paste("Spp_", ., sep = "")) %>%
    dplyr::select(-Noxious) %>%
    # Output in wide format
    tidyr::spread(key = indicator, value = list, fill = NA)

  spp_list <- dplyr::full_join(spp_list_sg, spp_list_nox)

  # Join with spp_inventory and return
  spp_inventory <- dplyr::full_join(spp_inventory_wide, spp_list)

  return(spp_inventory)
}


#' @export soil_stability_calc
#' @rdname aim_gdb
# Calculate soil stability values
soil_stability_calc <- function(header, soil_stability_tall) {
  # Gather and subset
  soil_stability_tall <- readRDS(soil_stability_tall) %>%
    subset(!is.na(Rating)) %>%
    # subset to relevant PrimaryKeys
    subset(PrimaryKey %in% header$PrimaryKey)

  # Calculate indicators
  soil_stability_calcs <- soil_stability(soil_stability_tall,
    cover = TRUE
  ) %>%
    # Rename fields
    dplyr::rename(
      SoilStability_All = all,
      SoilStability_Protected = covered,
      SoilStability_Unprotected = uncovered
    )

  # Return
  return(soil_stability_calcs)
}

#' @export build_terradat_indicators
#' @rdname aim_gdb
# Build indicators feature class
build_terradat_indicators <- function(dsn,
                                      source,
                                      species_file,
                                      lpi_tall,
                                      gap_tall,
                                      height_tall,
                                      spp_inventory_tall,
                                      soil_stability_tall,
                                      ...) {
  # Test that source is  "TerrADat"
  if (!source %in% c("TerrADat")) {
    stop("Invalid indicator source specified")
  }

  # Assign filter expressions
  filter_exprs <- rlang::quos(...)

  # Read header in
  header <- header_build(dsn = dsn, source = source, !!!filter_exprs)

  # Join all indicator calculations together
  indicators <- list(
    header,
    # LPI
    lpi_calc(
      lpi_tall = lpi_tall,
      header = header,
      source = source,
      species_file = species_file
    ),
    # Gap
    gap_calc(
      gap_tall = gap_tall,
      header = header
    ),
    # Height
    height_calc(
      height_tall = height_tall,
      header = header,
      source = source,
      species_file = species_file
    ),
    # Species Inventory
   spp_inventory_calc(
      spp_inventory_tall = spp_inventory_tall,
      header = header,
      species_file = species_file,
      source = source
    ),
    # Soil Stability
    soil_stability_calc(
      soil_stability_tall = soil_stability_tall,
      header = header
    ),
    # Rangeland Health
    gather_rangeland_health(dsn,
      source = source
    ) %>%
      # Remove RecKey field
      dplyr::select(-RecKey)
  )

  all_indicators <- Reduce(dplyr::left_join, indicators)

  return(all_indicators)
}

# Build LMF Indicators
#' @export build_lmf_indicators
#' @rdname aim_gdb

build_lmf_indicators <- function(dsn, source,
                                 species_file,
                                 lpi_tall,
                                 gap_tall,
                                 height_tall,
                                 spp_inventory_tall,
                                 soil_stability_tall,...) {

  # Test that source is  "LMF"
  try(
    !source %in% c("LMF", "NRI"),
    stop("Invalid indicator source specified")
  )


  # Assign filter expressions
  filter_exprs <- rlang::quos(...)

  # Read header in
  header <- header_build(dsn, source = source, !!!filter_exprs)

  # Join all indicator calculations together
  indicators <- list(
    header,
    # LPI
    # LPI
   lpi_calc(
      lpi_tall = lpi_tall,
      header = header,
      source = source,
      species_file = species_file
    ),
    # Gap
    gap_calc(
      gap_tall = gap_tall,
      header = header
    ),
   #  # Height
   height_calc(
      height_tall = height_tall,
      header = header,
      source = source,
      species_file = species_file
    ),
    # Species Inventory
    spp_inventory_calc(
      spp_inventory_tall = spp_inventory_tall,
      header = header,
      species_file = species_file,
      source = source
    ),
    # Soil Stability
    soil_stability_calc(
      soil_stability_tall = soil_stability_tall,
      header = header
    ),
    # Rangeland Health
    IIRH <- gather_rangeland_health(dsn, source = source)
  )

  all_indicators <- Reduce(dplyr::left_join, indicators)

  return(all_indicators)
}

# Build LMF Indicators
#' @export build_indicators
#' @rdname aim_gdb
# Build wrapper
build_indicators <- function(dsn, source, lpi_tall,
                             species_file,
                             gap_tall,
                             height_tall,
                             spp_inventory_tall,
                             soil_stability_tall,...) {
  all_indicators <- switch(source,
    "TerrADat" = build_terradat_indicators(
      dsn = dsn,
      source = source,
      lpi_tall = lpi_tall,
      gap_tall = gap_tall,
      height_tall = height_tall,
      spp_inventory_tall = spp_inventory_tall,
      soil_stability_tall = soil_stability_tall,
      species_file = species_file,
      ...
    ),
    "AIM" = build_terradat_indicators(
      dsn = dsn,
      source = source,
      lpi_tall = lpi_tall,
      gap_tall = gap_tall,
      height_tall = height_tall,
      spp_inventory_tall = spp_inventory_tall,
      soil_stability_tall = soil_stability_tall,
      species_file = species_file,
      ...
    ),
    "LMF" = build_lmf_indicators(
      dsn = dsn,
      source = source,
      lpi_tall = lpi_tall,
      gap_tall = gap_tall,
      height_tall = height_tall,
      spp_inventory_tall = spp_inventory_tall,
      soil_stability_tall = soil_stability_tall,
      species_file = species_file,
      ...
    )
  )

   # TODO DELETE IN 2019!!!! ####
  # rename  fields that were mis-named in aim.gdb
  if("AH_PreferredForbCover" %in% names(all_indicators)){

    all_indicators <- all_indicators %>%
      dplyr::mutate(AH_PreferredForb = AH_PreferredForbCover)

  }

  # Compare indicator field names with the names for a the target feature class
  feature_class_field_names <- sf::st_read(dsn, layer = source)

  feature_class_field_names <- feature_class_field_names[,
    !colnames(feature_class_field_names) %in%
      c(
        "created_user",
        "created_date",
        "last_edited_user",
        "last_edited_date"
      )
  ]

  #
  indicator_field_names <- data.frame(
    name = names(all_indicators),
    calculated = "yes"
  )

  missing_names <- data.frame(
    name = names(feature_class_field_names),
    feature.class = "yes"
  ) %>%
    # Join feature class field names to indicator field names
    dplyr::full_join(indicator_field_names) %>%

    # get the field names where there is not corrollary in calculated
    subset(is.na(calculated), select = "name") %>%
    dplyr::mutate(value = NA) %>%
    # make into a data frame
    tidyr::spread(key = name, value = value) %>%
    dplyr::select(-Shape, -GlobalID)

  # Add a row for each PrimaryKey inall_indicators
  missing_names[nrow(all_indicators), ] <- NA
  # For some indicators, the null value is 0 (to indicate the method was completed,
  # but no data in that group were collected)
  missing_names[, grepl(names(missing_names), pattern = "^FH|^AH|^Num")] <- 0

  # Merge back to indicator data to create a feature class for export
  final_feature_class <- dplyr::bind_cols(all_indicators, missing_names)

  return(final_feature_class)
}
