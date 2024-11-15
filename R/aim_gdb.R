#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @param header Dataframe. Plot header containing plot metadata
#' @param source String. Specifies data source, \code{"AIM", "LMF"}
#' @param ... Query in grepl format that subsets plots.
#' @return A \code{tbl} of indicators of either tall or wide format.


# Build the header portion of the terradat table
#' @export gather_header_terradat
#' @rdname aim_gdb
gather_header_terradat <- function(dsn = NULL, tblPlots = NULL,
                                   date_tables = NULL, ...) {
  # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # tblPlots provides the link between species tables
  if(!is.null(tblPlots)){
    header <- tblPlots
  } else if (!is.null(dsn)){
    # (LPI, Height, Species Richness) and tblStateSpecies
    header <- sf::st_read(
      dsn = dsn, layer = "tblPlots",
      stringsAsFactors = FALSE
    )
  } else {
    stop("Provide either tblPlots or a path to a GDB containing it")
  }

  header <- header %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs)

  # data from different sources / years capitalize this differently.
  if("DateLoadedInDB" %in% colnames(header) | !("DateLoadedInDb" %in% colnames(header))) {
    header$DateLoadedInDb <- header$DateLoadedInDB
  }

  # add these fields if missing
  if(!("Design" %in% colnames(header))) header$Design <- NA
  if(!("DesignFlag" %in% colnames(header))) header$DesignFlag <- NA
  if(!("Purpose" %in% colnames(header))) header$Purpose <- NA
  if(!("PurposeFlag" %in% colnames(header))) header$PurposeFlag <- NA
  if(!("ProjectName" %in% colnames(header))) header$ProjectName <- NA

  header <- header %>%
    # Select the field names we need in the final feature class
    dplyr::select(PrimaryKey, SpeciesState, PlotID, PlotKey, DBKey,
                  EcologicalSiteId = EcolSite, Latitude_NAD83 = Latitude, Longitude_NAD83 = Longitude, State,
                  Elevation,
                  County, DateEstablished = EstablishDate, DateLoadedInDb,
                  Design, DesignFlag, Purpose, PurposeFlag,
                  ProjectName
    ) %>%

    # If there are any Sites with no PrimaryKeys, delete them
    subset(!is.na(PrimaryKey))

  ## get date from all tables provided to date_tables
  # if date_tables is not provided, load all of lpi gap and species richness headers, as present in the geodatabase
  if(!is.null(dsn) & is.null(date_tables)){
    layernames <- sf::st_layers(dsn)$name
    if("tblLPIHeader" %in% layernames){
      print("Reading dates from tblLPIHeader")
      tblLPIHeader <- sf::st_read(dsn, "tblLPIHeader")
    }
    if("tblGapHeader" %in% layernames){
      print("Reading dates from tblGapHeader")
      tblGapHeader <- sf::st_read(dsn, "tblGapHeader")
    }
    if("tblSpecRichHeader" %in% layernames){
      print("Reading dates from tblSpecRichHeader")
      tblSpecRichHeader <- sf::st_read(dsn, "tblSpecRichHeader")
    }

    date_tables <- list(tblLPIHeader, tblGapHeader, tblSpecRichHeader)
  }

  if(is.null(date_tables) & is.null(dsn)){
    stop("date_tables must be provided if dsn is not. Provide a list of tables containing FormDate or collectDate")
  }

  if(class(date_tables) != "list"){
    stop("date_tables must be a list of minimum length 1")
  }


  # tblHorizontalFlux uses collectDate, most other tables use FormDate
  tblDate <- lapply(date_tables, function(date_table){
    if("FormDate" %in% colnames(date_table)) {
      out <- date_table %>% dplyr::select(PrimaryKey, Date = FormDate)
    } else if("collectDate" %in% colnames(date_table)) {
      out <- date_table %>% dplyr::select(PrimaryKey, Date = collectDate)
    } else {
      out <- data.frame()
    }

    return(out)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(DateVisited = dplyr::first(na.omit(Date), order_by = na.omit(Date)))

  header <- header %>% dplyr::left_join(tblDate, by = c("PrimaryKey"))

  # Return the header file
  return(header)
}

# Build the header portion of the LMF table
#' @export gather_header_lmf
#' @rdname aim_gdb
gather_header_lmf <- function(dsn = NULL,  ...) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  point <- sf::read_sf(
    dsn = dsn,
    layer = "POINT")

  point <- point %>%
    # remove spatial attributes
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(
      PrimaryKey, SpeciesState,
      COUNTY, STATE, DBKey
    )

  # County and State are referred to by number codes, let's use the name
  point <- sf::st_read(dsn,
                       layer = "COUNTYNM",
                       stringsAsFactors = FALSE
  ) %>%
    dplyr::select(COUNTY, COUNTYNM, STATE) %>%
    dplyr::left_join(point, .,
                     by = c("COUNTY", "STATE")
    ) %>%
    dplyr::distinct() %>%


    # Add state
    dplyr::left_join(sf::st_read(dsn,
                                 layer = "STATENM",
                                 stringsAsFactors = FALSE
    ),
    by = "STATE"
    ) %>%

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
    dplyr::mutate(PlotKey = PrimaryKey) %>%
    dplyr::distinct() %>%

    # Populate DateLoadedInDB
    dplyr::mutate(DateLoadedInDb = DBKey)

  # Get the field coordinates
  point_coordinate <- sf::st_read(
    dsn = dsn, layer = "POINTCOORDINATES",
    stringsAsFactors = FALSE
  ) %>%
    as.data.frame() %>%
    dplyr::select(PrimaryKey,
                  Latitude_NAD83 = REPORT_LATITUDE,
                  Longitude_NAD83 = REPORT_LONGITUDE,
                  LocationType
    ) %>%
    dplyr::left_join(point, .,
                     by = "PrimaryKey"
    )

  # Add elevation data
  point_elevation <- sf::read_sf(
    dsn = dsn,
    layer = "GPS"
  ) %>%
    dplyr::select(PrimaryKey,
                  DateVisited = CAPDATE, # The GPS capture date is the best approx
                  Elevation = ELEVATION
    ) %>%
    dplyr::left_join(point_coordinate, .,
                     by = "PrimaryKey"
    ) %>%

    # Convert elevation to meters
    dplyr::mutate(Elevation = Elevation * 0.3048)

  # Add Ecological Site Id
  point_ESD_raw <- sf::st_read(dsn,
                               layer = "ESFSG",
                               stringsAsFactors = FALSE
  )

  # add in ESFSG_PREFIX column to old data in order to keep up with LMF schema changes
  if(!"ESFSG_PREFIX" %in% colnames(point_ESD_raw)) point_ESD_raw$ESFSG_PREFIX <- ""

  point_ESD <- point_ESD_raw %>%
    dplyr::left_join(point_elevation, ., by = "PrimaryKey") %>%

    # If the ESD coverage !=all, figure what portion of the plot the dominant ESD
    # is on the plot by taking the End_Mark-Start_Mark and dividng by the line length
    dplyr::mutate(
      ESD_coverage =
        dplyr::if_else(
          condition = COVERAGE == "all",
          true = as.integer(300),
          false = (END_MARK - START_MARK)
        ),
      # LMF schema is being updated to include F/R prefix under the column ESFSG_PREFIX
      # replace NA's with "" in ESFSG_PREFIX
      ESFSG_PREFIX = tidyr::replace_na(ESFSG_PREFIX, ""),
      EcologicalSiteId = trimws(paste(ESFSG_PREFIX, ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE, sep = "")),
      MLRA = ESFSG_MLRA %>% gsub("^$", NA, .)
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
  point_ESD <- point_ESD %>% dplyr::mutate(PlotID = PrimaryKey)

  return(point_ESD)
}

# Build the header portion of the LMF table
#' @export gather_header_nri
#' @rdname aim_gdb
gather_header_nri <- function(dsn = NULL, speciesstate, ...) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # if(!is.null(POINT)){
  #   point <- POINT
  # } else if(!is.null(dsn)){
  point <- read.csv(file.path(dsn, "POINT.csv"), stringsAsFactors = FALSE)
  # } else {
  #   stop("Provide either POINT or a path to a folder containing it")
  # }

  # Get the LMF points
  point <- point %>%
    # remove spatial attributes
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    # dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(
      PrimaryKey,
      COUNTY, STATE, DBKey
    )

  # County and State are referred to by number codes, let's use the name
  point <- read.csv(file.path(dsn, "COUNTYNM.csv"), stringsAsFactors = FALSE) %>%
    dplyr::select(COUNTY, COUNTYNM, STATE) %>%
    dplyr::left_join(point, .,
                     by = c("COUNTY", "STATE")
    ) %>%
    dplyr::distinct() %>%


    # Add state
    dplyr::left_join(read.csv(file.path(dsn, "STATENM.csv"), stringsAsFactors = FALSE),
                     by = c("STATE", "DBKey")
    ) %>%

    # pair down to needed fields
    dplyr::select(
      PrimaryKey,
      DBKey,
      County = COUNTYNM,
      State = STABBR
    ) %>%
    dplyr::distinct() %>%

    # Populate DateLoadedInDB
    dplyr::mutate(DateLoadedInDB = DBKey)

  # Get the field coordinates
  point_coordinate <- read.csv(file.path(dsn, "POINTCOORDINATES.csv"),
                               stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      Latitude_NAD83 = dplyr::coalesce(
        FIELD_LATITUDE,
        TARGET_LATITUDE
      ),
      Longitude_NAD83 = dplyr::coalesce(
        FIELD_LONGITUDE,
        TARGET_LONGITUDE
      ),
      LocationType = dplyr::if_else(Latitude_NAD83 == TARGET_LATITUDE, "Target", "Field")
    ) %>%
    dplyr::select(
      PrimaryKey,
      Latitude_NAD83,
      Longitude_NAD83,
      LocationType
    ) %>%
    dplyr::left_join(point, .,
                     by = "PrimaryKey"
    )

  # Add elevation data
  point_elevation <- read.csv(file.path(dsn, "GPS.csv"),
                              stringsAsFactors = FALSE
  ) %>%
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
                        stringsAsFactors = FALSE
  ) %>%
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
    dplyr::group_by(PrimaryKey, DBKey, EcologicalSiteId) %>%
    dplyr::summarise(PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) %>%

    # Arrange by ESD_coverage and find the dominant ecological site
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey, DBKey) %>%
    dplyr::arrange(dplyr::desc(PercentCoveredByEcoSite), .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

    # Join to point.elevation to build the final header
    dplyr::left_join(point_elevation, ., by = c("PrimaryKey", "DBKey")) %>%
    dplyr::distinct()

  # get a vector of which rows need prefixing
  point_ESD <- point_ESD %>% dplyr::mutate(
    EcologicalSiteId = dplyr::case_when(stringr::str_detect(point_ESD$EcologicalSiteId, "^[0-9]") ~
                                          paste0("R", EcologicalSiteId),
                                        TRUE ~ EcologicalSiteId),
    EcologicalSiteId = stringr::str_trim(EcologicalSiteId)
  )

  # Attach SpeciesState
  point_ESD$SpeciesState <- speciesstate

  # Create PlotID, which is needed in later functions
  point_ESD <- point_ESD %>% dplyr::mutate(PlotID = PrimaryKey)

  # Return the point_ESD as the header file
  return(point_ESD)
}

# Build the header portion of the Survey123 table
#' export gather_header_survey123
#' rdname aim_gdb
# gather_header_survey123 <- function(PlotChar, speciesstate, ...){
#     # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
#     filter_exprs <- rlang::quos(...)
#
#     header <- PlotChar %>%
#       as.data.frame() %>%
#
#       # Filter using the filtering expression specified by user
#       dplyr::filter(!!!filter_exprs)
#
#     # Add these fields, to match terradat formatting
#     header$Design <- NA
#     header$DesignFlag <- NA
#     header$Purpose <- NA
#     header$PurposeFlag <- NA
#     header$ProjectName <- NA
#     header$State <- NA
#     header$DBKey <- NA
#     header$County <- NA
#     header$DateLoadedInDb <- NA
#     header$SpeciesState <- speciesstate
#     header$PrimaryKey <- header$PlotKey
#
#
#     header <- header %>%
#       # Select the field names we need in the final feature class
#       dplyr::select(PrimaryKey, PlotID, PlotKey, DBKey, DateVisited = DateFormat,
#                     EcologicalSiteId = Ecolsite, Latitude_NAD83 = y, Longitude_NAD83 = x, State, SpeciesState,
#                     County, DateEstablished = EstabDate,
#                     DateLoadedInDb,
#                     Design, DesignFlag, Purpose, PurposeFlag
#       ) %>%
#
#       # If there are any Sites with no PrimaryKeys, delete them
#       subset(!is.na(PrimaryKey))
#
#     # alert to  duplicate primary keys
#     dupkeys <- header$PrimaryKey[duplicated(header$PrimaryKey)]
#     if(length(dupkeys) > 0){
#       dupnames <- paste(unique(dupkeys), collapse = ", ")
#       warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#     }
#
#     # Return the header file
#     return(header)
# }
#
# Build the header wrapper
#' @export gather_header
#' @rdname aim_gdb
# Header build wrapper function
gather_header <- function(dsn = NULL, source, tblPlots = NULL, date_tables = NULL, #PlotChar_0 = NULL,
                          speciesstate = NULL, ..., autoQC = TRUE) {
  # Error check
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
  })

  # Apply appropriate header function

  header <- switch(toupper(source),
                   "LMF" = gather_header_lmf(dsn = dsn, ...),
                   "NRI" = gather_header_nri(dsn = dsn, speciesstate = speciesstate, ...),
                   "TERRADAT" = gather_header_terradat(dsn = dsn, tblPlots = tblPlots, date_tables = date_tables, ...),
                   "AIM" = gather_header_terradat(dsn = dsn, tblPlots = tblPlots, date_tables = date_tables, ...),
                   "DIMA" = gather_header_terradat(dsn = dsn, tblPlots = tblPlots, date_tables = date_tables, ...)#,
                   # "SURVEY123" = gather_header_survey123(PlotChar = PlotChar_0, speciesstate = speciesstate)
  )

  header$source <- source

  if("sf" %in% class(header)) header <- sf::st_drop_geometry(header)

  # Apply QC helper functions to remove duplicates
  if(autoQC){
    message("Checking for duplicated rows. Disable by adding the parameter 'autoQC = FALSE'")
    header <- tdact_remove_duplicates(header)
  }

  return(header)
}


#' @export lpi_calc
#' @rdname aim_gdb
# Calculate the LPI indicators
lpi_calc <- function(header,
                     lpi_tall,
                     species_file,
                     source,
                     dsn) {

  print("Beginning LPI indicator calculation")
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

  # check for generic species in Species list
  if (source %in% c("LMF", "AIM", "TerrADat")) {
    species_list <- sf::st_read(
      dsn = dsn,
      layer = "tblStateSpecies",
      stringsAsFactors = FALSE
    ) %>%
      # Get unknown codes and clean them up. Unknown codes beging with a 2 (LMF/NRI)
      # or a 2 letter prefix followed by a number.
      # Older projects also used "AAFF" etc. to identify unknown and dead
      # beyond recognition codes. So we'll need to detect those too
      dplyr::filter(stringr::str_detect(
        string = SpeciesCode,
        pattern = "^2[[:alpha:]]|^[A-z]{2}[[:digit:]]"
      ) &
        is.na(Notes))

    try(if (nrow(species_list) > 0) {
      stop(
        "Invalid generic species codes present in species list.
       Please resolve before calculating indicators."
      )
    })
  }

  # Join to the state species list via the SpeciesState value
  if (verbose) {
    message("Joining species information to the LPI data.")
  }
  lpi_species <- species_join(
    data = current_data,
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else(
      source == "TerrADat",
      TRUE,
      FALSE
    )
  ) |>
    dplyr::distinct()

  # Correct the Non-Woody to NonWoody
  lpi_species$GrowthHabit[grepl(
    pattern = "Non-woody|Nonwoody|Non-Woody",
    x = lpi_species$GrowthHabit
  )] <- "NonWoody"

  # Add a Forb Graminoid field
  lpi_species$ForbGraminoid[lpi_species$GrowthHabitSub %in%
                            c("Forb/herb", "Forb", "Graminoid", "Grass", "Forb/Herb",
                             "FORB", "FORB/HERB", "GRAMINOID", "GRASS")] <- "ForbGraminoid"

  # If non-vascular in GrowthHabitSub, indicate that in GrowthHabit
  lpi_species$GrowthHabit[grepl(
    pattern = "NonVascular|Nonvascular|Non-vascular|Succulent",
    x = lpi_species$GrowthHabitSub
  )] <- "NA"

  # If non-vascular in GrowthHabit,null it out
  lpi_species$GrowthHabit[grepl(
    pattern = "NonVascular|Nonvascular|Non-vascular|Succulent",
    x = lpi_species$GrowthHabit
  )] <- "NA"

# keeping sedges since we are now doing Graminoid calculations
  # For the purposes of cover calcs, Non-Woody==Forb & Grass != Sedge, so we need to remove sedges
  # lpi_species$GrowthHabit[lpi_species$GrowthHabitSub == "Sedge"] <- NA

  # Correct the Sub-shrub to SubShrub
  lpi_species$GrowthHabitSub[grepl(
    pattern = "Sub-Shrub|subshrub|Sub-shrub|Subshrub",
    x = lpi_species$GrowthHabitSub
  )] <- "SubShrub"

  # Add a Shrub/SubShrub/Succulent field
  lpi_species$ShrubSucculent[grepl(
    pattern = "SubShrub|Shrub|Succulent",
    x = lpi_species$GrowthHabitSub
  )] <- "ShrubSucculent"

  # Calculate Total Foliar Cover ----
  total_foliar <- pct_cover_total_foliar(
    lpi_tall = lpi_species,
    tall = TRUE,
    by_line = FALSE
  )

  # Calculate between plant cover (includes bare soil) ----
  between.plant.cover <- pct_cover_between_plant(
    lpi_tall = lpi_species,
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
    "AL" = "NonVegLitter",
    "TH" = "HerbLitter",
    "DL" = "HerbLitter",
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
    "PC" = "BareSoil",
    "SA" = "BareSoil",
    "BR" = "Rock",
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
                    stringr::str_replace(., "FH_BareSoilCover", "BareSoil"))

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

  # Any hit litter ####
  lpi_species_litter <- lpi_species %>%
    dplyr::mutate(
      Litter = dplyr::case_when(
        code %in% c("HL", "L", "DN", "ER", "AM", "TH","DL") ~ "HerbLitter",
        code %in% "WL" ~ "WoodyLitter"
      ),
      TotalLitter = dplyr::case_when(
        code %in% c(
          "HL",
          "L",
          "DN",
          "ER",
          "AM",
          "WL",
          "NL",
          "EL",
          "HT",
          "AL",
          "TH",
          "DL"
        ) ~ "TotalLitter"
      )
    )

  litter <- pct_cover(lpi_species_litter,
                      tall = TRUE,
                      by_line = FALSE,
                      hit = "any",
                      Litter
  ) %>%
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "HERBLITTER" ~ "HerbLitter",
      indicator == "WOODYLITTER" ~ "WoodyLitter"
    ))

  total_litter <- pct_cover(lpi_species_litter,
                            tall = TRUE,
                            hit = "any",
                            by_line = FALSE,
                            TotalLitter
  ) %>%
    dplyr::mutate(indicator = indicator %>% dplyr::recode("TOTALLITTER" = "TotalLitter"))

  litter <- dplyr::bind_rows(litter, total_litter) %>%
    dplyr::mutate(indicator = paste("AH_", indicator, "Cover", sep = ""))


  # Species Group Cover ----
  # Set the replacement values for valid indicator names ----
  spp.cover.replace <- c(
    "NON" = "Non",
    "^NO\\." = "NonNox",
    "NO$" = "NonNox",
    "^YES" = "Nox",
    "ANNUAL" = "Ann",
    "PERENNIAL" = "Peren",
    "[[:punct:]]" = "",
    "GRAMINOID" = "Graminoid",
    "FORB" = "Forb",
    "NON" = "No",
    "SUBSHRUB" = "SubShrub",
    "SHRUB" = "Shrub",
    "SUCCULENT" = "Succulent",
    "TREE" = "Tree",
    " " = "",
    "STATURE" = "",
    "SAGEBRUSH" = "Sagebrush",
    "GRASS" = "Graminoid",
    "SHORT" = "Short",
    "TALL" = "Tall",
    "0" = "Live",
    "1" = "Dead",
    "PREFERRED" = "Preferred",
    "WOODY" = "Woody"
  )


  # Any hit cover ----
  ah_spp_group_cover <- dplyr::bind_rows(
    # cover by Noxious, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Noxious, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Noxious, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Noxious
    ) %>% dplyr::mutate(indicator = paste(indicator, ".", sep = "")),

    # Cover by Noxious, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Noxious, Duration, GrowthHabit
    ),

    # Sage Grouse Groups
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              SG_Group
    ),

    # Cover Duration and GrowthHabit
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Duration, GrowthHabit
    ),

    # Cover by GrowthHabit
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              GrowthHabit
    ),

    # Shrub/Succulent Cover
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              ShrubSucculent
    ),
    # Forb Graminoid Cover by Duration
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Duration, ForbGraminoid
    )
  )


  if (source %in% c("TerrADat", "Survey123", "AIM")) {
    # Sagebrush live or dead
    ah_spp_group_cover <- dplyr::bind_rows(
      ah_spp_group_cover,
      pct_cover(lpi_species,
                tall = TRUE,
                hit = "any",
                by_line = FALSE,
                SG_Group, chckbox
      )
    )
  }
  lpi_species <- dplyr::mutate(.data = lpi_species,
                               # Update the Duration values so that we don't
                               # need to do special renaming of indicators.
                               # This also lumps biennials in with annuals.
                               Duration = dplyr::case_when(grepl(x = Duration,
                                                                 pattern = "perennial",
                                                                 ignore.case = TRUE) ~ "Peren",
                                                           grepl(x = Duration,
                                                                 pattern = "(annual)|(biennial)",
                                                                 ignore.case = TRUE) ~ "Ann",
                                                           .default = Duration),
                               # Updates to the GrowthHabit variable to harmonize
                               # values with expectations, including adding a
                               # new value for nonvasculars which shifts those
                               # qualifying species out of the general nonwoody
                               # calculations
                               GrowthHabit = dplyr::case_when(grepl(x = GrowthHabit,
                                                                    pattern = "^non-?woody$",
                                                                    ignore.case = TRUE) ~ "NonWoody",
                                                              grepl(x = GrowthHabitSub,
                                                                    pattern = "^non-?vascular$",
                                                                    ignore.case = TRUE) ~ "Nonvascular",
                                                              # This removes sedges from consideration???
                                                              # Maybe an artifact of trying to avoid spitting
                                                              # out unused indicators
                                                              GrowthHabitSub == "Sedge" ~ NA,
                                                              .default = GrowthHabit),
                               # Updates to GrowthHabitSub, mostly harmonizing
                               # variations on naming conventions
                               GrowthHabitSub = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                       pattern = "forb",
                                                                       ignore.case = TRUE) ~ "Forb",
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^sub-?shrub$",
                                                                       ignore.case = TRUE) ~ "SubShrub",
                                                                 # Not sure why we're removing non-vasculars??
                                                                 # Maybe an artifact of trying to avoid spitting
                                                                 # out unused indicators. Blame Alaska.
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^non-?vascular$",
                                                                       ignore.case = TRUE) ~ NA,
                                                                 # Anyway, doing the exact same to moss
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^moss$",
                                                                       ignore.case = TRUE) ~ NA,
                                                                 # And to lichen
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^lichen$",
                                                                       ignore.case = TRUE) ~ NA,
                                                                 .default = GrowthHabitSub),
                               # The chckbox variable is a numeric representation
                               # of a logical value, but 0 is for a "dead" record
                               # and 1 is for a "live" record, so let's actually
                               # make that easy on ourselves
                               Live = dplyr::case_when(chckbox %in% c("0") ~ "Live",
                                                       # chckbox %in% c("1") ~ "Dead",
                                                       .default = NA),
                               # Add a variable for shrubs and succulents so we
                               # can easily calculate indicators for just them
                               ShrubSucculent = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                       pattern = "shrub|succulent",
                                                                       ignore.case = TRUE) ~ "ShrubSucculent",
                                                                 .default = NA),
                               # For the any hit litter cover
                               Litter = dplyr::case_when(code %in% litter_codes[["HerbLitter"]] ~ "HerbLitter",
                                                         code %in% litter_codes[["WoodyLitter"]] ~ "WoodyLitter",
                                                         .default = NA),
                               TotalLitter = dplyr::case_when(code %in% unlist(litter_codes) ~ "TotalLitter",
                                                              .default = NA),
                               # Make separate photosynthesis columns because at
                               # least one species is classified as both
                               C3 = dplyr::case_when(grepl(x = photosynthesis,
                                                           pattern = "C3") ~ "C3",
                                                     .default = NA),
                               C4 = dplyr::case_when(grepl(x = photosynthesis,
                                                           pattern = "C4") ~ "C4",
                                                     .default = NA),
                               # For all the grass-specific indicators
                               Grass = dplyr::case_when(Family %in% c("Poaceae") ~ "Grass",
                                                        .default = NA),
                               # This is to turn the SG_Group codes into values
                               # that match the expected indicator names
                               SG_Group = dplyr::case_when(grepl(x = SG_Group,
                                                                 pattern = "Short") ~ "ShortPerenGrass",
                                                           grepl(x = SG_Group,
                                                                 pattern = "Tall") ~ "TallPerenGrass",
                                                           .default = SG_Group),
                               # For combined forb and graminoid cover
                               ForbGraminoid = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                      pattern = "(^((graminoid)|(grass))$)|forb",
                                                                      ignore.case = TRUE) ~ "ForbGraminoid",
                                                                .default = NA),
                               # For combined forb and grass cover
                               ForbGrass = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                  pattern = "forb",
                                                                  ignore.case = TRUE) | Family %in% "Poaceae" ~ "ForbGrass",
                                                            .default = NA),
                               # For biocrust cover
                               Biocrust = dplyr::case_when(code %in% biocrust_identifiers ~ "Biocrust",
                                                           .default = NA),
                               # For pinyon-juniper cover
                               PJ = dplyr::case_when(code %in% pj_identifiers ~ "PJ",
                                                     .default = NA),
                               # For conifer cover
                               Conifer = dplyr::case_when(Family %in% conifer_identifiers ~ "Conifer",
                                                          .default = NA),
                               # This is for basal cover by plants
                               Plant = dplyr::case_when(!is.na(GrowthHabit) ~ "Plant",
                                                        .default = NA),
                               # This is just to make the Invasive values match
                               # the desired indicator names
                               Invasive = stringr::str_to_title(string = invasive),
                               # This is for the native and non-native cover
                               # It assumes that everything flagged as EXOTIC or
                               # ABSENT should be considered NonNative and that
                               # everything else is Native
                               Native = dplyr::case_when(!(exotic %in% c("EXOTIC", "ABSENT")) ~ "Native",
                                                         .default = "NonNative"),
                               # For noxious cover. This assumes that anything
                               # flagged as YES is noxious and nothing else is
                               Noxious = dplyr::case_when(Noxious %in% c("YES") ~ "Noxious",
                                                          .default = NA),
                               # For rock cover
                               Rock = dplyr::case_when(code %in% rock_codes ~ "Rock",
                                                       .default = NA),
                               # This is for values in the code variable that we
                               # want to calculate cover for. This is a distinct
                               # variable so we can do that without calculating
                               # cover for *EVERY* value in the code variable.
                               SpecialConsiderationCode = dplyr::case_when(code %in% special_consideration_codes["Duff"] ~ "Duff",
                                                                           code %in% special_consideration_codes["Lichen"] ~ "Lichen",
                                                                           code %in% special_consideration_codes["Moss"] ~ "Moss",
                                                                           code %in% special_consideration_codes["EmbLitter"] ~ "EmbLitter",
                                                                           code %in% special_consideration_codes["Water"] ~ "Water",
                                                                           code %in% special_consideration_codes["Cyanobacteria"] ~ "Cyanobacteria",
                                                                           code %in% special_consideration_codes["VagrLichen"] ~ "VagrLichen",
                                                                           .default = NA),
                               between_plant = dplyr::case_when(code %in% between_plant_codes[["Woodylitter"]] ~ "WoodyLitter",
                                                                code %in% between_plant_codes[["HerbLitter"]] ~ "HerbLitter",
                                                                code %in% between_plant_codes[["NonVegLitter"]] ~ "NonVegLitter",
                                                                code %in% between_plant_codes[["EmbLitter"]] ~ "EmbLitter",
                                                                code %in% between_plant_codes[["DepSoil"]] ~ "DepSoil",
                                                                code %in% between_plant_codes[["Duff"]] ~ "Duff",
                                                                code %in% between_plant_codes[["Lichen"]] ~ "Lichen",
                                                                code %in% between_plant_codes[["Moss"]] ~ "Moss",
                                                                code %in% between_plant_codes[["Cyanobacteria"]] ~ "Cyanobacteria",
                                                                code %in% between_plant_codes[["Water"]] ~ "Water",
                                                                code %in% between_plant_codes[["Rock"]] ~ "Rock",
                                                                code %in% between_plant_codes[["VagrLichen"]] ~ "VagrLichen",
                                                                code %in% between_plant_codes[["BareSoil"]] ~ "BareSoil",
                                                                .default = NA),
                               # Special indicators for remote sensing use
                               AdditionalRemoteSensing = dplyr::case_when(code %in% c("DS") ~ "DS",
                                                                          .default = NA)
  )

  #### Calculations ############################################################
  ##### Total foliar cover #####################################################
  ####### Total foliar cover #####################################################
  if (verbose) {
    message("Calculating total foliar cover.")
  }
  total_foliar <- pct_cover_total_foliar(lpi_tall = lpi_species,
                                         tall = TRUE,
                                         by_line = FALSE)

  ####### All other cover #####################################################
  variable_groups <- list("first" = fh_variable_groupings,
                          "any" = ah_variable_groupings,
                          "basal" = basal_variable_groupings)

  # This is going to look gnarly, but automates stuff so we don't have to do the
  # capitalization corrections by hand
  unique_grouping_vars <- unique(c(unlist(fh_variable_groupings),
                                   unlist(ah_variable_groupings),
                                   unlist(basal_variable_groupings)))
  capitalization_lookup_list <- lapply(X = unique_grouping_vars,
                                       data = lpi_species,
                                       FUN = function(X, data){
                                         # message(paste(X,
                                         #               collapse = ", "))
                                         current_values <- unique(data[[X]])
                                         current_values <- current_values[!is.na(current_values)]
                                         if (length(current_values) > 0) {
                                           setNames(object = current_values,
                                                    nm = paste0("^",
                                                                toupper(current_values),
                                                                "$"))
                                         } else {
                                           NULL
                                         }
                                       })
  names(capitalization_lookup_list) <- unique_grouping_vars

  # This calculates the indicators.
  # The first level is iterating over the list variable_groups, working through
  # the hit types and the second level is working through all the groupings
  # within the hit type.
  cover_indicators_list <- lapply(X = names(variable_groups),
                                  variable_groups = variable_groups,
                                  data = lpi_species,
                                  capitalization_lookup_list = capitalization_lookup_list,
                                  verbose = verbose,
                                  FUN = function(X, variable_groups, data, capitalization_lookup_list, verbose){
                                    current_hit <- X
                                    message(paste("Calculating", current_hit, "hit indicators."))

                                    current_variable_groupings <- variable_groups[[current_hit]]
                                    # For the current hit type ("first", "any",
                                    # "basal"), calculate indicators for each
                                    # required variable grouping
                                    current_results_list <- lapply(X = seq(length(current_variable_groupings)),
                                                                   data = data,
                                                                   hit = current_hit,
                                                                   current_variable_groupings = current_variable_groupings,
                                                                   capitalization_lookup_list = capitalization_lookup_list,
                                                                   verbose = verbose,
                                                                   FUN = function(X, data, hit, current_variable_groupings, capitalization_lookup_list, verbose){
                                                                     current_grouping_vars <- current_variable_groupings[[X]]
                                                                     if (verbose) {
                                                                       message(paste("Calculating", hit, "hit indicators grouped by the variable(s):",
                                                                                     paste(current_grouping_vars,
                                                                                           collapse = ", "),
                                                                                     paste0("(Grouping ", X, " of ", length(current_variable_groupings), ")")))
                                                                     }
                                                                     # This is a little messy because pct_cover()
                                                                     # wants bare variable names.
                                                                     # There may be a better way to do this, but
                                                                     # for now this builds the function call as a
                                                                     # string and then executes that
                                                                     base_function_call_string <- paste0("pct_cover(lpi_tall = data,",
                                                                                                         "tall = TRUE,",
                                                                                                         "by_line = FALSE,",
                                                                                                         "hit = '", hit, "'")
                                                                     if (!is.null(current_grouping_vars)) {
                                                                       base_function_call_string <- paste0(base_function_call_string,
                                                                                                           ",")
                                                                     }
                                                                     function_call_string <- paste0(base_function_call_string,
                                                                                                    paste(current_grouping_vars,
                                                                                                          collapse = ","),
                                                                                                    ")")
                                                                     current_results_raw <- eval(expr = parse(text = function_call_string))

                                                                     # Sometimes there are no data that had non-NA
                                                                     # values in the variables of interest, so
                                                                     # we have to be prepared for that.
                                                                     if (nrow(current_results_raw) < 1) {
                                                                       if (verbose) {
                                                                         message("No qualifying data for the requested indicator(s). Returning NULL.")
                                                                       }
                                                                       return(NULL)
                                                                     }

                                                                     if (verbose) {
                                                                       message("Adjusting indicator names.")
                                                                     }

                                                                     # Now we rename the indicators.
                                                                     # We'll split them into their component parts
                                                                     # and then use the appropriate lookup vector
                                                                     # for each part to correct the capitalization.
                                                                     # There are more efficient ways to do this,
                                                                     # but this is extensible, standardized, and
                                                                     # basically hands-off for us when we update
                                                                     # indicators.
                                                                     current_results <- tidyr::separate_wider_delim(data = current_results_raw,
                                                                                                                    cols = indicator,
                                                                                                                    # Of course this doesn't use
                                                                                                                    # actual regex despite that
                                                                                                                    # being the tidyverse standard
                                                                                                                    delim = ".",
                                                                                                                    names = current_grouping_vars)


                                                                     # A for loop might actually be fastest (and
                                                                     # is certainly easiest), so that's the
                                                                     # solution for now.
                                                                     # I attempted to use mutate() with {{}} and
                                                                     # := but it wasn't evaluating the
                                                                     # str_replace_all() correctly because I couldn't
                                                                     # convince it to retrieve the relevant vector
                                                                     # with {{}} or dplyr::vars() for use as the
                                                                     # string argument.
                                                                     for (current_variable in current_grouping_vars) {
                                                                       current_results[[current_variable]] <- stringr::str_replace_all(string = current_results[[current_variable]],
                                                                                                                                       pattern = capitalization_lookup_list[[current_variable]])
                                                                     }

                                                                     # Having now made the variables with the
                                                                     # corrected components, we can recombine them
                                                                     current_results <- tidyr::unite(data = current_results,
                                                                                                     col = indicator,
                                                                                                     dplyr::all_of(current_grouping_vars),
                                                                                                     sep = "")

                                                                     # And add the hit prefix and "Cover" to the
                                                                     # indicator names
                                                                     current_prefix <- switch(EXPR = hit,
                                                                                              "first" = "FH_",
                                                                                              "any" = "AH_",
                                                                                              "basal" = "AH_Basal")
                                                                     current_results <- dplyr::mutate(.data = current_results,
                                                                                                      indicator = paste0(current_prefix,
                                                                                                                         indicator,
                                                                                                                         "Cover")) |>
                                                                       # And correct for the special case indicators
                                                                       dplyr::mutate(.data = _,
                                                                                     indicator = stringr::str_replace_all(string = indicator,
                                                                                                                          pattern = nonstandard_indicator_lookup))
                                                                     # We'll keep only the bare minimum here.
                                                                     dplyr::select(.data = current_results,
                                                                                   PrimaryKey,
                                                                                   indicator,
                                                                                   percent) |>
                                                                       # Get only the indicators we want to actually keep. Doing this saves us
                                                                       # from wasting memory storing unnecessary indicators even temporarily
                                                                       # and spares us the horror of storing them even less efficiently in
                                                                       # a wide format after this loop.
                                                                       dplyr::filter(.data = _,
                                                                                     indicator %in% expected_indicator_names)
                                                                   })

                                    # Bind all those results together
                                    dplyr::bind_rows(current_results_list)
                                  })

  # It's possible to accidentally calculate the same indicator more than once,
  # e.g. in Alaska where you might find "Moss" in the variable GrowthHabitSub
  # and so get a FH_MossCover when calculating both from GrowthHabitSub *AND*
  # SpecialConsiderationCode
  cover_indicators <- dplyr::bind_rows(cover_indicators_list) |>
    dplyr::distinct()

  #### Combine all LPI based cover indicators ##################################
  if (verbose) {
    message("Combining all cover indicators and converting to a wide format.")
  }
  lpi_indicators <- dplyr::bind_rows(cover_indicators,
                                     total_foliar) |>
    # Remove duplicates (which I guess is possible)
    dplyr::distinct(.data = _) |>
    # Spread to a wide format.
    tidyr::pivot_wider(data = _,
                       names_from = indicator,
                       values_from = percent,
                       values_fill = 0)

  if (any(isFALSE(is.na(unique(lpi_species$ShrubShape))))) {
    if (verbose) {
      message("Calculating sagebrush shape indicators and joining to output.")
    }
    sagebrush_shape_calc <- sagebrush_shape(lpi_tall = lpi_species,
                                            live = TRUE)
    lpi_indicators <- dplyr::left_join(x = lpi_indicators,
                                       y = sagebrush_shape_calc,
                                       by = "PrimaryKey")
  } else {
    if (verbose) {
      message("No qualifying data were found in ShrubShape. Skipping sagebrush shape indicators.")
    }
  }

  # Keep only the indicators we want
  chunked_indicators_list[[current_chunk]] <- dplyr::select(lpi_indicators,
                                                            PrimaryKey,
                                                            dplyr::any_of(expected_indicator_names))

  #### Final munging ###########################################################
  output <- dplyr::bind_rows(chunked_indicators_list)

  # There shouldn't be any NA values in numeric variables because those should
  # be 0s. But we're going to leave NAs in the character indicators.
  output <- dplyr::mutate(.data = output,
                          dplyr::across(.cols = dplyr::where(fn = is.numeric),
                                        .fns = ~ tidyr::replace_na(data = .x,
                                                                   replace = 0)))

  # Add in variables for indicators we want but which had no qualifying data and
  # therefore should have a value of 0 for all plots.
  # setdiff() is rad and I wish I'd known about it years ago.
  # We'll make sure to set ONLY the numeric indicators' NAs to 0.
  # The character indicators get NAs.
  output_missing_numeric_indicators <- setdiff(x = expected_indicator_names,
                                               y = c(names(output),
                                                     character_value_indicators))
  output[output_missing_numeric_indicators] <- 0
  output_missing_character_indicators <- setdiff(x = character_value_indicators,
                                                 y = names(output))
  output[output_missing_character_indicators] <- NA

  if (length(c(output_missing_numeric_indicators, output_missing_character_indicators)) > 0) {
    warning(paste("The following indicators had no qualifying data and have been populated with 0 or NA as appropriate:",
                  paste(c(output_missing_numeric_indicators, output_missing_character_indicators),
                        collapse = ", ")))
  }

  # This will reorder the variables to be as expected!
  output <- dplyr::select(.data = output,
                          dplyr::all_of(c("PrimaryKey",
                                          expected_indicator_names)))

  output
}


#' @export gap_calc
#' @rdname aim_gdb
# Calculate the Gap indicators for AIM
gap_calc <- function(header,
                     gap_tall,
                     verbose = FALSE) {
  if (!"data.frame" %in% class(header)) {
    stop("header must be a data frame.")
  }
  if (!"PrimaryKey" %in% names(header)) {
    stop("The variable PrimaryKey must appear in the header data frame.")
  }

  if(verbose) {
    print("Reading gap data")
  }

  data <- readRDS(gap_tall)

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
                  GapCover_25_50 = "25-50",
                  GapCover_51_100 = "51-100",
                  GapCover_101_200 = "101-200",
                  GapCover_200_plus = "201-Inf"
    )
  # Return
  return(gap_calc)
}


#' @export height_calc
#' @rdname aim_gdb
# Calculate the Height indicators for AIM
height_calc <- function(header, height_tall,
                        species_file = species_file,
                        source) {
  print("Beginning Height indicator calculation")

  # gather tall height
  height <- readRDS(height_tall) %>%

    # subset by PK and add the SpeciesState from the header
    dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .)

  # Join to species list
  height_species <- species_join(
    data = height,
    data_code = "Species",
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else(
      source == "TerrADat",
      TRUE,
      FALSE
    )
  )

  # Correct the Non-Woody to NonWoody
  height_species$GrowthHabit[grepl(
    pattern = "Non-woody|Nonwoody|Non-Woody",
    x = height_species$GrowthHabit
  )] <- "NonWoody"

  # For any unresolved height errors, change height to "0" so
  # they are omitted from the calculations, but keep heights with no species assigned
  height_no_species <- height_species %>% dplyr::filter(is.na(Species) & !is.na(Height))
  height_bad_species <- height_species %>% subset(GrowthHabit_measured == GrowthHabit)

  height_species <- dplyr::bind_rows(height_no_species, height_bad_species)

  # Add a forb and grass category
  height_species$pgpf[height_species$Duration == "Perennial" &
                        height_species$GrowthHabitSub %in%
                        c("Forb/herb", "Forb", "Graminoid", "Grass", "Forb/Herb", "Sedge")] <- "PerenForbGraminoid"

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
    ) %>% subset(indicator %in% c("Forb", "Graminoid", "Grass", "Forb/herb", "Forb/Herb", "Sedge")),

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
    ) %>% subset(indicator == "PerenForbGraminoid"),

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
  if (source %in% c("TerrADat", "AIM", "DIMA")) {
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
                      "Graminoid" = "Graminoid",
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
  print("Beginning Species Inventory indicator calculation")

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
    overwrite_generic_species = dplyr::if_else(
      source == "TerrADat",
      TRUE,
      FALSE
    )
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

  # Join with spp_inventory, drop columns, and return
  spp_inventory <- dplyr::full_join(spp_inventory_wide, spp_list) %>%
    dplyr::select_if(!names(.) %in% c("DBKey"))

  return(spp_inventory)
}


#' @export soil_stability_calc
#' @rdname aim_gdb
# Calculate soil stability values
soil_stability_calc <- function(header, soil_stability_tall) {
  print("Beginning Soil Stability indicator calculation")
  # Gather and subset
  soil_stability_tall <- readRDS(soil_stability_tall) %>%
    subset(!is.na(Rating)) %>%
    # subset to relevant PrimaryKeys
    subset(PrimaryKey %in% header$PrimaryKey)

  # Calculate indicators


  soil_stability_calcs <- soil_stability(soil_stability_tall,
                                         cover = TRUE
  )
  if(all(c('all', 'uncovered', 'covered') %in% colnames(soil_stability_calcs))) {
    soil_stability_calcs <- soil_stability_calcs  %>%
      # Rename fields
      dplyr::rename(
        SoilStability_All = all,
        SoilStability_Protected = covered,
        SoilStability_Unprotected = uncovered
      )
  }


  # Return
  return(soil_stability_calcs)
}

#' @export build_terradat_indicators
#' @rdname aim_gdb
# Build indicators feature class
build_terradat_indicators <- function(header, source, dsn,
                                      species_file,
                                      lpi_tall,
                                      gap_tall,
                                      height_tall,
                                      spp_inventory_tall,
                                      soil_stability_tall, ...) {
  # Test that source is  "TerrADat"
  if (!source %in% c("TerrADat", "AIM")) {
    stop("Invalid indicator source specified")
  }

  lpi_required_layers <- c("tblLPIHeader",
                           "tblLPIDetail",
                           "tblPlots",
                           "tblNationalPlants",
                           "tblStateSpecies")
  gap_required_layers <- c("tblGapHeader",
                           "tblGapDetail")
  height_required_layers <- c("tblLPIHeader",
                              "tblLPIDetail")
  soil_stability_required_layers <- c("tblSoilStabHeader",
                                      "tblSoilStabDetail")
  species_inventory_required_layers <- c("tblSpecRichHeader",
                                         "tblSpecRichDetail")
  rangeland_health_required_layers <- c("tblQualHeader",
                                        "tblQualDetail")

  required_layers <- unique(c(lpi_required_layers,
                              gap_required_layers,
                              height_required_layers,
                              soil_stability_required_layers,
                              species_inventory_required_layers,
                              rangeland_health_required_layers))

  available_layers <- sf::st_layers(dsn = dsn)[["names"]]

  missing_layers <- setdiff(x = required_layers,
                            y = available_layers)

  if (length(missing_layers) > 0) {
    warning(paste0("The following tables are missing from the specified geodatabase: ",
                   paste(missing_layers,
                         collapse = ", "),
                   ". The indicators which depend on those will not be calculated."))
  }

  # # Join all indicator calculations together
  # Calculate all indicators and send them to a list, to later be reduced
  # If a method is not provided (ie the path to the table provided as NULL)
  # then we need a NULL variable to go into the list
  if(!is.null(lpi_tall)){
    # LPI
    lpi <- lpi_calc(lpi_tall = lpi_tall,
                    header = header,
                    source = source,
                    species_file = species_file,
                    dsn = dsn)
  } else {
    warning("Unable to calculate LPI indicators due to incomplete data.")
  }

  ##### Gap --------------------------------------------------------------------
  if (all(gap_required_layers %in% available_layers)) {
    indicators_list[["gap"]] <- gap_calc(dsn = dsn)
  } else {
    warning("Unable to calculate gap indicators due to incomplete data.")
  }

  # Height
  if(!is.null(height_tall)){
    height <- height_calc(height_tall = height_tall,
                          header = header,
                          source = source,
                          species_file = species_file)
  } else {
    warning("Unable to calculate height indicators due to incomplete data.")
  }

  # Species Inventory
  if(!is.null(spp_inventory_tall)){
    spinv <- spp_inventory_calc(spp_inventory_tall = spp_inventory_tall,
                                header = header,
                                species_file = species_file,
                                source = source)
  } else {
    warning("Unable to calculate soil stability indicators due to incomplete data.")
  }


  ##### Species Inventory ------------------------------------------------------
  if (all(species_inventory_required_layers %in% available_layers)) {
    indicators_list[["species"]] <- spp_inventory_calc(dsn = dsn)
  } else {
    warning("Unable to calculate species inventory indicators due to incomplete data.")
    species_indicators <- NULL
  }

  # Rangeland health
  if(all(c("tblQualHeader", "tblQualDetail") %in% sf::st_layers(dsn)$name)){
    print("Gathering rangeland health indicators from dsn")
    rh <- gather_rangeland_health(dsn, source = source) %>%
      # Remove RecKey field, which is not applicable at the indicator level
      dplyr::select_if(!names(.) %in% c("RecKey"))
  } else {
    print("Rangeland health data not found")
    rh <- NULL
  }

  output <- purrr::reduce(.x = indicators_list,
                          .f = dplyr::left_join)

  # if (class(lpi_tall) == "character") {
  #   if (tools:file_ext(lpi_tall) == "rds") {
  #     lpi_tall_header <- readRDS(lpi_tall) |>
  #       dplyr::left_join(x = dplyr::select(.data = header,
  #                                          PrimaryKey, SpeciesState),
  #                        y = _,
  #                        by = "PrimaryKey") |>
  #       # This is here for now, but shouldn't be once we've got more auto QC in
  #       # place.
  #       suppressWarnings()
  #   } else {
  #     stop("When lpi_tall is a character string it must be the path to a .rds file containing tall LPI data.")
  #   }
  # } else if (class(lpi_tall) == "data.frame") {
  #   lpi_tall_header <- dplyr::left_join(x = dplyr::select(.data = header,
  #                                                         PrimaryKey, SpeciesState),
  #                                       y = lpi_tall,
  #                                       by = "PrimaryKey") |>
  #     # This is here for now, but shouldn't be once we've got more auto QC in
  #     # place.
  #     suppressWarnings()
  # }

  # # Read header in
  # header <- readRDS(header) %>%
  #   # Filter using the filtering expression specified by user
  #   dplyr::filter(!!!filter_exprs) %>%
  #   dplyr::filter(source %in% c("AIM", "TerrADat"))
  #
  # # Check header for data
  # if(nrow(header) == 0){
  #   stop("No rows in header file")
  # }
  #
  # # Join all indicator calculations together
  # Calculate all indicators and send them to a list, to later be reduced
  # If a method is not provided (ie the path to the table provided as NULL)
  # then we need a NULL variable to go into the list
  # if(!is.null(lpi_tall)){
  #   # LPI
  #   lpi <- lpi_calc(lpi_tall = lpi_tall,
  #                   header = header,
  #                   source = source,
  #                   species_file = species_file,
  #                   dsn = dsn,
  #                   generic_species_file = generic_species_file)
  # } else {
  #   print("LPI data not provided")
  #   lpi <- NULL
  # }
  #
  # # Gap
  # if(!is.null(gap_tall)){
  #   gap <- gap_calc(gap_tall = gap_tall,
  #                   header = header)
  # } else {
  #   print("Gap data not provided")
  #   gap <- NULL
  # }
  #
  # # Height
  # if(!is.null(height_tall)){
  #   height <- height_calc(height_tall = height_tall,
  #                         header = header,
  #                         source = source,
  #                         species_file = species_file,
  #                         generic_species_file = generic_species_file)
  # } else {
  #   print("Height data not provided")
  #   height <- NULL
  # }
  #
  # # Species Inventory
  # if(!is.null(spp_inventory_tall)){
  #   spinv <- spp_inventory_calc(spp_inventory_tall = spp_inventory_tall,
  #                               header = header,
  #                               species_file = species_file,
  #                               source = source,
  #                               generic_species_file = generic_species_file)
  # } else {
  #   print("Species inventory data not provided")
  #   spinv <- NULL
  # }
  #
  # # Soil Stability
  # if(!is.null(soil_stability_tall)){
  #   sstab <- soil_stability_calc(soil_stability_tall = soil_stability_tall,
  #                                header = header)
  # } else {
  #   print("Soil stability data not provided")
  #   sstab <- NULL
  # }
  #
  # # Rangeland health
  # if(!is.null(dsn)){
  #   if(all(c("tblQualHeader", "tblQualDetail") %in% sf::st_layers(dsn)$name)){
  #     print("Gathering rangeland health indicators from dsn")
  #     rh <- gather_rangeland_health(dsn, source = source) %>%
  #       # Remove RecKey field, which is not applicable at the indicator level
  #       dplyr::select_if(!names(.) %in% c("RecKey"))
  #   } else {
  #     print("Rangeland health data not found")
  #     rh <- NULL
  #   }
  # } else {
  #   rh <- NULL
  # }

  # Combine the indicators
  l_indicators <- list(header, lpi, gap, height, spinv, sstab, rh)
  # Drop unused methods
  l_indicators_dropnull <- l_indicators[!sapply(l_indicators, is.null)]

  # reduce the list to a data frame
  all_indicators <- Reduce(dplyr::left_join, l_indicators_dropnull)

  return(all_indicators)
}

# Build LMF Indicators
#' @export build_lmf_indicators
#' @rdname aim_gdb

build_lmf_indicators <- function(header, source, dsn,
                                 species_file,
                                 lpi_tall,
                                 gap_tall,
                                 height_tall,
                                 spp_inventory_tall,
                                 soil_stability_tall, ...) {

  # Test that source is  "LMF"
  try(
    !source %in% c("LMF", "NRI"),
    stop("Invalid indicator source specified")
  )


  # Assign filter expressions
  filter_exprs <- rlang::quos(...)

  # Read header in
  header <- readRDS(header) %>%
    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%
    dplyr::filter(source %in% c("LMF", "NRI"))

  # Check header for data
  if(nrow(header) == 0){
    stop("No rows in header file")
  }

  # Join all indicator calculations together
  indicators <- list(
    header,
    # LPI
    lpi_calc(
      lpi_tall = lpi_tall,
      header = header,
      source = source,
      species_file = species_file,
      dsn = dsn
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
    gather_rangeland_health(dsn, source = source)
  )

  all_indicators <- Reduce(dplyr::left_join, indicators)

  return(all_indicators)
}

# Build LMF Indicators
#' @export build_indicators
#' @rdname aim_gdb
# Build wrapper
build_indicators <- function(header, source, dsn, lpi_tall,
                             species_file,
                             gap_tall,
                             height_tall,
                             spp_inventory_tall,
                             soil_stability_tall, ...) {
  all_indicators <- switch(source,
                           "TerrADat" = build_terradat_indicators(
                             header = header,
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
                             header = header,
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
                             header = header,
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
                           "NRI" = build_lmf_indicators(
                             header = header,
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

  # If target feature class is a gdb compare indicator field names with the names for a the target feature class
  if(substr(dsn, nchar(dsn)-2, nchar(dsn)) == "gdb"){
    print("Reading column names from dsn. Missing columns will be added to output.")
    feature_class_field_names <- sf::st_read(dsn,
                                             layer = dplyr::if_else(source %in% c("AIM", "TerrADat"), "TerrADat", source)
    )

    feature_class_field_names <- feature_class_field_names[
      ,
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

      # get the field names where there is not corollary in calculated
      subset(is.na(calculated), select = "name") %>%
      dplyr::mutate(value = NA) %>%
      # make into a data frame
      tidyr::spread(key = name, value = value) %>%
      dplyr::select(-Shape, -GlobalID)

    # Add a row for each PrimaryKey inall_indicators
    missing_names[nrow(all_indicators), ] <- NA
    # For some indicators, the null value is 0 (to indicate the method was completed,
    # but no data in that group were collected)
    # Skip this if the method was not provided
    if(!is.null(lpi_tall)){
      missing_names[, grepl(names(missing_names), pattern = "^FH|^AH")] <- 0
    }

    if(!is.null(spp_inventory_tall)){
      missing_names[, grepl(names(missing_names), pattern = "^Num")] <- 0
    }

    # Merge back to indicator data to create a feature class for export
    final_feature_class <- dplyr::bind_cols(all_indicators, missing_names)
    return(final_feature_class)

  } else {
    return(all_indicators)
  }
}


#' Remove duplicate helper function
#' @description Helper function used to remove duplicates
#' @noRd

tdact_remove_duplicates <- function(indata) {

  cols_to_exclude_from_duplicate_check <- c("DBKey", "DateLoadedInDb")
  data_check <- indata[,!(colnames(indata) %in% cols_to_exclude_from_duplicate_check)]

  # For runspeed, drop columns that are all identical
  vec_varied_cols <- vapply(data_check, function(x) length(unique(x)) > 1, logical(1L))
  vec_varied_cols["PrimaryKey"] <- TRUE # Needed if only one primary key is in the input data
  data_varied_cols_only <- data_check[,vec_varied_cols]

  # get just duplicated rows
  data_duplicated_columns <-
    data_varied_cols_only[duplicated(data_varied_cols_only) | duplicated(data_varied_cols_only, fromLast = T),]

  # give a warning if duplicated rows are found
  if(nrow(data_duplicated_columns) > 0){
    message("Duplicate rows found in input data (columns not printed have no variation in all input data)")

    # Print the data, including DBKey and DateLoaded, but not columsn with only one value in the whole table
    print(indata %>% dplyr::filter(PrimaryKey %in% data_duplicated_columns$PrimaryKey) %>%
            dplyr::select(dplyr::any_of(c(colnames(data_duplicated_columns), cols_to_exclude_from_duplicate_check))) %>%
            dplyr::arrange(PrimaryKey))

    # drop duplicates from output data
    n_duplicates <- sum(duplicated(data_varied_cols_only))
    warning(paste(n_duplicates, "duplicates removed"))
    outdata <- indata[!duplicated(data_varied_cols_only),]
  } else {
    outdata <- indata
  }

  return(outdata)
}

#' Remove no data row helper function
#' @description Hidden helper function used to remove rows with no data
#' @noRd

tdact_remove_empty <- function(indata, datatype){

  # Create vector to select which fields are essential
  datacols <- switch(datatype,
                     "gap" = c("GapStart", "GapEnd", "Gap"),
                     "height" = c("Height"), # Species field is very important but not used by all early projects
                     "hzflux" = c("sedimentWeight", "sedimentGperDayByInlet", "sedimentGperDay"),
                     "lpi" = c("layer", "code"),
                     "soilhz" = c("HorizonDepthUpper", "HorizonDepthLower"),
                     "soilstab" = c("Veg", "Rating"),
                     "specinv" = c("Species"),
                     "geosp" = c("Species"),
                     "rh" = c("RH_WaterFlowPatterns", "RH_PedestalsTerracettes", "RH_BareGround",
                              "RH_Gullies", "RH_WindScouredAreas", "RH_LitterMovement",
                              "RH_SoilSurfResisErosion", "RH_SoilSurfLossDeg",
                              "RH_PlantCommunityComp", "RH_Compaction", "RH_FuncSructGroup",
                              "RH_DeadDyingPlantParts", "RH_LitterAmount", "RH_AnnualProd",
                              "RH_InvasivePlants", "RH_ReprodCapabilityPeren",
                              "RH_SoilSiteStability", "RH_BioticIntegrity", "RH_HydrologicFunction"),
                     "unknown"
                     ## Not necessary for geoIndicators or header
  )

  if(length(datacols) == 1){ # if datacols is a vector of length >1 (it usually is) this line is needed
    if(datacols == "unknown"){
      stop("datacols value not recognized")
    }
  }

  message(paste("Checking for rows with no data in all of these columns:", paste(datacols, collapse = ", ")))

  # Select only data columns and count how many are NA
  data_datacols_only <- data.frame(indata[,datacols]) %>% dplyr::mutate(nNA = rowSums(is.na(.)))

  # Rows where all essential values are NA must be eliminated
  vec_hasdata <- data_datacols_only$nNA != length(datacols)

  if(sum(vec_hasdata) < nrow(indata)){
    n_missing <- sum(!vec_hasdata)
    warning(paste(n_missing, "row(s) with no essential data removed"))
  }

  outdata <- indata[vec_hasdata,]

  return(outdata)
}
