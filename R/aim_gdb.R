#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @param header
#' @param source
#' @param layer
#' @param ... Query in grepl format that subsets plots.
#' @return A \code{tbl} of indicators of either tall or wide format.


# Build the header portion of the terradat table
#' @export header_build_terradat
#' @rdname aim_gdb
header_build_terradat <- function(dsn, ... ){
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  #Error check
  testthat::test_that("dsn ends with .gdb extension", {
    testthat::expect_equal(grepl(x = dsn, pattern = ".gdb$"), TRUE)
  })


  # tblPlots provides the link between species tables
  #(LPI, Height, Species Richness) and tblStateSpecies
  plots <- sf::read_sf(dsn = dsn, layer = "tblPlots") %>% as.data.frame() %>%
    #Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%

    # Select the field names we need in the final feature class
    dplyr::select(PrimaryKey, SpeciesState, SiteKey, PlotID, PlotKey, DBKey,
                  EcologicalSiteId = EcolSite, Latitude, Longitude, State,
                  County, DateEstablished = EstablishDate, DateLoadedInDb)

  # tblSites provides some project metatdata
  sites <- sf::st_read(dsn = dsn, layer = "tblSites")

  # Join the sites and plots to a final header
  header <-  sites %>% dplyr::select(SiteID, SiteKey,
                                     ProjectName = SiteName) %>%

    dplyr::left_join(plots, by = "SiteKey") %>%

    #If there are any Sites with no PrimaryKeys, delete them
    subset(!is.na(PrimaryKey))

  #Return the header file
  return(header)

}

# Build the header portion of the LMF table
#' @export header_build_lmf
#' @rdname aim_gdb
header_build_lmf<- function(dsn, ... ){
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  #Error check
  testthat::test_that("dsn ends with .gdb extension", {
    testthat::expect_equal(grepl(x = dsn, pattern = ".gdb$"), TRUE)
  })


  # Get the LMF points
  point <- sf::read_sf(dsn = dsn, layer = "POINT") %>% as.data.frame() %>%
    #Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(PrimaryKey, SpeciesState,
                  COUNTY, STATE, DBKey)

    # County and State are referred to by number codes, let's use the name
  point <-  sf::st_read(dsn, layer = "COUNTYNM") %>%
    dplyr::select(COUNTY, COUNTYNM, STATE) %>%
    dplyr::left_join(point, ., by = c("COUNTY", "STATE")) %>%
    dplyr::distinct() %>%


    # Add state
    dplyr::left_join(sf::st_read(dsn, layer = "STATENM"), by = "STATE") %>%

    #pair down to needed fields
    dplyr::select(PrimaryKey = PrimaryKey.x,
                  SpeciesState,
                  DBKey = DBKey.x,
                  County = COUNTYNM,
                  State = STABBR) %>% dplyr::distinct()

  # Get the field coordinates
  point_coordinate <- sf::read_sf(dsn = dsn, layer = "POINTCOORDINATES") %>%
    as.data.frame() %>%

    dplyr::select(PrimaryKey,
                  Latitude = REPORT_LATITUDE,
                 Longitude = REPORT_LONGITUDE,
                 LocationType) %>%

    dplyr::left_join(point, ., by = "PrimaryKey")



  #Add elevation data
  point_elevation <- sf::read_sf(dsn = dsn, layer = "GPS") %>%
    dplyr::select(PrimaryKey,
                  DateVisited = CAPDATE,#The GSP capture date is the best approx
                  Elevation = ELEVATION) %>%
    dplyr::left_join(point_coordinate, ., by = "PrimaryKey") %>%

    # Convert elevation to meters
    dplyr::mutate(Elevation = Elevation*0.3048)

  #Add Ecological Site Id
  point_ESD <- sf::st_read(dsn, layer = "ESFSG") %>%
    dplyr::left_join(point_elevation, ., by = "PrimaryKey") %>%

    # If the ESD coverage !=all, figure what portion of the plot the dominant ESD
    # is ion the plot by taking the End_Mark-Start_Mark and dividng by the line length
    dplyr::mutate(ESD_coverage =
                    dplyr::if_else(condition = COVERAGE == "all",
                                   true = as.integer(300),
                                   false = (END_MARK - START_MARK)),
                  EcologicalSiteId =
                    paste(ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE, sep = "")) %>%

    # Add up the coverage on each plot and get the percent coverage
    dplyr::group_by(PrimaryKey, EcologicalSiteId) %>%
    dplyr::summarise(PercentCoveredByEcosite = 100*sum(ESD_coverage)/300) %>%

    #Arrange by ESD_coverage and find the dominant ecological site
    dplyr::ungroup() %>% dplyr::group_by(PrimaryKey) %>%
    dplyr::arrange(dplyr::desc(PercentCoveredByEcosite), .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

 # Join to point.elevation to build the final header
    dplyr::left_join(point_elevation, ., by = "PrimaryKey")


  #Return the point_ESD as the header file
  return(point_ESD)

}

# Header build wrapper function
header_build <- function (dsn, source, ...) {
  # Error check
  #Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"))
    stop("No valid source provided"))

  # Apply appropriate header function

  header <- switch (toupper(source),
                    "LMF" = header_build_lmf(dsn = dsn, ...),
                    "NRI" = header_build_lmf(dsn = dsn, ...),
                    "TERRADAT" = header_build_terradat(dsn = dsn, ...),
                    "AIM" = header_build_terradat(dsn = dsn, ...),
                    "DIMA" = header_build_terradat(dsn = dsn, ...))

  return(header)
}


#' @export lpi_calc
#' @rdname aim_gdb
#Calculate the LPI indicators
lpi_calc <- function(header, dsn, layer) {

  # Format the lpi data for calculations ----
  lpi.tall <- gather.lpi(dsn = dsn, source = layer)

    # Join the lpi data to the header PrimaryKeys and add the StateSpecies Key
  lpi_tall_header <-  dplyr::left_join(dplyr::select(header,
                                                     PrimaryKey,
                                                     DBKey,
                                                     SpeciesState),
                                       lpi.tall,
                                       by = c("PrimaryKey", "DBKey"))

  # Appply
  # Join to the state species list via the SpeciesState value
  lpi.species <- species.join(data = lpi_tall_header, species.file = dsn) %>%
    dplyr::distinct()

  # If sedges exist as a growth habit, combine with graminoid
  lpi.species <- lpi.species %>%
    dplyr::mutate(GrowthHabitSub = GrowthHabitSub %>%
                    stringr::str_replace( pattern = "Sedge",
                                          replacement = "Graminoid"))

  # Calculate Total Foliar Cover ----
  total.foliar <- pct.cover.total.foliar( lpi.tall = lpi.species,
                                          tall = TRUE)

  # Calculate between plant cover (includes bare soil) ----
  between.plant.cover <- pct.cover.between.plant(lpi.tall = lpi.species,
                                                 by.year = FALSE,
                                                 by.line = FALSE,
                                                 tall = TRUE)

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
    "CY" = "Cynobacteria",
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
    #Filter Litter Indicators
    dplyr::filter(grepl(pattern = "Litter", x = indicator)) %>%
    #Sum all indicator hits
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(indicator = "FH_TotalLitterCover",
                     percent = sum(percent)) %>%
    #Add back to the rest of the between plant cover indicators
    dplyr::bind_rows(between.plant.cover, .)


  # Species Group Cover ----
  # Set the replacement values for valid indicator names ----
   spp.cover.replace <-c(
     "NON-WOODY" = "ForbGrass",
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
    "TALL" = "Tall"

  )


  # Any hit cover ----
  ah.spp.group.cover <- rbind(
    # cover by Noxious, Duration, and GrowthHabitSub combination
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              Noxious, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              Noxious, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              Noxious
    ) %>% dplyr::mutate(indicator = paste(indicator, ".", sep = "")),
    # Cover by Noxious, Duration, GrowthHabit status
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              Noxious, Duration, GrowthHabit
    ),
    # Sage Grouse Groups
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "any",
              by.year = FALSE,
              by.line = FALSE,
              GRSG_Group
    )
  ) %>%

    # Remove incomplete indicator groups
    subset(!grepl(indicator, pattern = "NA"))

  #Fix to indicator names so they are valid for AIM.gdb
  ah.spp.group.cover <- ah.spp.group.cover %>%
    # Substitute "NonNox" for "NO
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("AH_", indicator, "Cover", sep = ""))

  # First hit cover ----
  fh.spp.group.cover <- rbind(
    # cover by Noxious, Duration, and GrowthHabitSub combination
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              Noxious, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              Noxious, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              Noxious
    ),
    # Cover by Noxious, Duration, GrowthHabit status
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              Noxious, Duration, GrowthHabit
    ),
    # Sage Grouse Groupings
    pct.cover(lpi.species,
              tall = TRUE,
              hit = "first",
              by.year = FALSE,
              by.line = FALSE,
              GRSG_Group
    )
  ) %>%

    # Remove incomplete indicator groups
    subset(!grepl(indicator, pattern = "NA"))


  fh.spp.group.cover <- fh.spp.group.cover %>%
    # Substitute for Field friendly names
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = ""))




  # Combine  all LPI based cover indicators----
  lpi.cover <- dplyr::bind_rows(ah.spp.group.cover,
                     fh.spp.group.cover,
                     total.foliar,
                     between.plant.cover) %>%
    #Spread to a wide format
    tidyr::spread(key = indicator, value = percent, fill = 0)


  #   SageBrush Shape, this is dependent on Shrub shape existing ----
  # Need to check this with sagebrush state data


  sagebrush.shape <- sagebrush_shape(lpi.tall = lpi.species,
                                     #NRI and LMF don't collect live v. dead
                                     live = ifelse(layer %in% c("LMF", "NRI"),
                                                   FALSE, TRUE))

  lpi_indicators <- dplyr::left_join(lpi.cover,
                                     sagebrush.shape,
                                     by = "PrimaryKey")


  #For TerrADat only, get the data visited from the first line in LPI
  if (layer == "TerrADat"){
    lpi_indicators <- lpi.species %>%
    dplyr::select(PrimaryKey, FormDate) %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(DateVisited = dplyr::first(FormDate,
                                                order_by = FormDate) %>%
                as.POSIXct()) %>%
    #Join to the lpi.cover data
    dplyr::left_join(lpi_indicators, ., by = "PrimaryKey")
  }

  #Return lpi.cover
  return(lpi_indicators)

}


#' @export gap_calc
#' @rdname aim_gdb
#Calculate the Gap indicators for AIM
gap_calc <- function(header, dsn, layer) {
  # tidy gap
  gap.tall <- gather.gap(dsn = dsn, source = layer) %>%
    #Subset to PrimaryKeys in the header
   subset(PrimaryKey %in% header$PrimaryKey)

  # Calculate indicators
  gap.calcs <- gap.cover(gap.tall = gap.tall,
                         by.line = FALSE,
                         tall = FALSE)$percent %>%
    dplyr::rowwise() %>%
     dplyr::select(PrimaryKey,
                  GapCover_25_50 = "[25,51)",
                  GapCover_51_100 = "[51,100)",
                  GapCover_101_200 = "[100,200)",
                  GapCover_200_plus = "[200,1e+05)"
    ) %>%

    #Calculate the summation indicator
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50,
                                         GapCover_51_100,
                                         GapCover_101_200,
                                         GapCover_200_plus)))


  #Return
  return(gap.calcs)
}


#' @export height_calc
#' @rdname aim_gdb
#Calculate the Height indicators for AIM
height_calc <- function(header, dsn, layer){
  # gather tall height
  height <- gather.height(dsn, source = layer) %>%
    #subset by PK and add the SpeciesState from the header
  dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .)

  # Join to species list
  height.species <- species.join(
    data = height,
    data.code = "Species", species.file = dsn
  ) %>%
    # Convert sedge to graminoid
    dplyr::mutate(GrowthHabitSub = stringr::str_replace(GrowthHabitSub,
                                               pattern = "Sedge",
                                               replacement = "Graminoid"))

  # For any unresolved height errors, change height to "0" so
  #they are omitted from the calculations
  height.species$Height[toupper(height.species$GrowthHabit_measured)
                        != toupper(height.species$GrowthHabit)] <- 0

  # Add a forb and grass category
  height.species$pgpf[height.species$Duration == "Perennial" &
                        height.species$GrowthHabitSub %in%
                        c("Forb/herb", "Forb", "Graminoid", "Grass", "Sedge")
                      ] <- "PerenGrassForb"

  # Height calculations----
  height.calc <- rbind(
    # Woody and Herbaceous heights
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      type
    ) %>% subset(indicator %in% c("woody", "herbaceous")),

    # Forb or Grass
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      GrowthHabitSub
    ) %>% subset(indicator %in% c("Forb", "Graminoid")),

    # Perennial Forb or Grass
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      Duration, GrowthHabitSub
    ) %>% subset(indicator %in% c("Perennial.Forb", "Perennial.Graminoid")),

    # Perennial Forb and Grass as a single category
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      pgpf
    ) %>% subset(indicator == "Hgt_PerenGrassForb_Avg"),

    # Perennial grass by Noxious/NonNoxious
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      Noxious, Duration, GrowthHabitSub
    ) %>% subset(indicator %in% c("No.Perennial.Graminoid",
                                  "Yes.Perennial.Graminoid")),


    # GRSG_group heights
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      GRSG_Group
    ) %>% subset(indicator != "NA"))

 #For TerrADat only
 if (layer == "TerrADat") {
   # Live sagebrush heights
   height.calc <- rbind(height.calc,
                        mean.height(
                          height.tall = height.species,
                          method = "mean",
                          omit.zero = TRUE, # remove zeros from average height calcs
                          by.line = FALSE,
                          tall = TRUE,
                          GRSG_Group, Chkbox
                        ) %>% subset(indicator == "Sagebrush.0")
   )
 }

  # Reformat for Indicator Field Name ----
  height.calc <- height.calc %>%
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(c(
                      "woody" = "Woody",
                      "herbaceous" = "Herbaceous",
                      "\\bYes.\\b" = "Nox",
                      "\\bNo.\\b" = "NonNox",
                      "Forb/herb" = "Forb",
                      "Graminoid" = "Grass",
                      "0" = "_Live",
                      "\\." = "",
                      " " = "",
                      "Perennial" = "Peren",
                      "Stature" = ""
                    )) %>%
                    paste("Hgt_", ., "_Avg", sep = ""))

  #Spread to wide format and return
  height.calc.wide <- height.calc %>% tidyr::spread(key = indicator,
                                                    value = mean.height,
                                                    fill = NA)

  return(height.calc.wide)
}


#' @export spp_inventory_calc
#' @rdname aim_gdb
#Calculate species inventory
spp_inventory_calc <- function (header, dsn, layer) {
  # tidy.species
  spp.inventory.tall <- gather.species.inventory(dsn = dsn, source = layer) %>%
    #Join to the header to get the relevant PrimaryKeys and SpeciesSate
    dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .,
                     by = "PrimaryKey")

   # Join to State Species List
  spp.inventory.species <- species.join(
    data = spp.inventory.tall,
    data.code = "Species",
    species.file = dsn
  )

  # Count the number of species present in each group
  spp.inventory <- rbind(
    # Noxious & Non-Noxious
    species.count(spp.inventory.species, Noxious) %>%
      dplyr::mutate(indicator = indicator %>% stringr::str_replace_all(c(
        "Yes" = "NoxPlant",
        "\\bNo\\b" = "NonNoxPlant"
      ))),

    # Preferred Forb
    species.count(spp.inventory.species, GRSG_Group) %>%
      # Subset to only Preferred Forb
      subset(indicator == "Preferred Forb") %>%
      dplyr::mutate(indicator = indicator %>%
                      stringr::str_replace_all(c(" " = "")))
  ) %>%
    # Format for appropriat indicator name
    dplyr::mutate(indicator = paste("NumSpp_", indicator, sep = ""))

  #Spread to wide
  spp.inventory.wide <- spp.inventory %>% tidyr::spread(key = indicator,
                                                        value = n,
                                                        fill = 0)

  #Get the list of species that fall into a category (e.g., Preferred Forb)
  spp.list <- spp.inventory.species %>%
    dplyr::group_by(PrimaryKey, GRSG_Group) %>%
    dplyr::summarize(list = toString(Species) %>%
                       stringr::str_replace_all(pattern = ",",
                                                replacement = ";")) %>%
    # Format field names
    subset(!is.na(GRSG_Group)) %>%
    dplyr::mutate(indicator = GRSG_Group %>%
                    stringr::str_replace_all(c(
                      "Perennial" = "Peren",
                      " " = "",
                      "Stature" = ""
                    )) %>%
                    paste("Spp_", ., sep = "")) %>%
    dplyr::select(-GRSG_Group) %>%
    #Output in wide format
    tidyr::spread(key = indicator, value = list, fill = NA)

  #Join with spp.inventory and return
  species.calc <- dplyr::full_join(spp.inventory.wide, spp.list)

  return(species.calc)
}


#' @export soil_stability_calc
#' @rdname aim_gdb
#Calculate soil stability values
soil_stability_calc <- function (header, dsn, layer) {
  # Gather and subset
  soil.stability.tall <- gather.soil.stability(dsn, source = layer) %>%
    subset(!is.na(Rating)) %>%
    #subset to relevant PrimaryKeys
    subset(PrimaryKey %in% header$PrimaryKey)

  # Calculate indicators
  soil.stability.calcs <- soil.stability(soil.stability.tall,
                                         cover = TRUE) %>%
    # Rename fields
    dplyr::rename(
      SoilStability_All = all,
      SoilStability_Protected = covered,
      SoilStability_Unprotected = uncovered
    )

  #Return
  return(soil.stability.calcs)
}

#' @export build_terradat_indicators
#' @rdname aim_gdb
#Build indicators feature class
build_terradat_indicators <- function(dsn, layer, ...) {
  #Test that layer is  "TerrADat"
  if (!layer %in% c("TerrADat")) {
    stop ("Invalid indicator layer specified")
  }

  #Assign filter expressions
  filter_exprs <- rlang::quos(...)

#Read header in
 header <- header_build(dsn = dsn, source = layer, !!!filter_exprs)

 #Join all indicator calculations together
 indicators <- list (header,
                     #LPI
                lpi_calc(dsn = dsn,
                              header = header,
                              layer = layer),
                     #Gap
                     gap_calc(dsn = dsn,
                              header = header,
                              layer = layer),
                     #Height
                     height_calc(dsn = dsn,
                                 header = header,
                                 layer = layer),
                     #Species Inventory
                     spp_inventory_calc(dsn = dsn,
                                        header = header,
                                        layer = layer),
                     #Soil Stability
                     soil_stability_calc(dsn = dsn,
                                         header = header,
                                         layer = layer),
                     #Rangeland Health
                     gather.rangeland.health.terradat(dsn)

 )

all_indicators <- Reduce(dplyr::left_join, indicators)

return(all_indicators)

}

#Build LMF Indicators
#' @export build_lmf_indicators
#' @rdname aim_gdb

build_lmf_indicators<- function(dsn, layer, ...) {

  #Test that layer is  "LMF"
  try (!layer %in% c("LMF", "NRI"),
    stop ("Invalid indicator layer specified"))


  #Assign filter expressions
  filter_exprs <- rlang::quos(...)

  #Read header in
  header <- header_build(dsn, source = layer, !!!filter_exprs)

  #Join all indicator calculations together
  indicators <- list (header,
                      # LPI
                      lpi_calc(dsn = dsn,
                               header = header,
                               layer = layer),
                      # Gap
                      gap_calc(dsn = dsn,
                               header = header,
                               layer = layer),
                      # Height
                      height_calc(dsn = dsn,
                                  header = header,
                                  layer = layer),
                      # Species Inventory
                      spp.inventory <- spp_inventory_calc(dsn = dsn,
                                         header = header,
                                         layer = layer),
                      #Soil Stability
                      soil.stab <- soil_stability_calc(dsn = dsn,
                                          header = header,
                                          layer = layer),
                      #Rangeland Health
                      IIRH <- gather.rangeland.health(dsn, layer = layer)


  )

  all_indicators <- Reduce(dplyr::left_join, indicators)

  return(all_indicators)

}


# Build wrapper
build_indicators <- function(dsn, layer, ...) {
  all_indicators <- switch(layer,
                           "TerrADat" = build_terradat_indicators(dsn = dsn,
                                                                  layer = layer,
                                                                  ...),
                           "LMF" = build_lmf_indicators(dsn = dsn,
                                                        layer = layer,
                                                        ...))


  # Compare indicator field names with the names for a the target feature class
  feature_class_field_names <- sf::st_read(dsn, layer = layer)

  feature_class_field_names <- feature_class_field_names[,
    !colnames(feature_class_field_names) %in%
      c("created_user",
        "created_date",
        "last_edited_user",
        "last_edited_date")]

  #
  indicator_field_names <- data.frame(name = names(all_indicators),
                                      calculated = "yes")

  missing_names <- data.frame(name = names(feature_class_field_names),
                              feature.class = "yes") %>%
    #Join feature class field names to indicator field names
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

  #Convert back to spatial
  final_feature_class <- sf::st_as_sf(x = final_feature_class,
                                      coords = c( "Longitude","Latitude"),
                                      crs = sf::st_crs(feature_class_field_names,),
                                      remove = FALSE)


  return(final_feature_class)

}
