#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @return A \code{tbl} of indicators of either tall or wide format.

#' @export header_build
#' @rdname aim_gdb
#Build the header portion of the table
header_build <- function(dsn, ... ){
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  #Test the dsn input for a valid .gdb extension
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
  header <- left_join(
    select(sites, SiteID, SiteKey, ProjectName = SiteName),
    plots) %>%

    #If there are any Sites with no PrimaryKeys, delete them
    subset(!is.na(PrimaryKey))

  #Return the header file
  return(header)

}

#' @export lpi_calc
#' @rdname aim_gdb
#Calculate the LPI indicators
lpi_calc <- function(header, dsn, layer) {
  # Format the lpi data for calculations ----
  lpi.tall <- gather.lpi(dsn = dsn, source = layer) %>%
    # Join the lpi data to the header PrimaryKeys and add the StateSpecies Key
    dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .)

  # Join to the state species list via the SpeciesState value
  lpi.species <- species.join(data = lpi.tall, species.file = dsn)

  # If sedges exist as a growth habit, combine with graminoid
  lpi.species <- lpi.species %>%
    dplyr::mutate(GrowthHabitSub = GrowthHabitSub %>%
                    stringr::str_replace( pattern = "Sedge",
                                          replacement = "Graminoid"))

  # Calculate Total Foliar Cover ----
  total.foliar <- pct.cover.total.foliar(lpi.species, tall = TRUE)

  # Calculate between plant cover (includes bare soil) ----
  between.plant.cover <- pct.cover.between.plant(lpi.species,
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
    dplyr::mutate(indicator = indicator %>% stringr::str_replace_all(., between.plant.replace)) %>%

    # Add FH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = "")) %>%

    # Remove "FH_" from the BareSoilCover indicator
    dplyr::mutate(indicator = indicator %>% stringr::str_replace(., "FH_BareSoilCover", "BareSoilCover"))

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
  ah.spp.group.cover<- ah.spp.group.cover %>%
    # Substitute "NonNox" for "NO
    dplyr::mutate(indicator = indicator %>% stringr::str_replace_all(., spp.cover.replace)) %>%

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
    # Substitute "NonNox" for "NO
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = ""))



  #   SageBrush Shape
  # Need to check this with sagebrush state data
  sagebrush.shape <- sagebrush.shape(lpi.species)

  # Combine  all LPI based cover indicators----
  lpi.cover <- dplyr::bind_rows(ah.spp.group.cover,
                     fh.spp.group.cover,
                     total.foliar,
                     between.plant.cover,
                     sagebrush.shape) %>%
    #Spread to a wide format
    tidyr::spread(key = indicator, value = percent, fill = 0)

  # Date visited is set from the first line in the LPI data
  lpi.cover <- lpi.species %>%
    dplyr::select(PrimaryKey, FormDate) %>%
    dplyr::group_by(PrimaryKey) %>%
    summarize(DateVisited = first(FormDate, order_by = FormDate) %>%
                as.POSIXct()) %>%
    #Join to the lpi.cover data
    dplyr::left_join(lpi.cover, .)




  #Return lpi.cover
  return(lpi.cover)

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
  gap.calcs <- gap.cover(gap.tall = gap.subset,
                         by.line = FALSE,
                         tall = FALSE)$percent %>%
    dplyr::select(PrimaryKey,
                  GapCover_25_50 = "[25,51)", GapCover_51_100 = "[51,100)",
                  GapCover_101_200 = "[100,200)", GapCover_200_plus = "[200,1e+05)"
    ) %>%
    #Calculate the summation indicator
    dplyr::mutate(GapCover_25_plus = sum(GapCover_25_50,
                                         GapCover_51_100,
                                         GapCover_101_200,
                                         GapCover_200_plus))


  #Return
  return(gap.calcs)
}


#' @export height_calc
#' @rdname aim_gdb
#Calculate the Height indicators for AIM
height_calc <- function(header, dsn){
  # gather tall height
  height <- gather.height(dsn, species.characteristics = FALSE) %>%
    #subset by PK and add the SpeciesState from the header
  dplyr::left_join(select(header, PrimaryKey, SpeciesState), .)

  # Join to species list
  height.species <- species.join(
    data = height.state.species,
    data.code = "Species", species.file = dsn
  ) %>%
    # Convert sedge to graminoid
    dplyr::mutate(GrowthHabitSub = str_replace(GrowthHabitSub,
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

  ## Height calculations----
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
    ) %>% subset(indicator != "NA"),
    # Live sagebrush heights
    mean.height(
      height.tall = height.species,
      method = "mean",
      omit.zero = TRUE, # remove zeros from average height calcs
      by.line = FALSE,
      tall = TRUE,
      GRSG_Group, Chkbox
    ) %>% subset(indicator == "Sagebrush.0")
  )
  ## Reformat for Indicator Field Name ----
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
spp_inventory_calc <- function (header, dsn) {
  # tidy.species
  spp.inventory.tall <- gather.species.inventory(dsn = dsn) %>%
    #Join to the header to get the relavent PrimaryKeys and SpeciesSate
    dplyr::left_join(dplyr::select(header, PrimaryKey, SpeciesState), .)

   # Join to State Species List
  spp.inventory.species <- species.join(
    data = spp.inventory.subset,
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
      dplyr::mutate(indicator = indicator %>% stringr::str_replace_all(c(" " = "")))
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
    select(-GRSG_Group) %>%
    #Output in wide format
    tidyr::spread(key = indicator, value = list, fill = NA)

  #Join with spp.inventory and return
  species.calc <- dplyr::full_join(spp.inventory.wide, spp.list)

  return(species.calc)
}


#' @export soil_stability_calc
#' @rdname aim_gdb
#Calculate soil stability values
soil_stability_calc <- function (header, dsn) {
  # Gather and subset
  soil.stability.tall <- gather.soil.stability(dsn) %>%
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

#' @export build.terradat.indicators
#' @rdname aim_gdb
#Build indicators feature class
build.terradat.indicators <- function(dsn, layer, ...) {
  #Assign filter expressions
  filter_exprs <- rlang::quos(...)

#Read header in
 header <- header_build(dsn, !!!filter_exprs)

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
                                 header = header),
                     #Species Inventory
                     spp_inventory_calc(dsn = dsn,
                                        header = header),
                     #Soil Stability
                     soil_stability_calc(dsn = dsn,
                                         header = header),
                     #Rangeland Health
                     gather.rangeland.health(dsn)

 )

 all.indicators<- Reduce(dplyr::left_join, indicators)


 # Compare indicator field names with the names for a the target feature class
 feature.class.field.names <- sf::st_read(dsn, layer = layer)

 #
 indicator.field.names <- data.frame(name = names(all.indicators),
                                     calculated = "yes")

 missing.names <- data.frame(name = names(feature.class.field.names),
                                              feature.class = "yes") %>%
   #Join feature class field names to indicator field names
   dplyr::full_join(indicator.field.names) %>%

   # get the field names where there is not corrollary in calculated
   subset(is.na(calculated), select = "name") %>%
   dplyr::mutate(value = NA) %>%
   # make into a data frame
   tidyr::spread(key = name, value = value) %>%
   dplyr::select(-Shape, -GlobalID)

 # Add a row for each PrimaryKey in all.indicators
 missing.names[nrow(all.indicators), ] <- NA
 # For some indicators, the null value is 0 (to indicate the method was completed, but no data in that group were collected)
 missing.names[, grepl(names(missing.names), pattern = "^FH|^AH|^Num")] <- 0

 # Merge back to indicator data to create a feature class for export
 final.feature.class <- cbind(all.indicators, missing.names)


}



#
#   #### Join all Indicators together ####
#   # First we need to establish the date the plot was visited
#   date.visited <- lpi.species %>%
#     dplyr::select(PrimaryKey, FormDate) %>%
#     dplyr::group_by(PrimaryKey) %>%
#     summarize(DateVisited = first(FormDate, order_by = FormDate) %>% as.POSIXct())
#
#   all.indicators <- site.plots %>%
#     # Add Date Visited from LPI
#     dplyr::left_join(date.visited) %>%
#     # Species cover data
#     dplyr::full_join(tidyr::spread(spp.group.cover, key = indicator, value = percent, fill = 0)) %>%
#     # Between plant cover
#     dplyr::full_join(tidyr::spread(between.plant.cover, key = indicator, value = percent, fill = 0)) %>%
#     # Height
#     dplyr::full_join(tidyr::spread(height.calc, key = indicator, value = mean.height, fill = 0)) %>%
#     # Gap
#     dplyr::full_join(tidyr::spread(gap.calcs, key = indicator, value = percent, fill = 0)) %>%
#     # Spp Inventory
#     dplyr::full_join(tidyr::spread(spp.inventory, key = indicator, value = n, fill = 0)) %>%
#     # Species Lists
#     dplyr::full_join(tidyr::spread(spp.lists, key = indicator, value = list, fill = 0)) %>%
#     # Soil stability
#     dplyr::full_join(soil.stability.cover) %>%
#     # Rangeland Healh
#     dplyr::left_join(rangeland.health) %>%
#     # Sagebrush Shape
#     dplyr::full_join(sagebrush.shape) %>%
#     # Total Foliar Cover
#     dplyr::full_join(total.foliar)
#
#
#
#   # Compare all indicators field names with the names for a completed feature class
#   indicator.field.names <- sf::st_read(dsn, layer = layer)
#
#   calculated.field.names <- data.frame(name = names(all.indicators), calculated = "yes")
#
#   missing.names <- dplyr::full_join(data.frame(name = names(indicator.field.names), feature.class = "yes"), calculated.field.names) %>%
#     # get the field names where there is not corrollary in calculated
#     subset(is.na(calculated), select = "name") %>%
#     dplyr::mutate(value = NA) %>%
#     # make into a data frame
#     tidyr::spread(key = name, value = value) %>%
#     dplyr::select(-Shape, -GlobalID)
#
#   # Add a row for each PrimaryKey in all.indicators
#   missing.names[nrow(all.indicators), ] <- NA
#   # For some indicators, the null value is 0 (to indicate the method was completed, but no data in that group were collected)
#   missing.names[, grepl(names(missing.names), pattern = "^FH|^AH|^Num")] <- 0
#
#   # Merge back to indicator data to create a feature class for export
#   final.feature.class <- cbind(all.indicators, missing.names)
#
#   return(final.feature.class)
# }
