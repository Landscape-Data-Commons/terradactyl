#' Convert wide-format TerrADat height data to a tall, tidy format
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param species.file Character string. The full file path (including file extension) to the csv containing the species list. If NULL then the file from the provided geodatabase will be used.
#' @param species.growth.habit.code Character. The field name for the growth habit codes in the species file.
#' @param growth.habit.file Character string. The full file path (including file extension) to the csv containing the growth habit list. If NULL then the file from the provided geodatabase will be used.
#' @param growth.habit.code Character. The field name for the growth habit codes in the growth habit file.
#' @param recorded.species.codes Vector. Species recorded so that \code{generic.growth.habit()} can identify unknown codes.
#' @param species.code Character. The field name for the species codes in the species file.
#' @param species.duration Character. the field name for the Duration field in the species file.
#' @return A data frames containing the data from the height measurements.

## Gather Height Data
#' @export gather.height
#' @rdname gather.height
gather.height <- function(dsn,
                          species.characteristics = TRUE,
                          species.file = "", # path to .csv or .gdb holding  the species table
                          species.code = "SpeciesCode", # field name in species file that identifies the species code
                          species.growth.habit.code = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
                          species.duration = "Duration", # field name in species file of the Duration assignment
                          growth.habit.file = "", # path to .csv or gdb holding tblSpeciesGrowthHabit
                          growth.habit.code = "Code") {
  # Make sure the geodatabse exists
  if (!file.exists(dsn)) {
    stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
  }

  # Read in the LPI tables from the geodatabase
  lpi.detail <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblLPIDetail"))
  lpi.header <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblLPIHeader"))

  ## Make this an else statement
  if (any(colnames(lpi.header) %in% "DBKey")) {
    levels <- rlang::quos(PrimaryKey, "DBKey")
  } else {
    levels <- rlang::quos(PrimaryKey)
  }

  # we only want to carry a subset of the lpi.header fields forward
  lpi.header <- dplyr::select(lpi.header, !!!levels, LineKey:CheckboxLabel)

  lpi.height.tall.woody <- dplyr::select(
    .data = lpi.detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Woody$")
  ) %>% dplyr::mutate(type = "woody")
  ## Strip out the extra name stuff so woody and herbaceous variable names will match.
  names(lpi.height.tall.woody) <- stringr::str_replace_all(
    string = names(lpi.height.tall.woody),
    pattern = "Woody$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi.height.tall.woody$GrowthHabit_measured <- "Woody"


  # Herbaceous height
  lpi.height.tall.herb <- dplyr::select(
    .data = lpi.detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Herbaceous$")
  ) %>% dplyr::mutate(type = "herbaceous")
  names(lpi.height.tall.herb) <- stringr::str_replace_all(
    string = names(lpi.height.tall.herb),
    pattern = "Herbaceous$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi.height.tall.herb$GrowthHabit_measured <- "Non-Woody"

  # Gather lower herbaceous heights
  lpi.height.tall.lower.herb <- dplyr::select(
    .data = lpi.detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("LowerHerb$")
  ) %>% dplyr::mutate(type = "lower.herbaceous")
  names(lpi.height.tall.lower.herb) <- stringr::str_replace_all(
    string = names(lpi.height.tall.lower.herb),
    pattern = "LowerHerb$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi.height.tall.lower.herb$GrowthHabit_measured <- "Non-Woody"

  # Merge all three gather types together
  lpi.height <- rbind(lpi.height.tall.woody, lpi.height.tall.herb, lpi.height.tall.lower.herb)
  lpi.height <- lpi.height %>% dplyr::full_join(x = ., y = lpi.header) %>% subset(., !is.na(Height))

  # Add NA to fields with no species
  lpi.height$Species[!grepl(pattern = "[[:digit:]]", lpi.height$Species)] <- NA


  ## If we're adding species
  if (species.characteristics) {

    # Print
    print("Gathering species data")
    lpi.height.species <- species.join(
      data = lpi.height,
      data.code = "Species",
      species.file = species.file, # path to .csv or .gdb holding  the species table
      species.code = species.code, # field name in species file that identifies the species code
      species.growth.habit.code = species.growth.habit.code, # field name in species file of the species code to link to GrowthHabit
      species.duration = species.duration, # field name in species file of the Duration assignment
      growth.habit.file = growth.habit.file, # path to .csv or gdb holding tblSpeciesGrowthHabit
      growth.habit.code = growth.habit.code
    )
    lpi.height.species <- unique(lpi.height.species)
    lpi.height.species <- lpi.height.species[!is.na(lpi.height.species$PrimaryKey), ]
    # Output the species level data
    return(lpi.height.species)
  }

  # Remove orphaned records and duplicates, if they exist
  lpi.height <- unique(lpi.height)
  lpi.height <- lpi.height[!is.na(lpi.height$PrimaryKey), ]
  # Output the woody/herbaceous level data
  return(lpi.height)
}

#' @export gather.height.lmf
#' @rdname gather.height

# Gather Height for LMF/NRI
gather.height.lmf <- function(dsn,
                              file.type = "gdb") {

  # Read in the data as .txt or .gdb
  vegheight <- switch(file.type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn, layer = "PASTUREHEIGHTS", stringsAsFactors = FALSE))
    },
    "txt" = {
      read.table(paste(dsn, "pastureheights.txt", sep = ""), stringsAsFactors = FALSE, header = FALSE, sep = "|", strip.white = TRUE)
    }
  )

  # if it is in a text file, there are no field names assigned.
  colnames <- as.vector(as.data.frame(subset(nri.data.column.explanations, TABLE.NAME == "PASTUREHEIGHTS", select = FIELD.NAME)))
  colnames <- colnames$FIELD.NAME
  vegheight <- vegheight[1:length(colnames)]
  names(vegheight) <- colnames

  # We need to establish and/or fix the PLOTKEY so it exists in a single field.
  vegheight$PLOTKEY <- paste(vegheight$SURVEY, vegheight$STATE, vegheight$COUNTY, vegheight$PSU, vegheight$POINT, sep = "")


  height.woody <- dplyr::select(
    .data = vegheight,
    PLOTKEY,
    TRANSECT,
    DISTANCE,
    dplyr::matches("^W")
  ) %>% dplyr::mutate(type = "woody")
  # remove the "W" from the names
  names(height.woody) <- stringr::str_replace_all(
    string = names(height.woody),
    pattern = "W",
    replacement = ""
  )

  height.herbaceous <- dplyr::select(
    .data = vegheight,
    PLOTKEY,
    TRANSECT,
    DISTANCE,
    dplyr::matches("^H")
  ) %>% dplyr::mutate(type = "herbaceous")

  # remove the "H" from the "HPLANT" field
  names(height.herbaceous)[names(height.herbaceous) == "HPLANT"] <- "PLANT"
  height <- rbind(height.woody, height.herbaceous)

  # remove NA values
  height <- subset(height, !is.na(HEIGHT))


  # The height units are concatenated in the field, separate so that we can convert to metric appopriately
  height <- tidyr::separate(height, "HEIGHT", c("HEIGHT", "UOM"), sep = " ", extra = "drop", fill = "right")

  # Convert to metric
  height$HEIGHT <- suppressWarnings(as.numeric(height$HEIGHT))
  height$UOM <- "cm"

  # convert to centimeters
  height$HEIGHT <- height$HEIGHT * 2.54
  height$UOM[is.na(height$UOM) | height$UOM == "in"] <- "cm"
  height$HEIGHT[height$UOM == "ft"] <- height$HEIGHT[height$UOM == "ft"] * 12
  height$UOM <- "cm"


  # rename field names
  height <- dplyr::rename(height,
    PrimaryKey = PLOTKEY, LineKey = TRANSECT, PointNbr = DISTANCE, Height = HEIGHT,
    Species = PLANT
  )

  # return height
  return(height)
}
