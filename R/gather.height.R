#' Convert wide-format TerrADat height data to a tall, tidy format
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param source Character string. Identify the source data ("TerrADat", "LMF", "NRI") for gathering
#' @param file_type Charater string. Identifies the original source file type ("gdb", "txt")
#' @return A data frames containing the data from the height measurements.

## Gather Height Data
#' @export gather_height_terradat
#' @rdname gather_height
gather_height_terradat <- function(dsn) {
  # Make sure the geodatabse exists
  if (!file.exists(dsn)) {
    stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
  }

  # Read in the LPI tables from the geodatabase
  lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblLPIDetail"))
  lpi_header <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblLPIHeader"))

  ## Make this an else statement
  if (any(colnames(lpi_header) %in% "DBKey")) {
    levels <- rlang::quos(PrimaryKey, "DBKey")
  } else {
    levels <- rlang::quos(PrimaryKey)
  }

  # we only want to carry a subset of the lpi_header fields forward
  lpi_header <- dplyr::select(lpi_header, !!!levels, LineKey:CheckboxLabel)

  lpi_height_tall_woody <- dplyr::select(
    .data = lpi_detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Woody$")
  ) %>% dplyr::mutate(type = "woody")
  # Strip out the extra name stuff so woody and herbaceous variable names match.
  names(lpi_height_tall_woody) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody),
    pattern = "Woody$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi_height_tall_woody$GrowthHabit_measured <- "Woody"


  # Herbaceous height
  lpi_height_tall_herb <- dplyr::select(
    .data = lpi_detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Herbaceous$")
  ) %>% dplyr::mutate(type = "herbaceous")
  names(lpi_height_tall_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_herb),
    pattern = "Herbaceous$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi_height_tall_herb$GrowthHabit_measured <- "NonWoody"

  # Gather lower herbaceous heights
  lpi_height_tall_lower_herb <- dplyr::select(
    .data = lpi_detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("LowerHerb$")
  ) %>% dplyr::mutate(type = "lower.herbaceous")
  names(lpi_height_tall_lower_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_lower_herb),
    pattern = "LowerHerb$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi_height_tall_lower_herb$GrowthHabit_measured <- "NonWoody"

  # Merge all three gather types together
  lpi_height <- rbind(
    lpi_height_tall_woody,
    lpi_height_tall_herb,
    lpi_height_tall_lower_herb
  )
  lpi_height <- lpi_height %>%
    dplyr::full_join(x = ., y = lpi_header) %>%
    subset(., !is.na(Height))

  # Add NA to fields with no species
  lpi_height$Species[!grepl(pattern = "[[:digit:]]|[[:alpha:]]", lpi_height$Species)] <- NA

  # Remove orphaned records and duplicates, if they exist
  lpi_height <- unique(lpi_height)
  lpi_height <- lpi_height[!is.na(lpi_height$PrimaryKey), ]

  # Output the woody/herbaceous level data
  return(lpi_height)
}

#' @export gather_height_lmf
#' @rdname gather_height

# Gather Height for LMF/NRI
gather_height_lmf <- function(dsn,
                              file_type = "gdb") {

  # Read in the data as .txt or .gdb
  vegheight <- switch(file_type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn,
        layer = "PASTUREHEIGHTS",
        stringsAsFactors = FALSE
      ))
    },
    "txt" = {
      read.table(paste(dsn, "pastureheights.txt", sep = ""),
        stringsAsFactors = FALSE,
        header = FALSE,
        sep = "|",
        strip.white = TRUE
      )
    },
    "csv" = {
      read.csv(dsn)
    }
  )

  if (file_type == "txt") {
    # if it is in a text file, there are no field names assigned.
    colnames <- subset(
      terradactyl::nri.data.column.explanations,
      TABLE.NAME == "PASTUREHEIGHTS"
    ) %>%
      dplyr::pull(FIELD.NAME) %>%
      unique()



    vegheight <- vegheight[seq_len(length(colnames))]
    names(vegheight) <- colnames

    # We need to establish and/or fix the PLOTKEY so it exists in a single field.
    vegheight$PrimaryKey <- paste(vegheight$SURVEY,
      vegheight$STATE,
      vegheight$COUNTY,
      vegheight$PSU,
      vegheight$POINT,
      sep = ""
    )

    # Assign DBKey
    vegheight$DBKey <- vegheight$SURVEY
  }

  height_woody <- dplyr::select(
    .data = vegheight,
    PrimaryKey,
    DBKey,
    TRANSECT,
    DISTANCE,
    dplyr::matches("^W")
  ) %>% dplyr::mutate(type = "woody")
  # remove the "W" from the names
  names(height_woody) <- stringr::str_replace_all(
    string = names(height_woody),
    pattern = "W",
    replacement = ""
  )

  height_herbaceous <- dplyr::select(
    .data = vegheight,
    PrimaryKey,
    DBKey,
    TRANSECT,
    DISTANCE,
    dplyr::matches("^H")
  ) %>% dplyr::mutate(type = "herbaceous")

  # remove the "H" from the "HPLANT" field
  names(height_herbaceous)[names(height_herbaceous) == "HPLANT"] <- "PLANT"
  height <- rbind(height_woody, height_herbaceous)

  # remove NA values
  height <- subset(height, !is.na(HEIGHT))


  # The height units are concatenated in the field,
  # separate so that we can convert to metric appopriately
  height <- tidyr::separate(height, "HEIGHT", c("HEIGHT", "UOM"),
    sep = " ", extra = "drop", fill = "right"
  )

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
    LineKey = TRANSECT,
    PointNbr = DISTANCE,
    Height = HEIGHT,
    Species = PLANT
  )

  # return height
  return(height)
}

#' Gather Height wrapper for all data types
#' @export gather_height
#' @rdname gather_height

gather_height <- function(dsn,
                          file_type = "gdb",
                          source) {
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
  } )

  # Gather gap using the appropriate method
  height <- switch(toupper(source),
    "AIM" = gather_height_terradat(dsn = dsn),
    "TERRADAT" = gather_height_terradat(dsn = dsn),
    "DIMA" = gather_height_terradat(dsn = dsn),
    "LMF" = gather_height_lmf(dsn = dsn, file_type = file_type),
    "NRI" = gather_height_lmf(dsn = dsn, file_type = file_type)
  )

  # Add source field so that we know where the data came from
  height$source <- toupper(source)

  # Make sure height is a numeric field
  height$Height <- as.numeric(height$Height)

  # Output height
  return(height)
}
