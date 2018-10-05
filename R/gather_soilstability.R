#' Gather Soil Stability Data
#' @param dsn Character string. Full filepath including file extension to the geodatabase that contains the tables tblSoilStabDetail and tblSoilStabHeader.
#'
#'

#' @export gather.soil.stability.terradat
#' @rdname gather_soilstability
gather.soil.stability.terradat <- function(dsn) {


  # read in tabular data
  soil.stability.detail <- suppressWarnings(sf::st_read(dsn, layer = "tblSoilStabDetail")) %>%
    dplyr::select(-c("created_user", "created_date", "last_edited_user", "last_edited_date", "GlobalID"))
  soil.stability.header <- suppressWarnings(sf::st_read(dsn, layer = "tblSoilStabHeader")) %>%
    dplyr::select(-c("created_user", "created_date", "last_edited_user", "last_edited_date", "GlobalID"))


  # remove orphaned records
  soil.stability.detail <- soil.stability.detail[!is.na(soil.stability.detail$PrimaryKey), ]
  # Morph soil stability detail into a tidy format

  # If DBKey Key exists, remove it
  if ("DBKey" %in% colnames(soil.stability.detail)) {
    soil.stability.detail <- dplyr::select(soil.stability.detail, -DBKey)
  }

  gathered <- soil.stability.detail %>%
    # Remove standard columns (In and Dip Times and Date Downloaded in DB)
    dplyr::select(., match = -dplyr::starts_with("In"), -dplyr::starts_with("Dip"), -dplyr::starts_with("DateLoaded")) %>%
    # Convert to tall format
    tidyr::gather(., key = variable, value = value, -PrimaryKey, -BoxNum, -RecKey, na.rm = TRUE)

  # Remove blank values
  gathered <- subset(gathered, value != "")

  # Separate numerical suffixes from field type
  gathered$key <- stringr::str_extract(string = gathered$variable, pattern = "^[A-z]+")
  gathered$Position <- stringr::str_extract(string = gathered$variable, pattern = "[0-9]+")

  gathered <- subset(gathered, select = -c(variable, BoxNum))

  # #Remove duplicates
  # gathered<-unique(gathered)

  # Spread the gathered data so that Line, Rating, Vegetation, and Hydro are all different variables

  soil.stability.detail.list <- lapply(
    X = as.list(unique(gathered$key)),
    FUN = function(k = as.list(unique(gathered$key)), df = gathered) {
      test <- df[df$key == k, ] %>%
        dplyr::mutate(id = 1:n()) %>%
        tidyr::spread(key = key, value = value) %>%
        dplyr::select(-id)
    }
  )
  # create a single tidy dataframe
  soil.stability.detail.tidy <- purrr::reduce(soil.stability.detail.list, dplyr::full_join) %>% unique()

  soil.stability.detail.tidy$Rating <- as.numeric(soil.stability.detail.tidy$Rating)

  # Merge soil stability detail and header tables
  soil.stability.tall <- dplyr::left_join(soil.stability.header, soil.stability.detail.tidy)


  # Return final merged file
  return(soil.stability.tall)
}

#' @export gather.soil.stability.terradat
#' @rdname gather_soilstability

gather.soil.stability.lmf <- function(dsn, file.type = "gdb") {
  soildisag <- switch(file.type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn = dsn, layer = "SOILDISAG"))
    },
    "txt" = {
      read.table(paste(dsn, "soildisag.txt", sep = ""), stringsAsFactors = FALSE, strip.white = TRUE, header = FALSE, sep = "|")
    }
  )
  # Add column names
  if (file.type == "txt") {
    colnames <- as.vector(as.data.frame(subset(terradactyl::nri.data.column.explanations, TABLE.NAME == "SOILDISAG", select = FIELD.NAME)))
    colnames <- colnames$FIELD.NAME
    soildisag <- soildisag[1:length(colnames)]
    names(soildisag) <- colnames
  }
  # We need to establish and/or fix the PLOTKEY so it exists in a single field.
  soildisag$PrimaryKey <- paste(soildisag$SURVEY, soildisag$STATE, soildisag$COUNTY, soildisag$PSU, soildisag$POINT, sep = "")

  #Remove any database management fields
  soildisag <- soildisag[!names(soildisag) %in% c ("created_user",
                                                   "created_date",
                                                   "last_edited_user",
                                                   "last_edited_date" )]
  # conver white space to NA
  soildisag[soildisag == ""] <- NA

  # Convert to tall format
  soil.tall <- dplyr::select(soildisag, VEG1:STABILITY18, PrimaryKey, DBKey) %>%
    tidyr::gather(., key = variable, value = value, -PrimaryKey, -DBKey)

  # Remove NAs
  gathered <- soil.tall[!is.na(soil.tall$value), ]

  # Separate numerical suffixes from field type
  gathered$variable <- stringr::str_extract(string = gathered$variable, pattern = "^[A-z]+")

  # Spread the gathered data so that Line, Rating, Vegetation, and Hydro are all different variables

  soil.stability.tidy <- lapply(
    X = as.list(unique(gathered$variable)),
    FUN = function(k = as.list(unique(gathered$variable)), df = gathered) {
      df[df$variable == k, ] %>%
        dplyr::mutate(id = 1:n()) %>%
        tidyr::spread(key = variable, value = value) %>%
        dplyr::select(-id)
    }
  ) %>% Reduce(dplyr::left_join, .)

  soil.stability.tidy <- dplyr::rename(soil.stability.tidy, Veg = VEG, Rating = STABILITY)
  soil.stability.tidy$Rating <- as.numeric(soil.stability.tidy$Rating)

  # Return final merged file
  return(soil.stability.tidy)
}


#' @export gather.soil.stability
#' @rdname gather_soilstability
gather.soil.stability <- function (dsn, source, file.type = "gdb") {

  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) stop("No valid source provided"))

  # Gather soil.stability using the appropriate method
  soil.stability <- switch(toupper(source),
                              "AIM" = gather.soil.stability.terradat(dsn = dsn),
                              "TERRADAT" = gather.soil.stability.terradat(dsn = dsn),
                              "DIMA" = gather.soil.stability.terradat(dsn = dsn),
                              "LMF" = gather.soil.stability.lmf(dsn = dsn, file.type = file.type),
                              "NRI" = gather.soil.stability.lmf(dsn = dsn, file.type = file.type)
  )

  # Add source field so that we know where the data came from
  soil.stability$source <- toupper(source)

  return(soil.stability)

}
