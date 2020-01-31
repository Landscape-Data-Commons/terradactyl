#' Gather Soil Stability Data
#' @param dsn Character string. Full filepath including file extension to the
#' geodatabase that contains soil stability data.
#' @param source String. Specifies the original data source.

#' @export gather_soil_stability_terradat
#' @rdname gather_soil_stability

gather_soil_stability_terradat <- function(dsn) {

  # read in tabular data
  soil_stability_detail <-
    suppressWarnings(sf::st_read(dsn,
                                 layer = "tblSoilStabDetail",
                                 stringsAsFactors = FALSE)) %>%
    dplyr::select(-c("created_user",
                     "created_date",
                     "last_edited_user",
                     "last_edited_date",
                     "GlobalID"))
  # tblSoilStabHeader
  soil_stability_header <-
    suppressWarnings(sf::st_read(dsn,
                                 layer = "tblSoilStabHeader",
                                 stringsAsFactors = FALSE)) %>%
    dplyr::select(-c("created_user",
                     "created_date",
                     "last_edited_user",
                     "last_edited_date",
                     "GlobalID"))


  # remove orphaned records
  soil_stability_detail <-
    soil_stability_detail[!is.na(soil_stability_detail$PrimaryKey), ]

  # If DBKey Key exists, remove it
  if ("DBKey" %in% colnames(soil_stability_detail)) {
    soil_stability_detail <- dplyr::select(soil_stability_detail, -DBKey)
  }

  gathered <- soil_stability_detail %>%
    # Remove standard columns (In and Dip Times and Date Downloaded in DB)
    dplyr::select(., match = -dplyr::starts_with("In"),
                  -dplyr::starts_with("Dip"),
                  -dplyr::starts_with("DateLoaded")) %>%

    # Convert to tall format
    tidyr::gather(., key = variable, value = value,
                  -PrimaryKey, -BoxNum, -RecKey, na.rm = TRUE)

  # Remove blank values
  gathered <- subset(gathered, value != "")

  # Separate numerical suffixes from field type
  gathered$key <- stringr::str_extract(string = gathered$variable,
                                       pattern = "^[A-z]+")
  gathered$Position <- stringr::str_extract(string = gathered$variable,
                                            pattern = "[0-9]+")

  gathered <- subset(gathered, select = -c(variable, BoxNum))

  # Remove Hydro = 0
  gathered <- gathered %>% subset(!(key == "Hydro" & value != 0))

  # Spread the gathered data so that Line, Rating, Vegetation,
  # and Hydro are all different variables

  soil_stability_detail_list <- lapply(
    X = as.list(unique(gathered$key)),
    FUN = function(k = as.list(unique(gathered$key)), df = gathered) {
      test <- df[df$key == k, ] %>%
        dplyr::mutate(id = 1:dplyr::n()) %>%
        tidyr::spread(key = key, value = value) %>%
        dplyr::select(-id)
    }
  )
  # create a single tidy dataframe
  soil_stability_detail_tidy <- purrr::reduce(soil_stability_detail_list,
                                              dplyr::full_join) %>% unique()

  soil_stability_detail_tidy$Rating <- soil_stability_detail_tidy$Rating %>%
    as.numeric()

  # Merge soil stability detail and header tables
  soil_stability_tall <- dplyr::left_join(soil_stability_header,
                                          soil_stability_detail_tidy)


  # Return final merged file
  return(soil_stability_tall)
}

#' @export gather_soil_stability_lmf
#' @rdname gather_soil_stability

gather_soil_stability_lmf <- function(dsn, file_type = "gdb") {
  soildisag <- switch(file_type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn = dsn, layer = "SOILDISAG",
                                   stringsAsFactors = FALSE))
    },
    "txt" = {
      read.table(paste(dsn, "soildisag.txt", sep = ""),
                 stringsAsFactors = FALSE,
                 strip.white = TRUE, header = FALSE, sep = "|")
    }
  )

  # Add column names
  if (file_type == "txt") {
    soildisag <- name_variables_nri(data = soildisag,
                                    table_name = "SOILDISAG")
  }

  # Remove any database management fields
  soildisag <- soildisag[!names(soildisag) %in% c(
    "created_user",
    "created_date",
    "last_edited_user",
    "last_edited_date"
  )]

  # convert white space to NA
  soildisag[soildisag == ""] <- NA

  # Convert to tall format
  soil_tall <- dplyr::select(soildisag, VEG1:STABILITY18, PrimaryKey, DBKey) %>%
    tidyr::gather(., key = variable, value = value, -PrimaryKey, -DBKey)

  # Remove NAs
  gathered <- soil_tall[!is.na(soil_tall$value), ]

  gathered <- tidyr::separate(gathered, col = variable,
                              into = c("type", "Position"),
                              sep = "[[:alpha:]]+",
                              remove = FALSE) %>%
    dplyr::mutate(variable = stringr::str_extract(string = gathered$variable,
                                                  pattern = "^[A-z]+")) %>%
    dplyr::select(-type)


  # Spread the gathered data so that Line, Rating, Vegetation,
  # and Hydro are all different variables

  soil_stability_tidy <- lapply(
    X = as.list(unique(gathered$variable)),
    FUN = function(k = as.list(unique(gathered$variable)), df = gathered) {
      df[df$variable == k, ] %>%
        dplyr::mutate(id = 1:dplyr::n()) %>%
        tidyr::spread(key = variable, value = value) %>%
        dplyr::select(-id)
    }
  ) %>% Reduce(dplyr::left_join, .)

  # Rename fields
  soil_stability_tidy <- dplyr::rename(soil_stability_tidy,
                                       Veg = VEG,
                                       Rating = STABILITY)

   # Make sure the rating field is numeric
  soil_stability_tidy$Rating <- as.numeric(soil_stability_tidy$Rating)

  # Remove 0 values
  soil_stability_tidy <- soil_stability_tidy %>% subset(Rating > 0)

  # Return final merged file
  return(soil_stability_tidy)
}

# Wrapper function for all soil stability gather functions
#' @export gather_soil_stability
#' @rdname gather_soil_stability
gather_soil_stability <- function(dsn, source, file_type = "gdb") {

  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"))
    stop("No valid source provided"))

  # Gather soil_stability using the appropriate method
  soil_stability <- switch(toupper(source),
    "AIM" = gather_soil_stability_terradat(dsn = dsn),
    "TERRADAT" = gather_soil_stability_terradat(dsn = dsn),
    "DIMA" = gather_soil_stability_terradat(dsn = dsn),
    "LMF" = gather_soil_stability_lmf(dsn = dsn, file_type = file_type),
    "NRI" = gather_soil_stability_lmf(dsn = dsn, file_type = file_type)
  )

  # Add source field so that we know where the data came from
  soil_stability$source <- toupper(source)

  return(soil_stability)
}
