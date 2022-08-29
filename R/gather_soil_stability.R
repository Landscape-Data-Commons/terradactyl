#' Convert soil stability data into tall, tidy data frame
#'
#' @description Given soil stability create a tall format data frame usable by
#' other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase containing the table of interest. This field
#' is unnecessary if you supply either both of tblSoilStabDetail and
#' tblSoilStabHeader (AIM/DIMA/TerrADat) or SOILHORIZON (LMF/NRI).
#' @param source Character string. The data source format, can be \code{"AIM"},
#' \code{"TerrADat"}, \code{"DIMA"}, \code{"LMF"}, or \code{"NRI"} (case independent).
#' @param tblSoilStabDetail Dataframe of the data structure tblSoilStabDetail
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param tblSoilStabHeader Dataframe of the data structure tblSoilStabHeader
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when data
#' source is LMF or NRI; alternately provide dsn.
#' @importFrom magrittr %>%
#' @name gather_soil_stability
#' @family <gather>
#' @return A tall data frame containing soil horizon data.
#' @examples
#' gather_soil_stability(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                       source = "AIM")
#' gather_soil_stability(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                       source = "LMF")
#'
#' aim_soilstabdetail <- read.csv("Path/To/tblSoilStabDetail.csv")
#' aim_soilstabheader <- read.csv("Path/To/tblSoilStabHeader.csv")
#' gather_soil_stability(source = "AIM",
#'                       tblSoilStabDetail = aim_soilstabdetail,
#'                       tblSoilStabHeader = aim_soilstabheader)
#'
#' lmf_horizons <- read.csv("Path/To/SOILHORIZON.csv")
#' gather_soil_stability(source = "LMF",
#'                       SOILHORIZON = lmf_horizons)

#' @export gather_soil_stability_terradat
#' @rdname gather_soil_stability
gather_soil_stability_terradat <- function(dsn = NULL,
                                           tblSoilStabDetail = NULL,
                                           tblSoilStabHeader = NULL) {


  if(!is.null(tblSoilStabDetail) & !is.null(tblSoilStabHeader)){
    soil_stability_detail <- tblSoilStabDetail
    soil_stability_header <- tblSoilStabHeader
  } else if (!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblSoilStabDetail and tblSoilStabHeader")
    }
    soil_stability_detail <-
      suppressWarnings(sf::st_read(dsn,
                                   layer = "tblSoilStabDetail",
                                   stringsAsFactors = FALSE, quiet = T
      ))

    soil_stability_header <-
      suppressWarnings(sf::st_read(dsn,
                                   layer = "tblSoilStabHeader",
                                   stringsAsFactors = FALSE, quiet = T
      ))
  } else {
    stop("Supply either tblSoilStabDetail and tblSoilStabHeader, or a path to a GDB containing those tables")
  }

  soil_stability_detail <- soil_stability_detail %>%
    dplyr::select_if(!names(.) %in% c(
      "created_user",
      "created_date",
      "last_edited_user",
      "last_edited_date",
      "GlobalID"
    ))

  soil_stability_header <- soil_stability_header %>%
    dplyr::select_if(!names(.) %in% c(
      "created_user",
      "created_date",
      "last_edited_user",
      "last_edited_date",
      "GlobalID"
    ))


  # remove orphaned records
  soil_stability_detail <-
    soil_stability_detail[!is.na(soil_stability_detail$PrimaryKey), ]

  # If DBKey Key exists, remove it
  if ("DBKey" %in% colnames(soil_stability_detail)) {
    soil_stability_detail <- dplyr::select(soil_stability_detail, -DBKey)
  }

  gathered <- soil_stability_detail %>%
    # Remove standard columns (In and Dip Times and Date Downloaded in DB)
    dplyr::select(.,
                  match = -dplyr::starts_with("In"),
                  -dplyr::starts_with("Dip"),
                  -dplyr::starts_with("DateLoaded")
    ) %>%

    # Convert to tall format
    tidyr::gather(.,
                  key = variable, value = value,
                  -PrimaryKey, -BoxNum, -RecKey, na.rm = TRUE
    )

  # Remove blank values
  gathered <- subset(gathered, value != "")

  # Separate numerical suffixes from field type
  gathered$key <- stringr::str_extract(
    string = gathered$variable,
    pattern = "^[A-z]+"
  )
  gathered$Position <- stringr::str_extract(
    string = gathered$variable,
    pattern = "[0-9]+"
  )

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
  soil_stability_detail_tidy <- purrr::reduce(
    soil_stability_detail_list,
    dplyr::full_join, by = c("RecKey", "PrimaryKey", "Position")
  ) %>% unique()

  soil_stability_detail_tidy$Rating <- soil_stability_detail_tidy$Rating %>%
    as.numeric()

  # Merge soil stability detail and header tables
  soil_stability_tall <- dplyr::left_join(
    soil_stability_header,
    soil_stability_detail_tidy, by = c("RecKey", "PrimaryKey")
  ) %>% dplyr::select_if(!names(.) %in% c(
    'PlotKey', 'DateModified', 'FormType', 'DataEntry', 'DataErrorChecking', 'DateLoadedInDb'
    )
  )

  # Return final merged file
  return(soil_stability_tall)
}

#' @export gather_soil_stability_lmf
#' @rdname gather_soil_stability

gather_soil_stability_lmf <- function(dsn = NULL,
                                      file_type = "gdb",
                                      SOILDISAG = NULL
) {


  if(!is.null(SOILDISAG)) {
    soildisag <- SOILDISAG
  } else if(!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing SOILDISAG")
    }

    soildisag <- switch(file_type,
                        "gdb" = {
                          suppressWarnings(sf::st_read(
                            dsn = dsn, layer = "SOILDISAG",
                            stringsAsFactors = FALSE, quiet = T
                          ))
                        },
                        "txt" = {
                          read.table(paste(dsn, "soildisag.txt", sep = ""),
                                     stringsAsFactors = FALSE,
                                     strip.white = TRUE, header = FALSE, sep = "|"
                          )
                        },
                        "csv" = {
                          read.csv(dsn)
                        }
    )

    # Add column names
    if (file_type == "txt") {
      soildisag <- name_variables_nri(
        data = soildisag,
        table_name = "SOILDISAG"
      )
    }

  } else {
    stop("Supply either SOILDISAG or a path to a gdb containing that table")
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

  gathered <- tidyr::separate(gathered,
                              col = variable,
                              into = c("type", "Position"),
                              sep = "[[:alpha:]]+",
                              remove = FALSE
  ) %>%
    dplyr::mutate(variable = stringr::str_extract(
      string = gathered$variable,
      pattern = "^[A-z]+"
    )) %>%
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
    #) %>% Reduce(dplyr::left_join, ., by = c("RecKey", "PrimaryKey", "Position"))
  ) %>% purrr::reduce(dplyr::left_join, by = c("PrimaryKey", "DBKey", "Position")
  )

  # Rename fields
  soil_stability_tidy <- dplyr::rename(soil_stability_tidy,
                                       Veg = VEG,
                                       Rating = STABILITY
  )

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
gather_soil_stability <- function(dsn = NULL,
                                  source,
                                  file_type = "gdb",
                                  tblSoilStabDetail = NULL,
                                  tblSoilStabHeader = NULL,
                                  SOILDISAG = NULL
) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    soil_stability <- gather_soil_stability_terradat(
      dsn = dsn,
      tblSoilStabDetail = tblSoilStabDetail,
      tblSoilStabHeader = tblSoilStabHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    soil_stability <- gather_soil_stability_lmf(
      dsn = dsn,
      file_type = file_type,
      SOILDISAG = SOILDISAG)

  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  soil_stability$source <- source

  if("sf" %in% class(soil_stability)) soil_stability <- sf::st_drop_geometry(soil_stability)

  if (any(class(soil_stability) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(soil_stability)[do.call(rbind, vapply(soil_stability,
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    soil_stability <- dplyr::mutate_at(soil_stability, dplyr::vars(change_vars),
                            dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  soil_stability <- soil_stability %>%
    dplyr::select(PrimaryKey, DBKey, tidyselect::everything())

  return(soil_stability)
}
