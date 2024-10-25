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
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
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
                                           tblSoilStabHeader = NULL,
                                           auto_qc_warnings = TRUE,
                                           verbose = FALSE) {

  # These are used for data management within a geodatabase and we're going to
  # drop them.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")

  if(!is.null(tblSoilStabDetail) & !is.null(tblSoilStabHeader)){
    detail <- tblSoilStabDetail
    header <- tblSoilStabHeader
  } else if (!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblSoilStabDetail and tblSoilStabHeader")
    }
    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblSoilStabDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))

    header <-suppressWarnings(sf::st_read(dsn = dsn,
                                          layer = "tblSoilStabHeader",
                                          stringsAsFactors = FALSE,
                                          quiet = TRUE))
  } else {
    stop("Supply either both tblSoilStabDetail and tblSoilStabHeader or a path to a GDB containing those tables.")
  }

  # Clean these up!
  detail <- dplyr::select(.data = detail,
                          -tidyselect::any_of(internal_gdb_vars),
                          -tidyselect::any_of("DBKey"),
                          # These are some book-keeping variables and don't
                          # contain relevant data.
                          -dplyr::matches(match = "^(In)|(Dip)|(DateLoaded)")) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  header <- dplyr::select(.data = header,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  # In some cases (due to bad DIMA defaults) empty rows may exist in DIMA data.
  # This finds all the records where there were no valid ratings and drops them.
  detail <- detail[which(apply(X = dplyr::select(.data = detail,
                                                 tidyselect::matches(match = "^Rating\\d+$")),
                               MARGIN = 1,
                               FUN = function(X){
                                 !all(is.na(as.vector(X)))
                               })), ]

  if (auto_qc_warnings) {
    if (verbose) {
      message("Running automatic QC checks for duplicated or orphaned records.")
    }
    auto_qc_warning(header_data = header,
                    detail_data = detail,
                    uid_variables = list(header = c("PrimaryKey",
                                                    "RecKey"),
                                         detail = c("PrimaryKey",
                                                    "RecKey",
                                                    "BoxNum")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }

  # Convert to tall format
  gathered <- tidyr::gather(detail,
                            key = variable,
                            value = value,
                            -PrimaryKey, -BoxNum, -RecKey,
                            na.rm = TRUE) |>
    dplyr::filter(.data = _,
                  value != "")

  detail_tall <- tidyr::pivot_longer(data = detail,
                                     cols = -tidyselect::all_of(c("PrimaryKey",
                                                                  "RecKey",
                                                                  "BoxNum")),
                                     names_to = c("variable",
                                                  "Position"),
                                     # This is a goofy regex, but the first
                                     # group, (^.*), will snag everything
                                     # prior to the second group,
                                     # ((?<=[A-z])\\d+$), which finds all the
                                     # digits between the last letter and the
                                     # end of the string. That first group
                                     # is the variable and the second is the
                                     # variable position in the sampling box.
                                     names_pattern = "(^.*)((?<=[A-z])\\d+$)",
                                     values_to = "value",
                                     # We have to coerce everything to character to
                                     # accommodate the cover values which are
                                     # character strings.
                                     values_transform = as.character,
                                     values_drop_na = TRUE) |>
    dplyr::filter(.data = _,
                  value != "",
                  # Correctly remove non-hydrophobic records with 0 values
                  # and hydrophobic records with non-zero values
                  !(variable == "Hydro" & value != "0"),
                  !(variable != "Hydro" & value == "0")) |>
    dplyr::distinct()

  # This will make things wider. We can't just use tidyr::pivot_wider() because
  # we don't have unique identifiers for the values which means that the value
  # variable would contain a list, which is unhelpful for how people want to use
  # these. So we'll make a list and join them together.
  # The lapply() works through the values in "variable" (Rating, Veg, etc.) and
  # grabs only the records with identical "variable" values. It then renames the
  # "value" variable to use whatever the "variable" value is for that set of
  # data and drops the "variable" variable.
  detail_tidy <- lapply(X = unique(detail_tall$variable),
                        detail_tall = detail_tall,
                        FUN = function(X, detail_tall){
                          # Renaming the "value" variable and then
                          # dropping the "variable" variable.
                          dplyr::filter(.data = detail_tall,
                                        variable == X) |>
                            # A little clunky, but this way we
                            # don't have to hardcode any
                            # "variable" values.
                            dplyr::rename(.data = _,
                                          tidyselect::all_of(setNames(object = c("value"),
                                                                      nm = X))) |>
                            dplyr::select(.data = _,
                                          -variable)
                        }) |>
    # The purrr::reduce() then binds all those together according to the
    # identifying variables. The use of a full_join() makes sure we don't drop
    # anything that had incomplete data. Yet.
    purrr::reduce(.x = _,
                  .f = dplyr::full_join,
                  by = c("RecKey",
                         "PrimaryKey",
                         "Position",
                         "BoxNum")) |>
    #However, there are likely going to be records in the source data where
    # there were no ratings but somehow there were other kinds of values, so
    # those get dropped.
    dplyr::filter(.data = _,
                  !is.na(Rating)) |>
    # And the last bit is coercing things to numeric.
    dplyr::mutate(.data = _,
                  dplyr::across(.cols = -tidyselect::all_of(c("RecKey",
                                                              "PrimaryKey")),
                                # Can't trust the variables to be coercible to
                                # numeric without introducing NAs. Even though
                                # that'd be possible in a pristine data set,
                                # you'll probably never have one. So, we check
                                # to see if coercion does violence and only
                                # coerce variables we won't damage.
                                .fns = ~ if(any(suppressWarnings(is.na(as.numeric(.x))))) {
                                  .x
                                } else {
                                  as.numeric(.x)
                                }))

  # Merge soil stability detail and header tables
  soil_stability_tall <- suppressWarnings(dplyr::left_join(x = header,
                                                           y = detail_tidy,
                                                           # Not enforcing this relationship
                                                           # because there may be duplicate
                                                           # records and the user can go ahead
                                                           # with this anyway.
                                                           # relationship = "one-to-many",
                                                           by = c("RecKey",
                                                                  "PrimaryKey")))

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

#' export gather_soil_stability_survey123
#' rdname gather_soil_stability
# gather_soil_stability_survey123 <- function(dsn = NULL,
#                                             SoilStability_0 = NULL) {
#
#
#   if(!is.null(SoilStability_0)){
#     soil_stability_detail = SoilStability_0
#   } else if (!is.null(dsn)){
#     if (!file.exists(dsn)) {
#       stop("dsn must be a valid filepath to a geodatabase containing tblSoilStabDetail and tblSoilStabHeader")
#     }
#     soil_stability_detail <-
#       suppressWarnings(sf::st_read(dsn,
#                                    layer = "tblSoilStabDetail",
#                                    stringsAsFactors = FALSE, quiet = T
#       ))
#
#     soil_stability_header <-
#       suppressWarnings(sf::st_read(dsn,
#                                    layer = "tblSoilStabHeader",
#                                    stringsAsFactors = FALSE, quiet = T
#       ))
#   } else {
#     stop("Supply either tblSoilStabDetail and tblSoilStabHeader, or a path to a GDB containing those tables")
#   }
#
#   # Turn PlotKey into PrimaryKey
#   soil_stability_detail$PrimaryKey <- soil_stability_detail$PlotKey
#
#   # Pare down columns to only necessary ones
#   soil_stability_detail <- soil_stability_detail %>%
#     dplyr::select(
#       PrimaryKey, Veg1:Hydro6
#     )
#
#   # In some cases (due to bad DIMA defaults) empty rows may exist in DIMA data. Remove them.
#   soil_stability_detail <- soil_stability_detail %>%
#     dplyr::filter(
#       !(is.na(Rating1) & is.na(Rating2) & is.na(Rating3) & is.na(Rating4) & is.na(Rating5) & is.na(Rating6)))
#
#   # remove orphaned records
#   soil_stability_detail <-
#     soil_stability_detail[!is.na(soil_stability_detail$PrimaryKey), ]
#
#   # If DBKey Key exists, remove it
#   if ("DBKey" %in% colnames(soil_stability_detail)) {
#     soil_stability_detail <- dplyr::select(soil_stability_detail, -DBKey)
#   }
#
#   gathered <- soil_stability_detail %>%
#     # Remove standard columns (In and Dip Times and Date Downloaded in DB)
#     dplyr::select(.,
#                   match = -dplyr::starts_with("In"),
#                   -dplyr::starts_with("Dip"),
#                   -dplyr::starts_with("DateLoaded")
#     ) %>%
#
#     # Convert to tall format
#     tidyr::gather(.,
#                   key = variable, value = value,
#                   -PrimaryKey, na.rm = TRUE
#     )
#
#   # Remove blank values
#   gathered <- subset(gathered, value != "")
#
#   # Separate numerical suffixes from field type
#   gathered$key <- stringr::str_extract(
#     string = gathered$variable,
#     pattern = "^[A-z]+"
#   )
#   gathered$Position <- stringr::str_extract(
#     string = gathered$variable,
#     pattern = "[0-9]+"
#   )
#
#   gathered <- subset(gathered, select = -c(variable, BoxNum))
#
#   # Remove Hydro = 0
#   gathered <- gathered %>% subset(!(key == "Hydro" & value != 0))
#
#   # Spread the gathered data so that Line, Rating, Vegetation,
#   # and Hydro are all different variables
#
#   soil_stability_detail_list <- lapply(
#     X = as.list(unique(gathered$key)),
#     FUN = function(k = as.list(unique(gathered$key)), df = gathered) {
#       test <- df[df$key == k, ] %>%
#         dplyr::mutate(id = 1:dplyr::n()) %>%
#         tidyr::spread(key = key, value = value) %>%
#         dplyr::select(-id)
#     }
#   )
#   # create a single tidy dataframe
#   soil_stability_detail_tidy <- purrr::reduce(
#     soil_stability_detail_list,
#     dplyr::full_join, by = c("RecKey", "PrimaryKey", "Position")
#   ) %>% unique()
#
#   soil_stability_detail_tidy$Rating <- soil_stability_detail_tidy$Rating %>%
#     as.numeric()
#
#   # Merge soil stability detail and header tables
#   soil_stability_tall <- dplyr::left_join(
#     soil_stability_header,
#     soil_stability_detail_tidy, by = c("RecKey", "PrimaryKey")
#   ) %>% dplyr::select_if(!names(.) %in% c(
#     'PlotKey', 'DateModified', 'FormType', 'DataEntry', 'DataErrorChecking', 'DateLoadedInDb'
#   )
#   )
#
#   # In some cases, the Hydro variable is lost. Add it back in
#   if (!"Hydro" %in% colnames(soil_stability_tall)){
#     soil_stability_tall$Hydro <- NA
#   }
#
#   # Return final merged file
#   return(soil_stability_tall)
# }
#

# Wrapper function for all soil stability gather functions
#' @export gather_soil_stability
#' @rdname gather_soil_stability
gather_soil_stability <- function(dsn = NULL,
                                  source,
                                  file_type = "gdb",
                                  tblSoilStabDetail = NULL,
                                  tblSoilStabHeader = NULL,
                                  SOILDISAG = NULL,
                                  autoQC = TRUE
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

  # Drop rows with no data
  soil_stability <- soil_stability %>%
    dplyr::filter(!(
                      is.na(Position) &
                      is.na(Rating) &
                      is.na(Veg)
    ))

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    soil_stability <- soil_stability %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "soilstab")
  }

  return(soil_stability)
}
