#' Convert height data into a tall, tidy data frame
#'
#' @description Given wide format line-point intercept data, create a tall
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblLPIDetail and
#' tblLPIHeader (AIM/DIMA/TerrADat) or PASTUREHEIGHTS (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblLPIHeader Dataframe of the data structure tblLPIHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblLPIDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblLPIDetail Dataframe of the data structure tblLPIDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblLPIHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param PASTUREHEIGHTS Dataframe of the data structure PASTUREHEIGHTS from the
#' LMF/NRI database; alternately provide \code{dsn}.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if \code{PASTUREHEIGHTS} is provided.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_height
#' @family <gather>
#' @return A tall data frame containing the data from the height measurements.
#' @examples
#' gather_height(dsn = "Path/To/AIM_Geodatabase.gdb",
#'               source = "AIM")
#' gather_height(dsn = "Path/To/LMF_Geodatabase.gdb",
#'               source = "LMF")
#'
#' aim_lpidetail <- read.csv("Path/To/tblLPIDetail.csv")
#' aim_lpiheader <- read.csv("Path/To/tblLPIHeader.csv")
#' gather_height(source = "AIM",
#'               tblLPIDetail = aim_lpidetail,
#'               tblLPIHeader = aim_lpiheader)
#'
#' lmf_heights <- read.csv("Path/To/PASTUREHEIGHTS.csv")
#' gather_height(source = "LMF",
#'               PASTUREHEIGHTS = lmf_heights)

## Gather Height Data
#' @export gather_height_terradat
#' @rdname gather_height
gather_height_terradat <- function(dsn = NULL,
                                   tblLPIDetail= NULL,
                                   tblLPIHeader = NULL,
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
                         "DBKey",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")

  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(tblLPIDetail) & !is.null(tblLPIHeader)) {
    if (verbose) {
      if (!is.null(dsn)) {
        message("Using the provided data frames. The provided dsn value is being ignored.")
      }
    }
    detail <- tblLPIDetail
    header <- tblLPIHeader
  } else if(!is.null(dsn)){
    if (verbose) {
      message("Attempting to use the provided dsn value.")
    }
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
    }

    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblLPIDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))
    header <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblLPIHeader",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))
  } else {
    stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing tables with those names.")
  }

  # Clean these up!
  # We're deliberating excluding some variables in both tables here. No sense
  # in bogging down the munging with extraneous things we'll never use.
  detail <- dplyr::select(.data = detail,
                          PrimaryKey, RecKey,
                          PointLoc, PointNbr,
                          tidyselect::matches(match = "(Woody)|(Herbaceous)|(LowerHerb)$")) |>
    dplyr::distinct()


  header <- dplyr::select(.data = header,
                          PrimaryKey,
                          LineKey:CheckboxLabel,
                          # tidyselect::matches(match = "DBKey"),
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()


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
                                                    "PointNbr")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }

  # Warn about introducing NAs by coercion.
  nas_by_coercion <- sapply(X = c(woody = "Woody",
                                  herbaceous = "Herbaceous",
                                  'lower herbaceous' = "LowerHerb"),
                            detail = detail,
                            FUN = function(X, detail){
                              if (class(detail[[paste0("Height", X)]]) %in% c("integer",
                                                                              "numeric")) {
                                0
                              } else {
                                tidyr::replace_na(data = detail[[paste0("Height", X)]],
                                                  replace = "placeholder") |>
                                  as.numeric() |>
                                  suppressWarnings() |>
                                  is.na() |>
                                  sum()
                              }
                            })
  nas_by_coercion <- nas_by_coercion[nas_by_coercion > 0]
  if (length(nas_by_coercion) > 0) {
    warning(paste0("There are non-numeric values in at least one height variable in tblLPIDetail. There will be ",
                   sum(nas_by_coercion),
                   " invalid height values replaced with NA across the following height types: ",
                   paste(names(nas_by_coercion),
                         collapse = ", "),
                   ". Any records with a height value of NA will be dropped from the output during processing."))
  }



  # There are three height types that we're going to be working with here, so
  # we'll deal with each independently and then mash them together with
  # dplyr::bind_rows().
  # This approach with the lapply() and renaming makes more sense than doing
  # pivoting with tidyr functions because the value types are mixed (e.g., the
  # species are stored as character strings but heights are numeric) so we can't
  # put them all in one data frame variable when we pivot the data to a long
  # format.
  lpi_heights_tall <- lapply(X = c(woody = "Woody",
                                   herbaceous = "Herbaceous",
                                   lower_herbaceous = "LowerHerb"),
                             detail = detail,
                             FUN = function(X, detail){
                               dplyr::select(.data = detail,
                                             PrimaryKey, RecKey,
                                             PointLoc, PointNbr,
                                             tidyselect::ends_with(match = X)) |>
                                 dplyr::rename_with(.data = _,
                                                    .cols = tidyselect::ends_with(match = X),
                                                    .fn = ~ stringr::str_extract(string = .x,
                                                                                 pattern = paste0(".+(?=", X, "$)"))) |>
                                 dplyr::mutate(.data = _,
                                               # We're going to suppress
                                               # warnings about introducing
                                               # NAs in this coercion because we
                                               # already warned the user above.
                                               Height = suppressWarnings(as.numeric(Height)),
                                               type = dplyr::case_when(X == "LowerHerb" ~ "lower.herbaceous",
                                                                       .default = tolower(X)),
                                               GrowthHabit_measured = dplyr::case_when(X == "Woody" ~ "Woody",
                                                                                       X %in% c("Herbaceous",
                                                                                                "LowerHerb") ~ "NonWoody",
                                                                                       .default = NA)) |>
                                 dplyr::filter(.data = _,
                                               !is.na(Height))
                             }) |>
    dplyr::bind_rows()

  lpi_height <- suppressWarnings(dplyr::left_join(x = lpi_heights_tall,
                                                  y = header,
                                                  # As per usual, not enforcing
                                                  # the relationship because
                                                  # we're letting the users move
                                                  # ahead with not-quite-clean
                                                  # data.
                                                  # relationship = "many-to-one",
                                                  by = c("PrimaryKey",
                                                         "RecKey")))

  # Make sure that the values in Species are either at least possibly a species
  # code or NA
  lpi_height <- dplyr::mutate(.data = lpi_height,
                              Species = dplyr::case_when(stringr::str_detect(string = Species,
                                                                             pattern = "[[:digit:]]|[[:alpha:]]") ~ Species,
                                                         .default = NA))

  # Make sure we don't have duplicates because that could totally happen,
  # depending on the data quality.
  lpi_height <- dplyr::distinct(.data = lpi_height)

  # Output the woody/herbaceous level data
  return(lpi_height)
}

#' @export gather_height_lmf
#' @rdname gather_height

# Gather Height for LMF/NRI
gather_height_lmf <- function(dsn = NULL,
                              file_type = "gdb",
                              PASTUREHEIGHTS = NULL) {
  #### Reading and cleanup #####################################################
  if(!is.null(PASTUREHEIGHTS)){
    vegheight <- PASTUREHEIGHTS
  } else if (!is.null(dsn)) {
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing PASTUREHEIGHTS")
    }

    # Read in the data as .txt or .gdb
    vegheight <- switch(file_type,
                        "gdb" = {
                          suppressWarnings(sf::st_read(dsn,
                                                       layer = "PASTUREHEIGHTS",
                                                       stringsAsFactors = FALSE,
                                                       quiet = T
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
      ) |>
        dplyr::pull(FIELD.NAME) |>
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




  } else {
    stop("Supply either PASTUREHEIGHTS or a path to a gdb containing that table")
  }

  ##### Making sure these are distinct records ---------------------------------
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
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
                         "FormType",
                         "DBKey")

  vegheight <- dplyr::select(.data = vegheight,
                             -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

  ##### Dealing with intersecting transects ------------------------------------
  # The arrangement of the transects for an LMF point crosses in the middle of
  # the transects (point 75) and so that intersection gets recorded twice, once
  # per transect. We'll drop the 75th record on the northeast-southwest transect
  # but we also want to warn the user that it's happening to any situations
  # where the assumption that they're identical is violated.
  duplicated_75mark_indices <- dplyr::filter(.data = vegheight,
                                             DISTANCE == 75) |>
    dplyr::select(.data = _,
                  -TRANSECT) |>
    duplicated(x = _)
  vegheight_75mark <- dplyr::filter(.data = vegheight,
                                    DISTANCE == 75)
  vegheight_75mark[["duplicated"]] <- duplicated_75mark_indices
  vegheight_75mark_summary <- dplyr::summarize(.data = vegheight_75mark,
                                               .by = PrimaryKey,
                                               n_records = dplyr::n(),
                                               has_duplicate = any(duplicated))
  if (any(!vegheight_75mark_summary$has_duplicate)) {
    warning(paste0("There are ", sum(!vegheight_75mark_summary$has_duplicate),
                   " plots where the height records at the 75th sampling locations on the two transects are not identical to each other despite being the intersection of those transects. The records associated with the 'nesw' transects will still be dropped for these plots."))
  }

  vegheight <- dplyr::filter(.data = vegheight,
                             !(TRANSECT == "nesw" & DISTANCE == 75))

  #### Reformatting and harmonizing ############################################
  # Most of the heavy lifting!
  # This will get us to the point where there's a separate record for each type
  # of measurement (woody and nonwoody) but there's a little more to adjust
  # after that.
  data_long <- dplyr::select(.data = vegheight,
                             PrimaryKey,
                             LineKey = TRANSECT,
                             PointNbr = DISTANCE,
                             tidyselect::matches(match = "HEIGHT"),
                             tidyselect::matches(match = "PLANT")) |>
    # This splits the species and the heights into separate records, but that's
    # actually fine for now.
    tidyr::pivot_longer(data = _,
                        cols = -tidyselect::all_of(c("PrimaryKey",
                                                     "LineKey",
                                                     "PointNbr")),
                        names_to = "variable",
                        values_to = "value") |>
    # This removes all the records where there were no species or no heights.
    # "None" is fine because we'll have a measure of 0 there.
    dplyr::filter(.data = _,
                  !(value %in% c(NA,
                                 ""))) |>
    dplyr::mutate(.data = _,
                  # Add in the type and GrowthHabit_measured variables based on
                  # whether the variable name was prefixed with a W, assuming
                  # that if there's not a W that meant it was nonwoody.
                  GrowthHabit_measured = dplyr::case_when(stringr::str_detect(string = variable,
                                                                              pattern = "^W") ~ "Woody",
                                                          .default = "NonWoody"),
                  type = dplyr::case_when(GrowthHabit_measured == "Woody" ~ "woody",
                                          .default = "herbaceous"),
                  # Drop the prefixes from these values so that we can pivot
                  # wider to combine the record types.
                  variable = stringr::str_extract(string = variable,
                                                  pattern = "PLANT|HEIGHT")) |>
    tidyr::pivot_wider(data = _,
                       names_from = "variable",
                       values_from = "value")

  # Now we split the HEIGHT variable into one with the height and one with the
  # units.
  data_long <- dplyr::mutate(.data = data_long,
                             # The pattern used will find numbers with decimal places
                             # but they shouldn't be in there in the first place.
                             Height = stringr::str_extract(string = HEIGHT,
                                                           pattern = "^\\d+\\.?\\d*") |>
                               as.numeric() |>
                               tidyr::replace_na(data = _,
                                                 replace = 0),
                             HeightUOM = stringr::str_extract(string = HEIGHT,
                                                              pattern = "[a-z]{2}$")) |>
    dplyr::mutate(.data = _,
                  # Convert to centimeters!!!
                  Height = dplyr::case_when(HeightUOM %in% c("in") ~ Height * 2.54,
                                            HeightUOM %in% c("ft") ~ Height * 2.54 * 12,
                                            .default = Height),
                  HeightUOM = "cm") |>
    dplyr::select(.data = _,
                  -HEIGHT)

  # We'll also replace any "None" or "" values in PLANT with NA
  data_long <- dplyr::mutate(.data = data_long,
                             Species = dplyr::case_when(PLANT %in% c("", "None") ~ NA,
                                                        .default = PLANT))

  # Return only the unique records and with the variables ordered how we'd like
  dplyr::select(.data = data_long,
                tidyselect::all_of(c("PrimaryKey",
                                     "LineKey",
                                     "PointNbr",
                                     "Species",
                                     "Height",
                                     "HeightUOM",
                                     "type",
                                     "GrowthHabit_measured"))) |>
    dplyr::distinct()
}

#' export gather_height_survey123
#' rdname gather_height
# gather_height_survey123 <- function(LPI_0,
#                                     LPIDetail_1) {
#
#   # This code copy-and-pasted from gather_height_terradat
#   lpi_header <- LPI_0
#   lpi_detail <- LPIDetail_1
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- lpi_header$PlotKey[duplicated(lpi_header$PlotKey) & duplicated(LPIHeader123$LineKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Survery123 uses GlobalID to link data, not PrimaryKey. Bring in PrimaryKey from PlotKey
#   lpi_header$PrimaryKey <- lpi_header$PlotKey
#   lpi_detail <- dplyr::left_join(lpi_detail, lpi_header %>% dplyr::select(GlobalID, PrimaryKey), by = c("ParentGlobalID" = "GlobalID"))
#
#   if (any(colnames(lpi_header) %in% "DBKey")) {
#     levels <- rlang::quos(PrimaryKey, "DBKey")
#   } else {
#     levels <- rlang::quos(PrimaryKey)
#   }
#
#   # we only want to carry a subset of the lpi_header fields forward
#   # lpi_header <- dplyr::select(lpi_header, !!!levels, PlotID:CheckboxLabel)
#
#   # Add null DBKey column if not present
#   if(!("DBKey" %in% colnames(lpi_header))) lpi_header$DBKey <- NA
#   if(!("DBKey" %in% colnames(lpi_detail))) lpi_detail$DBKey <- NA
#
#   lpi_height_tall_woody <- dplyr::select(
#     .data = lpi_detail,
#     !!!levels,
#     PointLoc,
#     PointNbr,
#     RecKey,
#     DBKey,
#     dplyr::matches("Woody$")
#   ) %>% dplyr::mutate(type = "woody")
#   # Strip out the extra name stuff so woody and herbaceous variable names match.
#   names(lpi_height_tall_woody) <- stringr::str_replace_all(
#     string = names(lpi_height_tall_woody),
#     pattern = "Woody$",
#     replacement = ""
#   )
#   # Add observed growth habit field
#   lpi_height_tall_woody$GrowthHabit_measured <- "Woody"
#
#   # Herbaceous height
#   lpi_height_tall_herb <- dplyr::select(
#     .data = lpi_detail,
#     !!!levels,
#     PointLoc,
#     PointNbr,
#     RecKey,
#     DBKey,
#     dplyr::matches("Herbaceous$")
#   ) %>% dplyr::mutate(type = "herbaceous")
#   names(lpi_height_tall_herb) <- stringr::str_replace_all(
#     string = names(lpi_height_tall_herb),
#     pattern = "Herbaceous$",
#     replacement = ""
#   )
#   # Add observed growth habit field
#   lpi_height_tall_herb$GrowthHabit_measured <- "NonWoody"
#
#   # Merge all gather types together
#   lpi_height <- rbind(
#     lpi_height_tall_woody,
#     lpi_height_tall_herb
#   )
#
#   lpi_height <- lpi_height %>%
#     dplyr::full_join(x = ., y = lpi_header, by = c("PrimaryKey", "DBKey")) %>%
#     subset(., !is.na(Height))
#
#   # Add NA to fields with no species
#   lpi_height$Species[!grepl(pattern = "[[:digit:]]|[[:alpha:]]", lpi_height$Species)] <- NA
#
#   # Remove orphaned records and duplicates, if they exist
#   lpi_height <- unique(lpi_height)
#   lpi_height <- lpi_height[!is.na(lpi_height$PrimaryKey), ]
#
#   # Make sure height is a numeric field
#   lpi_height$Height <- suppressWarnings(as.numeric(lpi_height$Height))
#
#   # final drop
#   lpi_height <- lpi_height %>%
#     dplyr::select(
#       "PrimaryKey", "DBKey", "PointLoc", "PointNbr", "RecKey", "Height",
#       "Species", "Chkbox", "type", "GrowthHabit_measured", "LineKey", "FormDate",
#       "Direction",
#       ## These methods data are not in Survey123
#       # "Measure",
#       # "LineLengthAmount",
#       # "SpacingIntervalAmount",
#       # "SpacingType",
#       # "HeightOption",
#       # "HeightUOM",
#       # "ShowCheckbox",
#       # "CheckboxLabel"
#     )
#
#
#   # Output the woody/herbaceous level data
#   return(lpi_height)
# }
#

#' Gather Height wrapper for all data types
#' @export gather_height
#' @rdname gather_height

gather_height <- function(dsn = NULL,
                          file_type = "gdb",
                          source,
                          tblLPIDetail = NULL,
                          tblLPIHeader = NULL,
                          PASTUREHEIGHTS = NULL,
                          autoQC = TRUE#,
                          # LPI_0 = NULL,
                          # LPIDetail_1 = NULL
) {
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    height <- gather_height_terradat(
      dsn = dsn,
      tblLPIHeader = tblLPIHeader,
      tblLPIDetail = tblLPIDetail
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    height <- gather_height_lmf(
      dsn = dsn, file_type = file_type,
      PASTUREHEIGHTS = PASTUREHEIGHTS
    )
    # } else if(toupper(source) %in% c("SURVEY123")){
    #   height <- gather_height_survey123(
    #     LPI_0 = LPI_0,
    #     LPIDetail_1 = LPIDetail_1
    #   )
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # height$source <- toupper(source)
  height$source <- source

  if("sf" %in% class(height)) height <- sf::st_drop_geometry(height)

  if (any(class(height) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(height)[do.call(rbind, vapply(height,
                                                       class))[, 1] %in% c("POSIXct", "POSIXt")]
    height <- dplyr::mutate_at(height, dplyr::vars(change_vars),
                               dplyr::funs(as.character))
  }

  height <- dplyr::select(.data = height,
                          PrimaryKey, LineKey,
                          tidyselect::everything())

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    height <- height %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "height")
  }

  # Output height
  return(height)
}
