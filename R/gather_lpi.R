#' Convert line-point intercept (LPI) data into a tall, tidy data frame
#'
#' @description Given wide format line-point intercept data, create a tall
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblLPIDetail and
#' tblLPIHeader (AIM/DIMA/TerrADat) or PINTERCEPT (LMF/NRI).
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
#' @param PINTERCEPT Dataframe of the data structure PINTERCEPT from the LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when source
#' is LMF or NRI; alternately provide dsn.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if PINTERCEPT is provided.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_lpi
#' @family <gather>
#' @return A tall data frame containing the data from the LPI pin intercepts
#' @examples
#' gather_lpi(dsn = "Path/To/AIM_Geodatabase.gdb",
#'            source = "AIM")
#' gather_lpi(dsn = "Path/To/LMF_Geodatabase.gdb",
#'            source = "LMF")
#'
#' aim_lpidetail <- read.csv("Path/To/tblLPIDetail.csv")
#' aim_lpiheader <- read.csv("Path/To/tblLPIHeader.csv")
#' gather_lpi(source = "AIM",
#'            tblLPIDetail = aim_lpidetail,
#'            tblLPIHeader = aim_lpiheader)
#'
#' lmf_pintercept <- read.csv("Path/To/PINTERCEPT.csv")
#' gather_lpi(source = "LMF",
#'            RANGEHEALTH = lmf_pintercept)

## Function to make tall format of LPI data from TerrADat
#' @export gather_lpi_terradat
#' @rdname gather_lpi
gather_lpi_terradat <- function(dsn = NULL,
                                tblLPIDetail = NULL,
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
  detail <- dplyr::select(.data = detail,
                              -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

  header <- dplyr::select(.data = header,
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

  # Make a tall data frame with the hit codes by layer.
  lpi_hits_tall <- dplyr::mutate(.data = detail,
                                 # Make sure we don't have factors in play.
                                 dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                               .fns = ~ as.character(.x))) |>
    # Get just the variables we need for this particular data frame.
    dplyr::select(.data = _,
                  PrimaryKey, RecKey,
                  PointLoc, PointNbr,
                  ShrubShape,
                  TopCanopy,
                  tidyselect::matches(match = "^Lower\\d$"),
                  SoilSurface) |>
    # Pivot it to be long (tall, in the older terminology).
    tidyr::pivot_longer(data = _,
                        # All those messy lower layer variable names are
                        # sandwiched between TopCanopy and SoilSurface, which
                        # makes it easy to do this and still snag them all.
                        cols = TopCanopy:SoilSurface,
                        names_to = "layer",
                        values_to = "code") |>
    # Dropping the values we can't work with because no valid hit was recorded.
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey),
                  !is.na(RecKey),
                  !code %in% c(NA, "N", "None", "")) |>
    dplyr::distinct(.data = _)

  # test <- dplyr::full_join(x = dplyr::mutate(lpi_hits_tall,
  #                                            original = TRUE),
  #                          y = dplyr::mutate(lpi_hits_tall_test,
  #                                            test = TRUE))
  # any(is.na(test$original))
  # any(is.na(test$test))

  # Make a tall data frame the checkbox status by layer.
  lpi_chkbox_tall <- dplyr::mutate(.data = detail,
                                   # Make sure we don't have factors in play.
                                   dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                                 .fns = ~ as.character(.x))) |>
    # Get just the variables we need for this particular data frame.
    dplyr::select(.data = _,
                  PrimaryKey, RecKey,
                  PointLoc, PointNbr,
                  # We only want the chkbox values for the top, soil, and
                  # numbered layers. This will exclude the woody and herbaceous
                  # ones.
                  tidyselect::matches(match = "^Chkbox(Top|Soil|Lower\\d)$")) |>
    # Pivot it to be long (tall, in the older terminology).
    tidyr::pivot_longer(data = _,
                        cols = tidyselect::matches(match = "^Chkbox"),
                        names_to = "layer",
                        # Removing the prefix so that these values will match
                        # the ones in our other tall data frame created above.
                        names_prefix = "^Chkbox",
                        values_to = "chckbox") |>
    # Remove the records with invalid checkbox values.
    dplyr::filter(.data = _,
                  chckbox %in% c(1, 0)) |>
    # And adjusting so that the non-numbered layers match those values we expect
    dplyr::mutate(.data = _,
                  layer = dplyr::case_when(layer == "Top" ~ "TopCanopy",
                                           layer == "Soil" ~ "SoilSurface",
                                           .default = layer)) |>
    dplyr::distinct(.data = _)

  # test <- dplyr::full_join(x = dplyr::mutate(lpi_chkbox_tall,
  #                                            original = TRUE),
  #                          y = dplyr::mutate(lpi_chkbox_tall_test,
  #                                            test = TRUE))
  # any(is.na(test$original))
  # any(is.na(test$test))


  # Print update because this function can take a while
  if (verbose) {
    message("Merging the header and detail tables")
  }

  # Join the header information to the hit and checkbox data.
  # The suppressWarnings() and lack of defined relationships in the joins are to
  # allow the user to run this with data that have not been adequately cleaned.
  lpi_tall <- suppressWarnings(dplyr::left_join(x = lpi_hits_tall,
                                                y = lpi_chkbox_tall,
                                                # relationship = "one-to-one",
                                                by = c("PrimaryKey", "RecKey",
                                                       "PointLoc", "PointNbr",
                                                       "layer")) |>
                                 dplyr::left_join(x = dplyr::select(.data = header,
                                                                    LineKey:CheckboxLabel,
                                                                    PrimaryKey, DBKey),
                                                  y = _,
                                                  # relationship = "one-to-many",
                                                  by = c("PrimaryKey", "RecKey")))

  # We want to coerce dates into character strings.
  # Find variables with a date class.
  date_vars <- which(sapply(X = names(lpi_tall),
                            lpi_tall = lpi_tall,
                            FUN = function(X, lpi_tall){
                              any(c("POSIXct", "POSIXt") %in% class(lpi_tall[[X]]))
                            }))
  # And then coerce the date values in those variables to strings.
  lpi_tall <- dplyr::mutate(.data = lpi_tall,
                            dplyr::across(.cols = tidyselect::all_of(date_vars),
                                          .fns = as.character))


  # Remove some of the header variables
  lpi_tall <- dplyr::select(.data = lpi_tall,
                            -tidyselect::any_of(c("DateModified",
                                                  "FormType",
                                                  "DataEntry",
                                                  "DataErrorChecking",
                                                  "DateVisited")))


  lpi_tall
}

#' @export gather_lpi_lmf
#' @rdname gather_lpi

gather_lpi_lmf <- function(dsn = NULL,
                           file_type = "gdb",
                           PINTERCEPT = NULL) {
  #### Reading and cleanup #####################################################
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(PINTERCEPT)) {
    pintercept <- PINTERCEPT
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a database containing PINTERCEPT")
    }
    # Read  PINTERCEPT table in .txt or .gdb or from a preformatted csv
    pintercept <- switch(file_type,
                         "gdb" = {
                           sf::st_read(dsn = dsn,
                                       layer = "PINTERCEPT",
                                       stringsAsFactors = FALSE,
                                       quiet = TRUE) |>
                             suppressWarnings()
                         },
                         "txt" = {
                           utils::read.table(file = file.path(dsn,
                                                              "pintercept.txt"),
                                             stringsAsFactors = FALSE,
                                             strip.white = TRUE,
                                             header = FALSE,
                                             sep = "|")
                         },
                         "csv" = {
                           read.csv(file = dsn,
                                    header = TRUE,
                                    stringsAsFactors = FALSE)
                         })

    # if it is in a text file, there are no field names assigned.
    if (file_type == "txt") {
      colnames <- terradactyl::nri.data.column.explanations$FIELD.NAME[
        terradactyl::nri.data.column.explanations$TABLE.NAME == "PINTERCEPT"
      ]

      colnames <- colnames[1:ncol(pintercept)] %>% subset(!is.na(.))
      names(pintercept) <- colnames
    }
  } else {
    stop("Supply either PINTERCEPT or the path to a GDB containing that table")
  }

  # Sometimes NAs might be introduced as variable names, but we can make sure to
  # drop those.
  pintercept <- pintercept[, !is.na(colnames(pintercept))]

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

  pintercept <- dplyr::select(.data = pintercept,
                              -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

  ##### Dealing with intersecting transects ------------------------------------
  # The arrangement of the transects for an LMF point crosses in the middle of
  # the transects (point 75) and so that intersection gets recorded twice, once
  # per transect. We'll drop the 75th record on the northeast-southwest transect
  # but we also want to warn the user that it's happening to any situations
  # where the assumption that they're identical is violated.
  duplicated_75mark_indices <- dplyr::filter(.data = pintercept,
                                             MARK == 75) |>
    dplyr::select(.data = _,
                  -TRANSECT) |>
    duplicated(x = _)
  pintercept_75mark <- dplyr::filter(.data = pintercept,
                                     MARK == 75)
  pintercept_75mark[["duplicated"]] <- duplicated_75mark_indices
  pintercept_75mark_summary <- dplyr::summarize(.data = pintercept_75mark,
                                                .by = PrimaryKey,
                                                n_records = dplyr::n(),
                                                has_duplicate = any(duplicated))
  if (any(!pintercept_75mark_summary$has_duplicate)) {
    warning(paste0("There are ", sum(!pintercept_75mark_summary$has_duplicate),
                   " plots where the LPI records at the 75th sampling locations on the two transects are not identical to each other despite being the intersection of those transects. The records associated with the 'nesw' transects will still be dropped for these plots."))
  }

  pintercept <- dplyr::filter(.data = pintercept,
                              !(TRANSECT == "nesw" & MARK == 75))

  ##### Harmonizing with expected values and variable names --------------------
  # We don't need all the extra LMF-internal variables after this point, so
  # we'll pare down and rename here.
  pintercept <- dplyr::select(.data = pintercept,
                              PrimaryKey,
                              LineKey = TRANSECT,
                              PointNbr = MARK,
                              tidyselect::matches(match = "^HIT\\d$"),
                              BASAL,
                              NONSOIL,
                              ShrubShape = SAGEBRUSH_SHAPE)

  # The way soil surface hits are recorded needs to be standardized.
  pintercept <- dplyr::mutate(.data = pintercept,
                              # When there's no basal hit recorded and
                              # there's nothing in the NONSOIL variable,
                              # that means that there was a surface hit on
                              # soil. Otherwise we'll turn all the empty values
                              # to NA to make it easier to merge BASAL and
                              # NONSOIL into a single SoilSurface variable.
                              NONSOIL = dplyr::case_when(BASAL %in% c("None") & NONSOIL %in% c("", NA) ~ "S",
                                                         NONSOIL %in% c("") ~ NA,
                                                         .default = NONSOIL),
                              # If a basal hit was recorded over a NONSOIL
                              # value of "BR" then the BASAL value should
                              # actually be NA. Also, all "None" values should
                              # be NA for the next step. Otherwise, leave the
                              # BASAL values alone.
                              BASAL = dplyr::case_when(!(BASAL %in% c("None")) & NONSOIL %in% c("BR") ~ NA,
                                                       BASAL %in% c("None") ~ NA,
                                                       .default = BASAL)) |>
    # The BASAL and NONSOIL variables can be combined into SoilSurface and
    # dropped.
    dplyr::mutate(.data = _,
                  SoilSurface = dplyr::coalesce(BASAL,
                                                NONSOIL)) |>
    dplyr::select(.data = _,
                  -BASAL,
                  -NONSOIL)

  # We also need to standardize the way that sagebrush shrub shapes are recorded
  # because they're numeric values but we need them to be characters which
  # represent the shape where 1 means columnar, 2 means spreading, 3 means
  # mixed, and 0 means there was no shape to record.
  pintercept <- dplyr::mutate(.data = pintercept,
                              # I don't totally trust that this'll be numeric,
                              # so we'll check for a numeric and a character.
                              ShrubShape = dplyr::case_when(ShrubShape %in% c(1, "1") ~ "C",
                                                            ShrubShape %in% c(2, "2") ~ "S",
                                                            ShrubShape %in% c(3, "3") ~ "M",
                                                            .default = NA))

  # We need to rename the HIT variables to match the expectations.
  # Get the appropriate variable names
  hit_layer_rename_vector <- stringr::str_extract(string = names(pintercept),
                                                  pattern = "^HIT\\d+$") |>
    na.omit(object = _)
  # Make sure that they're in ascending order and have the new names associated.
  hit_layer_rename_vector <- hit_layer_rename_vector[stringr::str_extract(string = hit_layer_rename_vector,
                                                                          pattern = "\\d+") |>
                                                       as.numeric() |>
                                                       order()] |>
    setNames(object = _,
             nm = c("TopCanopy",
                    paste0("Lower",
                           seq_len(length(hit_layer_rename_vector) - 1))))
  # And use that vector to rename the variables.
  pintercept <- dplyr::rename(.data = pintercept,
                              tidyselect::all_of(hit_layer_rename_vector))

  # Make sure that we don't have any dates as POSITX values and coerce them into
  # character. This shouldn't be necessary, but just in case!
  pintercept <- dplyr::mutate(.data = pintercept,
                              dplyr::across(.cols = tidyselect::where(fn = ~ class(.x) %in% c("POSIXct", "POSIXt")),
                                            .fns = as.character))

  #### Pivoting to long ########################################################
  data_tall <- tidyr::pivot_longer(data = pintercept,
                                   cols = tidyselect::all_of(c(names(hit_layer_rename_vector),
                                                               "SoilSurface")),
                                   names_to = "layer",
                                   values_to = "code") |>
    # Remove all the non-records!
    dplyr::filter(.data = _,
                  !(code %in% c("")))

  # Return only the unique records remaining!
  dplyr::distinct(data_tall)
}

# Gather LPI data from NPS I&M networks
# currently not used
gather_lpi_nps <- function(dsn) {
  lpi_raw <- read.csv(dsn)

  # add plot metadata
  lpi_raw <- lpi_raw %>% dplyr::mutate(
    PrimaryKey = Unit_Plot,
    DBKey = dsn,
    FormDate = Start_Date,
    LineLengthAmount = 50,
    SpacingIntervalAmount = 0.5,
    SpacingType = "m",
    PointNbr = Point,
    LineKey = Transect,
    RecKey = paste(Unit_Plot, Visit_Year,
                   Transect,
                   sep = "_"
    )
  )
}

#' export gather_lpi_survey123
#' rdname gather_lpi
# gather_lpi_survey123 <- function(dsn = NULL,
#                                  LPI_0 = NULL,
#                                  LPIDetail_1 = NULL) {
#
#   # This code copy-and-paste from terradat gather
#   lpi_detail <- LPIDetail_1
#   lpi_header <- LPI_0
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- lpi_header$PlotKey[duplicated(lpi_header$PlotKey) & duplicated(lpi_header$LineKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Add null DBKey column if not present
#   if(!("DBKey" %in% colnames(lpi_header))) lpi_header$DBKey <- NA
#   if(!("DBKey" %in% colnames(lpi_detail))) lpi_detail$DBKey <- NA
#
#   # Survey123 uses GlobalID to join, and PlotKey becomes PrimaryKey
#   lpi_header$PrimaryKey <- lpi_header$PlotKey
#
#   lpi_detail <- dplyr::left_join(
#     x = lpi_detail,
#     y = lpi_header %>% dplyr::select(PrimaryKey, GlobalID),
#     by = c("ParentGlobalID" = "GlobalID")
#   ) %>%
#     dplyr::select(-"ParentGlobalID", -"GlobalID")
#
#
#   # Make a tall data frame with the hit codes by layer and the checkbox designation
#   lpi_hits_tall <- lpi_detail %>%
#     dplyr::mutate_if(is.factor, as.character) %>%
#     dplyr::select(
#       "PrimaryKey",
#       "PointLoc",
#       "PointNbr",
#       "RecKey",
#       "ShrubShape",
#       "TopCanopy",
#       "SoilSurface", dplyr::matches("^Lower")
#     ) %>%
#     tidyr::gather(
#       key = "layer",
#       value = "code",
#       "TopCanopy", "SoilSurface", dplyr::matches("^Lower")
#     )
#
#   # Remove all records where no hit was recorded (e.g., "None", "NA"
#
#   lpi_hits_tall <- dplyr::filter(
#     .data = lpi_hits_tall,
#     !is.na(code),
#     code != "",
#     code != "N",
#     code != "None",
#     !is.na(PrimaryKey),
#     !is.na(RecKey)
#   )
#
#
#   ## Make a tall data frame the checkbox status by layer
#
#   lpi_chkbox_tall <- lpi_detail %>%
#     dplyr::select(
#       "PrimaryKey",
#       "PointLoc",
#       "PointNbr",
#       "RecKey",
#       dplyr::matches("^Chkbox")
#     ) %>%
#     tidyr::gather(
#       key = "layer",
#       value = "chckbox",
#       dplyr::matches("^Chkbox")
#     )
#
#   # Remove Woody and Herbaceous Checkbox
#   lpi_chkbox_tall <- lpi_chkbox_tall[!(lpi_chkbox_tall$chckbox %in%
#                                          c(
#                                            "ChckboxWoody",
#                                            "ChckboxHerbaceous"
#                                          )), ]
#
#   ## Make the names in the layer variable match
#   lpi_chkbox_tall$layer <- gsub(lpi_chkbox_tall$layer,
#                                 pattern = "^Chkbox",
#                                 replacement = ""
#   )
#
#   lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Top"] <- "TopCanopy"
#   lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Soil"] <- "SoilSurface"
#
#   # Print update because this function can take a while
#   message("Merging LPI Header and LPI Detail tables")
#
#   # Merge checkbox and hit data as well as the header data
#   lpi_tall <- dplyr::left_join(
#     x = lpi_hits_tall,
#     y = lpi_chkbox_tall,
#     by = c("PrimaryKey", "PointLoc", "PointNbr", "RecKey", "layer")
#   ) %>%
#     dplyr::left_join(
#       x =
#
#         dplyr::select(
#         lpi_header,
#         "PrimaryKey",
#         "LineKey",
#         "DBKey",
#         "FormDate",
#         "Direction"#,
#         # "Measure", # Missing but necessary data
#         # "LineLengthAmount",
#         # "SpacingIntervalAmount",
#         # "SpacingType",
#         # "HeightOption",
#         # "HeightUOM",
#         # "ShowCheckBox",
#         # "CheckboxLabel",
#       ),
#       y = .,
#       by = c("PrimaryKey")
#     )
#
#   # Find date fields & convert to character
#   # Find fields that are in a Date structure
#   change_vars <- names(lpi_tall)[class(lpi_tall) %in%
#                                    c("POSIXct", "POSIXt")]
#
#   # Update fields
#   lpi_tall <- dplyr::mutate_at(
#     lpi_tall, dplyr::all_of(dplyr::vars(all_of(change_vars))),
#     list(as.character)
#   )
#
#
#   ## drops
#   lpi_tall <- lpi_tall %>% dplyr::select_if(!names(.) %in% c(
#
#     "DateModified", "FormType", "FormType",
#     "DataEntry", "DataErrorChecking", "DateVisited")
#   )
#
#
#   return(lpi_tall)
#   ## Output the list
# }



#' @export gather_lpi
#' @rdname gather_lpi

# Wrapper gather.lpi function
gather_lpi <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblLPIDetail = NULL,
                       tblLPIHeader = NULL,
                       PINTERCEPT = NULL#,
                       # LPI_0 = NULL,
                       # LPIDetail_1 = NULL
) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    lpi <- gather_lpi_terradat(dsn = dsn,
                               tblLPIDetail = tblLPIDetail,
                               tblLPIHeader = tblLPIHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    lpi <- gather_lpi_lmf(dsn = dsn,
                          file_type = file_type,
                          PINTERCEPT = PINTERCEPT)
    lpi$chckbox <- NA
    # } else if(toupper(source) == "SURVEY123"){
    #   lpi <- gather_lpi_survey123(LPI_0 = LPI_0,
    #                               LPIDetail_1 = LPIDetail_1)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # Add source field
  # lpi$source <- toupper(source)
  lpi$source <- source

  if("sf" %in% class(lpi)){
    lpi <- sf::st_drop_geometry(lpi)
  }

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  date_vars <- which(sapply(X = setNames(object = names(lpi),
                                         nm = names(lpi)),
                            lpi = lpi,
                            FUN = function(X, lpi){
                              any(c("POSIXct", "POSIXt") %in% class(lpi[[X]]))
                            }))

  # Update fields
  lpi <- dplyr::mutate(.data = lpi,
                            dplyr::across(.cols = tidyselect::all_of(date_vars),
                                          .fns = as.character))
  ## text field
  lpi$LineKey <- as.character(lpi$LineKey)

  # reorder so that primary key is leftmost column
  lpi <- lpi %>%
    dplyr::select(PrimaryKey, DBKey, LineKey, tidyselect::everything())

  return(lpi)
}
