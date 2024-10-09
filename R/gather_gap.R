#' Convert gap data into a tall, tidy data frame
#'
#' @description Given wide format gap data, create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblGapDetail and
#' tblGapHeader (AIM/DIMA/TerrADat) or both of GINTERCEPT and POINT (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblGapHeader Dataframe of the data structure tblGapHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblGapDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblGapDetail Dataframe of the data structure tblGapDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblGapHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param GINTERCEPT Dataframe of the data structure GINTERCEPT from the LMF/NRI
#' database. Use with POINT when data source is LMF or NRI; alternately provide
#' dsn.
#' @param POINT Dataframe of the data structure POINT from the LMF database.
#' Use with GINTERCEPT when data source if LMF or NRI; alternately provide dsn.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if GINTERCEPT and POINT are provided.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_gap
#' @family <gather>
#' @return A tall data frame containing the data from the gap measurements.
#' @examples
#' gather_gap(dsn = "Path/To/AIM_Geodatabase.gdb",
#'            source = "AIM")
#' gather_gap(dsn = "Path/To/LMF_Geodatabase.gdb",
#'            source = "LMF")
#'
#' aim_gapdetail <- read.csv("Path/To/tblGapDetail.csv")
#' aim_gapheader <- read.csv("Path/To/tblGapHeader.csv")
#' gather_gap(source = "AIM",
#'            tblGapDetail = aim_gapdetail,
#'            tblGapHeader = aim_gapheader)
#'
#' lmf_gintercept <- read.csv("Path/To/GINTERCEPT.csv")
#' lmf_point <- read.csv("Path/To/POINT.csv")
#' gather_gap(source = "LMF",
#'            GINTERCEPT = lmf_gintercept,
#'            POINT = lmf_point)

## gather gap data
#' @export gather_gap_terradat
#' @rdname gather_gap
gather_gap_terradat <- function(dsn = NULL,
                                tblGapDetail = NULL,
                                tblGapHeader = NULL,
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

  ### switch by input types
  if(!is.null(tblGapDetail) & !is.null(tblGapHeader)){
    if (verbose) {
      if (!is.null(dsn)) {
        message("Using the provided data frames. The provided dsn value is being ignored.")
      }
    }
    detail <- tblGapDetail
    header <- tblGapHeader
  } else if(!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblGapDetail and tblGapHeader")
    }
    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    # Read tblGapDetail
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblGapDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))

    # Read tblGapHeader
    header <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblGapHeader",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))

  } else {
    stop("Provide both tblGapDetail and tblGapHeader or the path to a GDB containing those tables.")
  }

  # Clean these up!
  detail <- dplyr::select(.data = detail,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  header <- dplyr::select(.data = header,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  # # add null DBKey field if not present
  # if(!"DBKey" %in% colnames(header)) header$DBKey <- NA
  # if(!"DBKey" %in% colnames(detail)) detail$DBKey <- NA

  # Make sure that Gap, GapStart, and GapEnd are all numeric.
  not_numeric_vars <- which(sapply(X = c(GapStart = "GapStart",
                                         GapEnd = "GapEnd",
                                         Gap = "Gap"),
                                   detail = detail,
                                   FUN = function(X, detail){
                                     class(detail[[X]]) != "numeric"
                                   }))
  detail <- dplyr::mutate(.data = detail,
                          dplyr::across(.cols = tidyselect::all_of(names(not_numeric_vars)),
                                        .fns = as.numeric))

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
                                                    "RecType",
                                                    "GapStart",
                                                    "GapEnd",
                                                    "Gap")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }


  # Merge header and detail data together.
  # We're suppressing warnings and not enforcing the relationship so that the
  # user can move ahead without cleaning their data first.
  # Because we have the orphaned headers from above, we'll do an inner join here
  # and use those to add in whatever else we need.
  gap_tall <- suppressWarnings(dplyr::inner_join(x = header,
                                                 y = detail,
                                                 # relationship = "one-to-many",
                                                 by = c("PrimaryKey",
                                                        "RecKey")))

  # This bit corrects (or at least replaces) NA values in a few of the variables
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            # Make sure that any NA values in the variables
                            # NoCanopyGaps and NoBasalGaps are assumed to
                            # be 0.
                            dplyr::across(.cols = tidyselect::all_of(c("NoCanopyGaps",
                                                                       "NoBasalGaps")),
                                          .fns = ~ as.numeric(tidyr::replace_na(data = .x,
                                                                                replace = 0))),
                            # For any records where we don't know the
                            # RecType value and NoCanopyGaps is 1, assume
                            # that RecType should be "C"
                            RecType = dplyr::case_when(NoCanopyGaps == 1 ~ tidyr::replace_na(RecType,
                                                                                             "C"),
                                                       .default = RecType),
                            # For any records where we don't know the
                            # Measure value and NoCanopyGaps is 1, assume
                            # that Measure should be 1
                            Measure = dplyr::case_when(NoCanopyGaps == 1 ~ tidyr::replace_na(Measure,
                                                                                             1),
                                                       .default = Measure),
                            # Make sure that any NA values in the variables
                            # GapStart, GapEnd, and Gap are 0 when the
                            # relevant No*Gaps and RecType values say there
                            # should be no gaps.
                            dplyr::across(.cols = tidyselect::all_of(c("GapStart",
                                                                       "GapEnd",
                                                                       "Gap")),
                                          .fns = ~ dplyr::case_when(NoCanopyGaps == 1 & RecType == "C" ~ tidyr::replace_na(data = .x,
                                                                                                                           replace = 0),
                                                                    NoBasalGaps == 1 & RecType == "B" ~ tidyr::replace_na(data = .x,
                                                                                                                          replace = 0),
                                                                    .default  = .x)))



  # Create records to represent the 0 values for situations where we have no
  # canopy or basal gaps but don't already have 0 values.
  # The function here is reframe() and not summarize() because some of the
  # combinations of values in the "by" variables don't have any qualifying
  # records and summarize() complains about it.
  missing_records <- dplyr::reframe(.data = gap_tall,
                                    # .by = 1:PrimaryKey,
                                    .by = tidyselect::all_of(c("PrimaryKey",
                                                               "LineKey",
                                                               "RecKey",
                                                               "NoCanopyGaps",
                                                               "NoBasalGaps")),
                                    needs_canopy = !("C" %in% RecType) & NoCanopyGaps == 1,
                                    needs_basal = !("B" %in% RecType) & NoBasalGaps == 1) |>
    dplyr::filter(.data = _,
                  needs_canopy | needs_basal) |>
    dplyr::mutate(.data = _,
                  RecType = dplyr::case_when(needs_canopy ~ "C",
                                             needs_basal ~ "B",
                                             .default = NA),
                  GapStart = 0,
                  GapEnd = 0,
                  Gap = 0,
                  Measure = 1) |>
    dplyr::select(.data = _,
                  -needs_canopy,
                  -needs_basal) |>
    dplyr::left_join(x = _,
                     y = dplyr::select(.data = header,
                                       -LineKey,
                                       -DBKey,
                                       -Measure,
                                       -NoCanopyGaps,
                                       -NoBasalGaps),
                     by = c("PrimaryKey", "RecKey"))

  gap_tall <- dplyr::bind_rows(gap_tall,
                               missing_records) |>
    dplyr::distinct()

  # Make sure that RecType is "P" for records where only perennial vegetation
  # was considered when evaluating if there was sufficient canopy to end a gap.
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            RecType = dplyr::case_when(PerennialsCanopy == 1 &
                                                         AnnualForbsCanopy == 0 &
                                                         AnnualGrassesCanopy == 0 &
                                                         OtherCanopy == 0 ~ "P",
                                                       .default = RecType))

  gap_tall
}

#' @export gather_gap_lmf
#' @rdname gather_gap
gather_gap_lmf <- function(dsn = NULL,
                           file_type = NULL,
                           GINTERCEPT = NULL,
                           POINT = NULL) {

  # if file type is NULL, define it by checking the extension of dsn
  if(is.null(file_type)){
    extension <- substr(dsn, nchar(dsn)-2, nchar(dsn))
    if(extension == "csv") {
      file_type <- "csv"
    } else if(extension == "gdb") {
      file_type <- "gdb"
    } else {
      file_type <- "txt"
    }
  }


  if(!is.null(GINTERCEPT) & !is.null(POINT)){
    gintercept <- GINTERCEPT
    point <- POINT
  } else if(!is.null(dsn) & file_type %in% c("gdb", "csv", "txt")){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a database containing GINTERCEPT and POINT")
    }
    gintercept <- switch(file_type,
                         "gdb" = {
                           suppressWarnings(sf::st_read(
                             dsn = dsn,
                             layer = "GINTERCEPT",
                             stringsAsFactors = FALSE,
                             quiet = T
                           )) %>%
                             dplyr::select_if(!names(.) %in% c(
                               'GlobalID',
                               'created_user',
                               'created_date',
                               'last_edited_user',
                               'last_edited_date'
                             ))
                         },
                         "txt" = {
                           read.table(paste(dsn, "gintercept.txt", sep = ""),
                                      stringsAsFactors = FALSE, strip.white = TRUE,
                                      header = FALSE, sep = "|"
                           )
                         },
                         "csv" = {
                           read.csv(dsn)
                         }
    )
    # Read in point file for other plot level information
    point <- switch(file_type,
                    "gdb" = {
                      suppressWarnings(sf::st_read(
                        dsn = dsn,
                        layer = "POINT",
                        stringsAsFactors = FALSE,
                        quiet = T
                      )) %>%
                        dplyr::select_if(!names(.) %in% c(
                          'GlobalID',
                          'created_user',
                          'created_date',
                          'last_edited_user',
                          'last_edited_date'
                        ))
                    },
                    "txt" = {
                      read.table(paste(dsn, "point.txt", sep = ""),
                                 stringsAsFactors = FALSE,
                                 strip.white = TRUE,
                                 header = FALSE, sep = "|"
                      )
                    },
                    "csv" = {
                      stop("csv not currently supported for gap data")
                      read.csv(point_dsn)
                    }
    )


    if (file_type == "txt") {
      # Add meaningful column names
      gintercept <- name_variables_nri(
        data = gintercept,
        table_name = "GINTERCEPT"
      )
      point <- name_variables_nri(
        data = gintercept,
        table_name = "POINT"
      )
    }
  }
  else {
    stop("Supply either GINTERCEPT and POINT, or the path to a GDB containing those tables")
  }

  # Ensure START_GAP and END_GAP are numeric
  gintercept$START_GAP <- as.numeric(gintercept$START_GAP)
  gintercept$END_GAP <- as.numeric(gintercept$END_GAP)

  # Look at the point table and add blanks or substitute perennial gap for
  # canopy gap
  canopy_infer <- point[point$GAPS_DIFFERENT_NESW == "N" |
                          point$GAPS_DIFFERENT_NWSE == "N", ] %>%
    dplyr::select(
      "PrimaryKey",
      "GAPS_DIFFERENT_NESW",
      "GAPS_DIFFERENT_NWSE"
    ) %>%

    # Gather so that we can query by lines
    tidyr::gather(key = "TRANSECT", value = "different", -"PrimaryKey") %>%

    # Select so that only values where canopy gap is not different
    subset(different == "N") %>%

    # Reduce line key to just line number
    dplyr::mutate(TRANSECT = stringr::str_replace_all(TRANSECT,
                                                      pattern = "GAPS_DIFFERENT_",
                                                      replace = ""
    ) %>% tolower())

  # select perennial gaps that are not different to canopy gaps and infer c
  # canopy gaps

  canopy_infer <- dplyr::full_join(gintercept, canopy_infer,
                                   by = c("PrimaryKey", "TRANSECT")
  ) %>%
    dplyr::filter(!is.na(different) & GAP_TYPE == "peren") %>%

    # Code perennial to canopy
    dplyr::mutate(GAP_TYPE = "canopy")

  # Join canopy data back to gintercept

  gintercept <- rbind(gintercept, dplyr::select(canopy_infer, -different))

  ## Add zeros where no canopy gap data were recorded
  zero_gap <- point %>%
    dplyr::select(
      "PrimaryKey",
      "DBKey",
      "BASAL_GAPS_NESW",
      "CANOPY_GAPS_NESW",
      "BASAL_GAPS_NWSE",
      "CANOPY_GAPS_NWSE",
      "PERENNIAL_CANOPY_GAPS_NESW",
      "PERENNIAL_CANOPY_GAPS_NWSE"
    ) %>%
    tidyr::gather(key = "TRANSECT", value = "zero", -c("PrimaryKey", "DBKey")) %>%

    # Filter for plots and lines where we need to insert zeros
    dplyr::filter(zero == "N") %>%

    # rework transect name
    dplyr::mutate(
      GAP_TYPE = stringr::str_replace(TRANSECT,
                                      pattern = "_.*",
                                      replacement = ""
      ) %>%
        # recode GAP_TYPE
        dplyr::recode(
          "CANOPY" = "canopy",
          "PERENNIAL" = "peren",
          "BASAL" = "basal"
        ),
      TRANSECT = stringr::str_sub(TRANSECT, -4) %>% tolower(),
      START_GAP = 0,
      END_GAP = 0
    )


  # Merge back to gintercept
  gintercept <- dplyr::full_join(gintercept, zero_gap,
                                 by = c("TRANSECT", "GAP_TYPE", "START_GAP",
                                        "END_GAP", "PrimaryKey", "DBKey")) %>%
    dplyr::select(-zero)

  # convert to metric, original data are in decimal feet
  gintercept$START_GAP <- gintercept$START_GAP * 30.48
  gintercept$END_GAP <- gintercept$END_GAP * 30.48
  gintercept$Gap <- abs(gintercept$END_GAP - gintercept$START_GAP)


  # check for negative values and remove
  gap <- gintercept %>% subset(Gap >= 0)


  # recode gap type so that it fits the DIMA types
  gap$GAP_TYPE <- as.character(gap$GAP_TYPE)
  gap$GAP_TYPE[gap$GAP_TYPE == "peren"] <- "P"
  gap$GAP_TYPE[gap$GAP_TYPE == "canopy"] <- "C"
  gap$GAP_TYPE[gap$GAP_TYPE == "basal"] <- "B"

  # rename fields so they can be merged with a DIMA/TerrADat type
  gap <- dplyr::rename(gap,
                       LineKey = TRANSECT, RecType = GAP_TYPE,
                       GapStart = START_GAP, GapEnd = END_GAP, SeqNo = SEQNUM
  )

  # units are metric
  gap$Measure <- 1

  # line length of an NRI transect in meters
  gap$LineLengthAmount <- 150 * 30.48 / 100

  # minimum gap size
  gap$GapMin <- 12 * 2.54

  # Strip down fields
  gap <- gap %>%
    dplyr::select_if(!names(.) %in% c(
      'SURVEY', 'COUNTY', 'STATE', 'PLOTKEY',
      'PSU', 'POINT',
      'created_user',
      'created_date',
      'last_edited_user',
      'last_edited_date',
      'GlobalID',
      'X'
    )) %>%
    # make sure data types are numeric when needed
    dplyr::mutate(
      GapStart = suppressWarnings(as.numeric(GapStart)),
      GapEnd = suppressWarnings(as.numeric(GapEnd)),
      Gap = suppressWarnings(as.numeric(Gap))
    )

  return(gap)
}

#' export gather_gap_survey123
#' rdname gather_gap
# gather_gap_survey123 <- function(Gap_0, GapDetail_1) {
#
#   # Add fields to match terradat schema
#   GapDetail_1$DBKey <- NA
#   GapDetail_1$SeqNo <- NA
#   Gap_0$DBKey <- NA
#   Gap_0$RecKey <- NA
#
#   # Select only the necessary fields
#   gap_detail <- GapDetail_1 %>%
#     dplyr::select(
#       GlobalID,
#       ParentGlobalID,
#       DBKey,
#       GapStart,
#       GapEnd,
#       RecType,
#       Gap,
#       SeqNo
#     )
#
#   gap_header <- Gap_0 %>%
#     dplyr::select(
#       GlobalID,
#       PrimaryKey = PlotKey,
#       DBKey,
#       LineKey,
#       RecKey,
#       FormDate,
#       Direction,
#       # Measure, # This is necessary but missing data
#       LineLengthAmount = LineLengthCM,
#       GapMin,
#       GapData, # Will have to be reclassified later
#       Notes = CollectNote,
#       NoCanopyGaps,
#       NoBasalGaps
#     )
#
#
#   ## ensure that gap, gapstart, and gapend are numeric
#   gap_detail$GapStart <- as.numeric(gap_detail$GapStart)
#   gap_detail$GapEnd <- as.numeric(gap_detail$GapEnd)
#   gap_detail$Gap <- as.numeric(gap_detail$Gap)
#   ##
#
#   # Merge header and detail data together
#   # Survey123 uses global ID, we have to translate it, then drop GlobalIDs
#   gap_tall <- dplyr::left_join(
#     x = gap_header,
#     y = gap_detail,
#     by = c("GlobalID" = "ParentGlobalID", "DBKey")
#   ) %>%
#     dplyr::select(-"GlobalID.y", -"GlobalID")
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- gap_tall$PrimaryKey[duplicated(gap_tall$PrimaryKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Remove all orphaned records
#   gap_tall <- gap_tall[!is.na(gap_tall$PrimaryKey), ]
#
#   # reclassify GapData from text to code
#   gap_tall$GapData <-
#     dplyr::recode(
#       gap_tall$GapData,
#       "Both" = 1, ### Probable bug here -- I do not know what survey123 codes this value as
#       "Canopy" = 2,
#       "Basal" = 3
#     )
#
#   # Look for NA values in NoCanopyGaps and NoBasalGaps, we assume they are 0
#   gap_tall <- gap_tall %>%
#     dplyr::mutate(
#       NoCanopyGaps = tidyr::replace_na(NoCanopyGaps, replace = 0),
#       NoBasalGaps = tidyr::replace_na(NoBasalGaps, replace = 0)
#     )
#
#   ## Add zero values where there is no canopy gap present on line
#   gap_tall[gap_tall$NoCanopyGaps == 1, ] <- gap_tall %>%
#     dplyr::filter(NoCanopyGaps == 1) %>%
#     tidyr::replace_na(list(
#       RecType = "C", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1
#     ))
#   gap_tall$NoCanopyGaps <- as.numeric(gap_tall$NoCanopyGaps)
#   gap_tall$NoBasalGaps <- as.numeric(gap_tall$NoBasalGaps)
#
#   ## Add zero values where there is no canopy gap present on the line, AND there is basal gap on the line
#   # Find missing records
#   gap_tall_missing_c <-
#     gap_tall %>%
#     dplyr::filter(NoCanopyGaps == 1,
#                   RecType != "C") %>%
#     dplyr::select(PrimaryKey, LineKey, RecKey, NoBasalGaps, NoCanopyGaps) %>%
#     unique() %>%
#     dplyr::mutate(RecType = "C", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1)
#
#   # Append them to gap_tall
#   gap_tall <-
#     dplyr::bind_rows(gap_tall, gap_tall_missing_c)
#
#   ## Add zero values where there is no basal gap present on line
#   gap_tall[gap_tall$NoBasalGaps == 1, ] <- gap_tall %>%
#     dplyr::filter(NoBasalGaps == 1) %>%
#     tidyr::replace_na(list(
#       RecType = "B", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1
#     ))
#
#   ## Add zero values where there is no basal gap present on the line, AND there is canopy gap on the line
#   # Find missing records
#   gap_tall_missing_b <-
#     gap_tall %>%
#     dplyr::filter(NoBasalGaps == 1,
#                   RecType != "B" | is.na(RecType)) %>%
#     dplyr::select(PrimaryKey, LineKey, RecKey, NoCanopyGaps, NoBasalGaps) %>%
#     unique() %>%
#     dplyr::mutate(RecType = "B", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1)
#
#   # Append them to gap_tall
#   gap_tall <-
#     dplyr::bind_rows(gap_tall, gap_tall_missing_b)
#
#   return(gap_tall)
# }
#

#' @export gather_gap
#' @rdname gather_gap
gather_gap <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblGapHeader = NULL,
                       tblGapDetail = NULL,
                       POINT = NULL,
                       GINTERCEPT = NULL#,
                       # Gap_0 = NULL,
                       # GapDetail_1 = NULL
) {

  # Gather gap using the appropriate method
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    gap <- gather_gap_terradat(dsn = dsn,
                               tblGapDetail = tblGapDetail,
                               tblGapHeader = tblGapHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    gap <- gather_gap_lmf(dsn = dsn,
                          file_type = file_type,
                          POINT = POINT,
                          GINTERCEPT = GINTERCEPT)
    # } else if(toupper(source) %in% c("SURVEY123")){
    # gap <- gather_gap_survey123(Gap_0 = Gap_0, GapDetail_1 = GapDetail_1)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  gap$source <- source

  if("sf" %in% class(gap)) gap <- sf::st_drop_geometry(gap)

  if (any(class(gap) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(gap)[do.call(rbind, vapply(gap,
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    gap <- dplyr::mutate_at(gap, dplyr::vars(change_vars),
                            dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  gap <- gap %>%
    dplyr::select(PrimaryKey, DBKey, LineKey, tidyselect::everything())

  return(gap)
}
