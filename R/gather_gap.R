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
                         "DBKey",
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
  # This is a left join so that we can properly include LineKeys that don't have
  # associated gaps because there were none. That's critical for calculating
  # gap percentages.
  gap_tall <- suppressWarnings(dplyr::left_join(x = header,
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

  #### Reading and cleanup #####################################################
  # if file type is NULL, define it by checking the extension of dsn
  valid_file_types <- c("csv", "gdb", "txt")

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

  gintercept <- dplyr::select(.data = gintercept,
                              -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()
  point <- dplyr::select(.data = point,
                         -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

  # Ensure START_GAP and END_GAP are numeric
  gintercept <- dplyr::mutate(.data = gintercept,
                              dplyr::across(.cols = tidyselect::all_of(c("START_GAP",
                                                                         "END_GAP")),
                                            .fns = as.numeric))

  if (any(gintercept$START_GAP > gintercept$END_GAP)) {
    warning("There are some records with negative gap sizes. These will be dropped.")
    gintercept <- dplyr::filter(.data = gintercept,
                                START_GAP <= END_GAP)
  }

  #### Identical perennial and all-plant canopy ################################
  # This takes the point table and identifies which transects at which points
  # are marked as having identical perennial-only and all-plant canopies
  # We'll use this to make records for the all-plant canopy values from the
  # perennial-only ones where necessary.

  # Grab only the relevant variables, the PrimaryKey and the ones indicating if
  # the gaps were different.
  potential_canopy_transects <- dplyr::select(.data = point,
                                              PrimaryKey,
                                              tidyselect::starts_with(match = "GAPS_DIFFERENT_")) |>
    # Pivot that so that there's one row for each PrimaryKey-transect
    # combination and a logical variable indicating if the gaps were different.
    tidyr::pivot_longer(data = _,
                        cols = -PrimaryKey,
                        names_to = "TRANSECT",
                        # This takes the variable names and gets the transect ID
                        # that's found in GINTERCEPT from them.
                        names_transform = ~ tolower(stringr::str_extract(string = .x,
                                                                         pattern = "[NSEW]{4}$")),
                        values_to = "same",
                        # This converts the character values into logical ones
                        # where TRUE corresponds to the gaps being the same.
                        values_transform = ~ .x %in% c("N")) |>
    dplyr::filter(.data = _,
                  same) |>
    dplyr::select(.data = _,
                  -same) |>
    dplyr::distinct()

  # These are the data which *might* need inference, by which we mean copying
  # the perennial-only records and changing the GAP_TYPE to "canopy".
  potential_inference_data <- dplyr::left_join(x = potential_canopy_transects,
                                               y = gintercept,
                                               relationship = "one-to-many",
                                               by = c("PrimaryKey",
                                                      "TRANSECT"))

  # Check to limit this to only transects where there's not already canopy data.
  agreement_joining_vars <- c("PrimaryKey",
                              "TRANSECT",
                              "START_GAP",
                              "END_GAP")
  agreement_summary <- dplyr::full_join(x = dplyr::filter(.data = potential_inference_data,
                                                          GAP_TYPE == "peren") |>
                                          dplyr::select(.data = _,
                                                        tidyselect::all_of(agreement_joining_vars)) |>
                                          dplyr::mutate(.data = _,
                                                        perennial = TRUE),
                                        y = dplyr::filter(.data = potential_inference_data,
                                                          GAP_TYPE == "canopy") |>
                                          dplyr::select(.data = _,
                                                        tidyselect::all_of(agreement_joining_vars)) |>
                                          dplyr::mutate(.data = _,
                                                        canopy = TRUE),
                                        by = agreement_joining_vars)

  # So, based on the agreement (or disagreement) we need to warn the user and
  # take action.
  if (any(is.na(agreement_summary$canopy)) | any(is.na(agreement_summary$perennial))) {
    disagreeing_transects <- dplyr::summarize(.data = agreement_summary,
                                              .by = tidyselect::all_of(c("PrimaryKey",
                                                                       "TRANSECT")),
                                              # We'll get the proportion of the
                                              # GAP_TYPE records that're NA
                                              # which should be the proportion
                                              # of records which don't match
                                              # between the gap types.
                                              proportion_na_canopy = sum(is.na(canopy)) / dplyr::n(),
                                              proportion_na_perennial = sum(is.na(perennial)) / dplyr::n())
    # This identifies the places where there were no canopy records but there
    # were perennial records. For those, we'll copy the perennial records and
    # change GAP_TYPE to "canopy"
    if (any(disagreeing_transects$proportion_na_canopy == 1)) {
      warning("There are transects where it was indicated that perennial-only and all-plant canopy gaps were identical but only perennial records exist. The all-plant records will be inferred from the perennial-only records.")
      inferred_canopy <- dplyr::left_join(x = dplyr::filter(.data = disagreeing_transects,
                                                            proportion_na_canopy == 1) |>
                                            dplyr::select(.data = _,
                                                          PrimaryKey,
                                                          TRANSECT),
                                          y = potential_inference_data,
                                          by = c("PrimaryKey",
                                                 "TRANSECT")) |>
        dplyr::mutate(.data = _,
                      GAP_TYPE = "canopy")
      gintercept <- dplyr::bind_rows(gintercept,
                                     inferred_canopy)
    }

    # This does the same but for perennial-only records.
    # This shouldn't happen based on how LMF is implemented in the field, but
    # better safe than sorry.
    if (any(disagreeing_transects$proportion_na_perennial == 1)) {
      warning("There are transects where it was indicated that perennial-only and all-plant canopy gaps were identical but only canopy records exist. This is unexpected based on LMF data collection methods, but the perennial-only records will be inferred from the all-plant records.")
      inferred_perennial <- dplyr::left_join(x = dplyr::filter(.data = disagreeing_transects,
                                                               proportion_na_perennial == 1) |>
                                               dplyr::select(.data = _,
                                                             PrimaryKey,
                                                             TRANSECT),
                                             y = potential_inference_data,
                                             by = c("PrimaryKey",
                                                    "TRANSECT")) |>
        dplyr::mutate(.data = _,
                      GAP_TYPE = "peren")
      gintercept <- dplyr::bind_rows(gintercept,
                                     inferred_perennial)
    }



    # This is where we warn the user if there were only partial mismatches.
    # This really shouldn't happen if the values are expected to be the same!
    partial_mismatches <- dplyr::filter(.data = disagreeing_transects,
                                        proportion_na_canopy < 1,
                                        proportion_na_canopy > 0,
                                        proportion_na_perennial < 1,
                                        proportion_na_perennial > 0) |>
      dplyr::select(.data = _,
                    PrimaryKey,
                    TRANSECT) |>
      dplyr::distinct()
    if (nrow(partial_mismatches) > 0) {
      warning(paste0("There are ", nrow(partial_mismatches), " transects representing ",
                     length(unique(partial_mismatches$PrimaryKey)),
                     " sampling locations where perennial-only and all-plant canopy were indicated to be identical but they are not. These records will be left as-is and calculations using those transects will produce different values for perennial-only and all-plant canopy."))
    }
  }

  #### Zero canopy gaps ########################################################
  # Find the places where there were supposedly no gaps, adding 0 records where
  # appropriate and warning the user when there are unexpected non-zero records.

  # Identify the places where the point table indicates that there aren't gaps.
  potential_zero_gap_transects <- dplyr::select(.data = point,
                                                PrimaryKey,
                                                tidyselect::matches(match = "_GAPS_[NSEW]{4}$")) |>
    tidyr::pivot_longer(data = _,
                        cols = tidyselect::matches(match = "_GAPS_[NSEW]{4}$"),
                        names_to = c("GAP_TYPE",
                                     "TRANSECT"),
                        # This will produce the corresponding GAP_TYPE and
                        # TRANSECT values from the variable names.
                        names_pattern = "(^[A-Z]+).+([NSEW]{4})",
                        names_transform = ~ stringr::str_extract(string = tolower(.x),
                                                                 pattern = "basal|canopy|peren|[nesw]{4}"),
                        values_to = "not_present",
                        values_transform = ~ .x %in% c("N")) |>
    dplyr::filter(.data = _,
                  not_present) |>
    dplyr::select(.data = _,
                  -not_present) |>
    dplyr::distinct()

  # Find the transects that claimed to not have data but actually do.
  actually_present <- dplyr::left_join(x = potential_zero_gap_transects,
                                       y = gintercept,
                                       by = c("PrimaryKey",
                                              "TRANSECT",
                                              "GAP_TYPE")) |>
    dplyr::filter(.data = _,
                  # Assuming that gaps are always recorded with the GAP_END
                  # value being larger than the START_GAP value, this will
                  # limit records to only those where there were no 0-length
                  # gaps
                  !(END_GAP %in% c(0, NA)))

  actually_present_summary <- dplyr::summarize(.data = actually_present,
                                               .by = c("GAP_TYPE"),
                                               pk_count = length(unique(PrimaryKey)),
                                               transect_count = length(unique(TRANSECT)))

  # If there were data where we didn't expect them, warn the user!
  if (nrow(actually_present_summary) > 0) {
    warning(paste0("There are ", sum(actually_present_summary$transect_count),
                   " transects across ", sum(actually_present_summary$pk_count),
                   " sampling locations which are flagged as not having gaps (perennial-only canopy, all-plant canopy, or basal) but which do have gaps recorded. These records will be left as-is."))

    # Remove the ones that have data from the set that we're making 0 records for
    potential_zero_gap_transects <- dplyr::select(.data = actually_present,
                                                  PrimaryKey,
                                                  TRANSECT) |>
      dplyr::distinct() |>
      dplyr::mutate(.data = _,
                    present = TRUE) |>
      dplyr::left_join(x = potential_zero_gap_transects,
                       y = _,
                       by = c("PrimaryKey",
                              "TRANSECT")) |>
      dplyr::filter(.data = _,
                    is.na(present)) |>
      dplyr::select(.data = _,
                    -present)
  }

  # Create the 0 records
  if (nrow(potential_zero_gap_transects) > 0) {
    zero_gaps <- dplyr::mutate(.data = potential_zero_gap_transects,
                               SEQNUM = 1,
                               START_GAP = 0,
                               END_GAP = 0) |>
      dplyr::left_join(x = _,
                       y = dplyr::distinct(dplyr::select(.data = gintercept,
                                                         -tidyselect::all_of(c("TRANSECT",
                                                                               "SEQNUM",
                                                                               "GAP_TYPE",
                                                                               "START_GAP",
                                                                               "END_GAP")))),
                       by = c("PrimaryKey"))
    gintercept <- dplyr::bind_rows(gintercept,
                                   zero_gaps)
  }

  #### Cleanup and conversion ##################################################
  # We need to reformat this so that it's in the standard format.

  output <- dplyr::rename(.data = gintercept,
                          LineKey = TRANSECT,
                          RecType = GAP_TYPE,
                          GapStart = START_GAP,
                          GapEnd = END_GAP,
                          SeqNo = SEQNUM) |>
    dplyr::mutate(.data = _,
                  # Convert from decimal feet to centimeters
                  dplyr::across(.cols = tidyselect::all_of(c("GapStart",
                                                             "GapEnd")),
                                .fns = ~ .x * 12 * 2.54),
                  # The abs() shouldn't be necessary, but just to be safe!
                  Gap = abs(GapStart - GapEnd),
                  RecType = dplyr::case_when(stringr::str_detect(string = RecType,
                                                                 "peren") ~ "P",
                                             stringr::str_detect(string = RecType,
                                                                 "canopy") ~ "C",
                                             stringr::str_detect(string = RecType,
                                                                 "basal") ~ "B",
                                             .default = RecType),
                  # The 1 here indicates that units are metric
                  Measure = 1,
                  # LMF transects are always 150 feet, but we want that length
                  # in meters.
                  LineLengthAmount = 150 * 12 * 2.54 / 100,
                  # The minimum gap size in LMF is 12 inches but we want cm
                  GapMin = 12 * 2.54)

  # Return only the variables we want and in an order we want.
  output_vars <- c("PrimaryKey",
                   "LineKey",
                   "LineLengthAmount",
                   "Measure",
                   "GapMin",
                   "RecType",
                   "SeqNo",
                   "GapStart",
                   "GapEnd",
                   "Gap")
  output <- dplyr::select(.data = output,
                          tidyselect::all_of(output_vars))

  output
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
                       GINTERCEPT = NULL,
                       autoQC = TRUE#,
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
    dplyr::select(PrimaryKey, LineKey, tidyselect::everything())

  # Drop rows with no data
  gap <- gap %>%
    dplyr::filter(!(is.na(Gap) &
                      is.na(GapEnd) &
                      is.na(GapMin) &
                      is.na(GapStart) &
                      is.na(LineKey) &
                      is.na(LineLengthAmount) &
                      is.na(Measure) &
                      is.na(RecType) &
                      is.na(SeqNo)))

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    gap <- gap %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "gap")
  }

  return(gap)
}
