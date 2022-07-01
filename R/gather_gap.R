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
                                tblGapHeader = NULL) {

  ### switch by input types
  if(!is.null(tblGapDetail) & !is.null(tblGapHeader)){
    gap_detail <- tblGapDetail %>%
      dplyr::select_if(!names(.) %in% c(
        'GlobalID',
        'created_user',
        'created_date',
        'last_edited_user',
        'last_edited_date',
        'DateLoadedInDb',
        'DateLoadedinDB'
      ))
    gap_header <- tblGapHeader %>%
      dplyr::select_if(!names(.) %in% c(
        'GlobalID',
        'created_user',
        'created_date',
        'last_edited_user',
        'last_edited_date',
        'DateLoadedInDb',
        'DateLoadedInDB'
      ))
  } else if(!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblGapDetail and tblGapHeader")
    }
    # Read tblGapDetail
    gap_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "tblGapDetail",
      stringsAsFactors = FALSE, quiet = T
    )) %>%

      # Remove database management fields that aren't relevant
      dplyr::select_if(!names(.) %in% c(
        'GlobalID',
        'created_user',
        'created_date',
        'last_edited_user',
        'last_edited_date',
        'DateLoadedInDb',
        'DateLoadedInDB'
      ))

    # Read tblGapHeader
    gap_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "tblGapHeader",
      stringsAsFactors = FALSE, quiet = T
    )) %>%

      # Remove database management fields that aren't relevant
      dplyr::select_if(!names(.) %in% c(
        'GlobalID',
        'created_user',
        'created_date',
        'last_edited_user',
        'last_edited_date',
        'DateLoadedInDb',
        'DateLoadedInDB'
      ))

  } else {
    stop("Supply either tblGapDetail and tblGapHeader, or the path to a GDB containing those tables")
  }


  ## New 2/15/22 # ensure that gap, gapstart, and gapend are numeric
  gap_detail$GapStart <- as.numeric(gap_detail$GapStart)
  gap_detail$GapEnd <- as.numeric(gap_detail$GapEnd)
  gap_detail$Gap <- as.numeric(gap_detail$Gap)
  ##

  # Merge header and detail data together
  gap_tall <- dplyr::left_join(
    x = gap_header,
    y = gap_detail,
    by = c("RecKey", "PrimaryKey", "DBKey")
  )

  ## Remove all orphaned records
  gap_tall <- gap_tall[!is.na(gap_tall$PrimaryKey), ]

  # Look for NA values in NoCanopyGaps and NoBasalGaps, we assume they are 0
  gap_tall <- gap_tall %>%
    dplyr::mutate(
      NoCanopyGaps = tidyr::replace_na(NoCanopyGaps, replace = 0),
      NoBasalGaps = tidyr::replace_na(NoBasalGaps, replace = 0)
    )

  ## Add zero values where there is no canopy gap present on line
  gap_tall[gap_tall$NoCanopyGaps == 1, ] <- gap_tall %>%
    dplyr::filter(NoCanopyGaps == 1) %>%
    tidyr::replace_na(list(
      RecType = "C",
      GapStart = 0,
      GapEnd = 0,
      Gap = 0
    ))


  ## Add zero values where there is no basal gap present on line
  gap_tall[gap_tall$NoBasalGaps == 1, ] <- gap_tall %>%
    dplyr::filter(NoBasalGaps == 1) %>%
    tidyr::replace_na(list(
      RecType = "B",
      GapStart = 0,
      GapEnd = 0,
      Gap = 0
    ))

  ## Identify which gaps are perennial gaps vs all canopy gaps. Perennial
  ## gaps are those with only PerennialsCanopy == 1
  gap_tall <- gap_tall %>% dplyr::mutate(RecType = as.character(RecType))

  gap_tall$RecType[gap_tall$PerennialsCanopy == 1 &
                     gap_tall$AnnualForbsCanopy == 0 &
                     gap_tall$AnnualGrassesCanopy == 0 &
                     gap_tall$OtherCanopy == 0] <- "P"

  ## last round drop
  gap_tall <- gap_tall %>% dplyr::select_if(!names(.) %in%
                                       c('DateLoadedInDb',
                                         'DateLoadedInDB',
                                         'DataErrorChecking',
                                         'DataEntry',
                                         'DateModified',
                                         'FormType')
  ) %>%
    # make sure data types are numeric when needed
    dplyr::mutate(
      GapStart = suppressWarnings(as.numeric(GapStart)),
      GapEnd = suppressWarnings(as.numeric(GapEnd)),
      Gap = suppressWarnings(as.numeric(Gap))
    )

  return(gap_tall)
}

#' @export gather_gap_lmf
#' @rdname gather_gap
gather_gap_lmf <- function(dsn = NULL,
                           file_type = "gdb",
                           GINTERCEPT = NULL,
                           POINT = NULL,
                           ESFSG = NULL) {

  if(!is.null(GINTERCEPT) & !is.null(POINT) & !is.null(ESFSG)){
    gintercept <- GINTERCEPT
    point <- POINT
    esfsg <- ESFSG
  } else if(!is.null(dsn) & file_type %in% c("gdb", "csv", "txt")){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a database containing GINTERCEPT, POINT, and ESFSG")
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


    # Read in ESFSG for other plot level information (specifically, line lengths)
    esfsg <- switch(file_type,
                    "gdb" = {
                      suppressWarnings(sf::st_read(
                        dsn = dsn,
                        layer = "ESFSG",
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
                      read.table(paste(dsn, "esfsg.txt", sep = ""),
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
      esfsg <- name_variables_nri(
        data = esfsg,
        table_name = "ESFSG"
      )
    }
  } else {
    stop("Supply GINTERCEPT, POINT, and ESFSG or the path to a GDB containing those tables")
  }


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

  # convert to metric (cm), original data are in decimal feet
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

#' @export gather_gap
#' @rdname gather_gap
gather_gap <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblGapHeader = NULL,
                       tblGapDetail = NULL,
                       POINT = NULL,
                       GINTERCEPT = NULL) {

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
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # gap$source <- toupper(source)
  gap$source <- source

  if("sf" %in% class(gap)) gap <- sf::st_drop_geometry(gap)

  if (any(class(gap) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(gap)[do.call(rbind, vapply(gap,
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    gap <- dplyr::mutate_at(gap, dplyr::vars(change_vars),
                            dplyr::funs(as.character))
  }

  return(gap)
}
