#' Gather TerrADat Gap data into tall/long data frame
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for Gap.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param file_type Character string. Type of file, text or geodatabase, to read from.
#' @param source Character string. The original source of the data. "TerrAdat", "AIM", "DIMA", "LMF", "NRI" are all valide options.
#' @param point_file Character string. File path to the point file, required for csv and text file types wher "LMF" or "NRI" are identified as the source.
#' @return A data frames containing the data from the Gap intercepts data in tall format.

#' @export gather_gap_terradat
#' @rdname gather_gap


gather_gap_terradat <- function(dsn) {
  # Read tblGapDetail
  gap_detail <- suppressWarnings(sf::st_read(dsn =dsn,
                                             layer = "tblGapDetail",
                                             stringsAsFactors = FALSE)) %>%

    # Remove database management fields that aren't relevant
   subset(., select = -c(
     GlobalID,
     created_user,
     created_date,
     last_edited_user,
     last_edited_date
    ))

  # Read tblGapHeader
  gap_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                             layer = "tblGapHeader",
                                             stringsAsFactors = FALSE)) %>%

    # Remove database management fields that aren't relevant
    subset(., select = -c(
      GlobalID,
      created_user,
      created_date,
      last_edited_user,
      last_edited_date
    ))


  # Merge header and detail data together
  gap_tall <- dplyr::left_join(
    x = gap_header,
    y = gap_detail
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
    tidyr::replace_na(list(RecType = "C",
                           GapStart = 0,
                           GapEnd = 0,
                           Gap = 0))

  ## Add zero values where there is no basal gap present on line
  gap_tall[gap_tall$NoBasalGaps == 1, ] <- gap_tall %>%
    dplyr::filter(NoBasalGaps == 1) %>%
    tidyr::replace_na(list(RecType = "B",
                           GapStart = 0,
                           GapEnd = 0,
                           Gap = 0))

  ## Identify which gaps are perennial gaps vs all canopy gaps. Perennial
  ## gaps are those with only PerennialsCanopy == 1
  gap_tall <-gap_tall %>% dplyr::mutate(RecType = as.character(RecType))

   gap_tall$RecType[gap_tall$PerennialsCanopyv==1 &
              gap_tall$AnnualForbsCanopy == 0 &
              gap_tall$AnnualGrassesCanopy ==0 &
              gap_tall$OtherCanopy == 0 ]<- "P"

  return(gap_tall)
}

#' @export gather_gap_lmf
#' @rdname gather_gap

gather_gap_lmf <- function(dsn,
                           file_type = "gdb",
                           point_dsn = "" ) {
  gintercept <- switch(file_type,
    "gdb" = {
      suppressWarnings(sf::st_read(
        dsn = dsn,
        layer = "GINTERCEPT",
        stringsAsFactors = FALSE
      )) %>%
        subset(., select = -c(
          GlobalID,
          created_user,
          created_date,
          last_edited_user,
          last_edited_date
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
                      stringsAsFactors = FALSE
                    )) %>%
                      subset(., select = -c(
                        GlobalID,
                        created_user,
                        created_date,
                        last_edited_user,
                        last_edited_date
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
                    read.csv(dsn)
                  }
  )


  if (file_type == "txt") {
    # Add meaningful column names
    gintercept <- name_variables_nri(data = gintercept,
                                     table_name = "GINTERCEPT")
    point <- name_variables_nri(data = gintercept,
                                table_name = "POINT")
  }

  # Look at the point table and add blanks or substitute perennial gap for
  # canopy gap
  canopy_infer <- point[point$GAPS_DIFFERENT_NESW == "N"|
                          point$GAPS_DIFFERENT_NWSE == "N",] %>%
    dplyr::select("PrimaryKey",
                  "GAPS_DIFFERENT_NESW",
                  "GAPS_DIFFERENT_NWSE") %>%

    # Gather so that we can query by lines
    tidyr::gather(key = "TRANSECT", value = "different", -"PrimaryKey") %>%

    # Select so that only values where canopy gap is not different
    subset(different == "N") %>%

    # Reduce line key to just line number
    dplyr::mutate(TRANSECT = stringr::str_replace_all(TRANSECT,
                                                  pattern = "GAPS_DIFFERENT_",
                                                  replace = "") %>% tolower)

  # select perennial gaps that are not different to canopy gaps and infer c
  # canopy gaps

  canopy_infer <- dplyr::full_join(gintercept, canopy_infer,
                                   by = c("PrimaryKey", "TRANSECT")) %>%
   dplyr::filter(!is.na(different) & GAP_TYPE == "peren") %>%

    # Code perennial to canopy
    dplyr::mutate(GAP_TYPE = "canopy")

  # Join canopy data back to gintercept

  gintercept <- rbind(gintercept, dplyr::select(canopy_infer, -different))

  ## Add zeros where no canopy gap data were recorded
  zero_gap <- point %>% dplyr::select("PrimaryKey",
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
    dplyr::mutate(GAP_TYPE = stringr::str_replace(TRANSECT,
                                                  pattern = "_.*",
                                                  replacement = "") %>%
                    # recode GAP_TYPE
                    dplyr::recode("CANOPY" =  "canopy",
                                  "PERENNIAL" = "peren",
                                  "BASAL" = "basal"),
      TRANSECT = stringr::str_sub(TRANSECT, -4) %>% tolower(),
      START_GAP = 0,
      END_GAP = 0)


  # Merge back to gintercept
  gintercept <- dplyr::full_join(gintercept, zero_gap)  %>% dplyr::select(-zero)

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
   gap <- gap[,!colnames(gap) %in% c("SURVEY","COUNTY",
                                                         "PSU","POINT",
                                                         "created_user",
                                                         "created_date",
                                                         "last_edited_user",
                                                         "last_edited_date",
                                                         "GlobalID", "X")]

  return(gap)
}

### Wrapper function for flexibility
#' @export gather_gap
#' @rdname gather_gap
gather_gap <- function(dsn,
                       file_type = "gdb",
                       source,
                       point_file = "") {
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
  } )

  # Check for valid source file if using LMF or NRI from txt or csv
  try(if(source %in% c("LMF", "NRI") &
         file_type %in% c("txt", "csv") &
         point_file == "") {
    stop("Must specify point_file")
  } )

  # Gather gap using the appropriate method
  gap <- switch(toupper(source),
    "AIM" = gather_gap_terradat(dsn = dsn),
    "TERRADAT" = gather_gap_terradat(dsn = dsn),
    "DIMA" = gather_gap_terradat(dsn = dsn),
    "LMF" = gather_gap_lmf(dsn = dsn, file_type = file_type),
    "NRI" = gather_gap_lmf(dsn = dsn, file_type = file_type)
  )

  # Add source field so that we know where the data came from
  gap$source <- toupper(source)

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  if (any(str(gap) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(gap)[do.call(rbind, vapply(gap, class))[, 1] %in%
      c("POSIXct", "POSIXt")]

    # Update fields
    gap <- dplyr::mutate_at(
      gap, dplyr::vars(change_vars),
      dplyr::funs(as.character)
    )
  }

  gap
}
