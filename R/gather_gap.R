#' Gather TerrADat Gap data into tall/long data frame
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for Gap.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param file_type Character string. Type of file, text or geodatabase, to read from.
#' @param source Character string. The original source of the data. "TerrAdat", "AIM", "DIMA", "LMF", "NRI" are all valide options.
#' @return A data frames containing the data from the Gap intercepts data in tall format.

#' @export gather_gap_terradat
#' @rdname gather_gap


gather_gap_terradat <- function(dsn) {
  # Read tblGapDetail
  gap_detail <- suppressWarnings(sf::st_read(dsn =dsn,
                                             layer = "tblGapDetail")) %>%

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
                                             layer = "tblGapHeader")) %>%

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

gather_gap_lmf <- function(dsn, file_type = "gdb") {
  gintercept <- switch(file_type,
    "gdb" = {
      suppressWarnings(sf::st_read(
        dsn = dsn,
        layer = "GINTERCEPT"
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


  if (file_type == "txt") {
    # Add meaningful column names
    gintercept <- name_variables_nri(data = gintercept,
                                     table_name = "GINTERCEPT")
  }


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
                       source) {
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
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
