#' Gather TerrADat Gap data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for Gap.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param file.type Character string. Type of file, text or geodatabase, to read from.
#' @param source Character string. The original source of the data. "TerrAdat", "AIM", "DIMA", "LMF", "NRI" are all valide options.
#' @return A data frames containing the data from the Gap intercepts data in tall format.
#' @export gather.gap.terradat
#' @rdname gather_gap


gather.gap.terradat <- function(dsn) {
  gap.detail <- suppressWarnings(sf::st_read(dsn, layer = "tblGapDetail")) %>%
    subset(., select = -c(
      GlobalID,
      created_user,
      created_date,
      last_edited_user,
      last_edited_date
    ))
  gap.header <- suppressWarnings(sf::st_read(dsn, layer = "tblGapHeader")) %>%
    subset(., select = -c(
      GlobalID,
      created_user,
      created_date,
      last_edited_user,
      last_edited_date
    ))


  # Merge header and detail data together
  gap.tall <- dplyr::left_join(
    x = gap.header,
    y = gap.detail
  )

  ## Remove all orphaned records
  gap.tall <- gap.tall[!is.na(gap.tall$PrimaryKey), ]

  #Look for NA values in NoCanopyGaps and NoBasalGaps, we assume they are 0
  gap.tall <- gap.tall %>%
    dplyr::mutate(NoCanopyGaps = tidyr::replace_na(NoCanopyGaps, replace = 0),
                  NoBasalGaps = tidyr::replace_na(NoBasalGaps, replace = 0))

  ## Add zero values where there is no canopy gap present on line
  gap.tall[gap.tall$NoCanopyGaps == 1, ] <- gap.tall %>%
    dplyr::filter(NoCanopyGaps == 1) %>%
    tidyr::replace_na(list(RecType = "C", GapStart = 0, GapEnd = 0, Gap = 0))

  ## Add zero values where there is no basal gap present on line
  gap.tall[gap.tall$NoBasalGaps == 1, ] <- gap.tall %>%
    dplyr::filter(NoBasalGaps == 1) %>%
    tidyr::replace_na(list(RecType = "B", GapStart = 0, GapEnd = 0, Gap = 0))

  return(gap.tall)
}

#' @export gather.gap.lmf
#' @rdname gather_gap

gather.gap.lmf <- function(dsn, file.type = "gdb") {
  gintercept <- switch(file.type,
                       "gdb" = {
                         suppressWarnings(sf::st_read(dsn = dsn,
                                                      layer = "GINTERCEPT")) %>%
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
                                    header = FALSE, sep = "|")
                       }
  )


  if (file.type == "txt") {
    # Add meaningful column names
    colnames <- as.vector(as.data.frame(subset(terradactyl::nri.data.column.explanations,
                                               TABLE.NAME == "GINTERCEPT",
                                               select = FIELD.NAME)))
    colnames <- colnames$FIELD.NAME
    pintercept <- gintercept[1:length(colnames)]
    names(gintercept) <- colnames
  }


  # convert to metric, original data are in decimal feet
  gintercept$START_GAP <- gintercept$START_GAP * 30.48
  gintercept$END_GAP <- gintercept$END_GAP * 30.48
  gintercept$Gap <- gintercept$END_GAP - gintercept$START_GAP


  # We need to establish and/or fix the PLOTKEY so it exists in a single field.
  gintercept$PrimaryKey <- paste(gintercept$SURVEY,
                                 gintercept$STATE,
                                 gintercept$COUNTY,
                                 gintercept$PSU,
                                 gintercept$POINT,
                                 sep = "")


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
  gap$LineLengthAmount <- 150 * 30.48/100

  # minimum gap size
  gap$GapMin <- 12 * 2.54

  # Strip down fields
  gap <- dplyr::select(gap, -c(SURVEY:POINT))

  return(gap)
}

### Wrapper function for flexibility
#' @export gather.gap
#' @rdname gather_gap
gather.gap <- function(dsn,
                       file.type = "gdb",
                       source) {
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"))
    stop("No valid source provided"))

  # Gather gap using the appropriate method
  gap <- switch(toupper(source),
    "AIM" = gather.gap.terradat(dsn = dsn),
    "TERRADAT" = gather.gap.terradat(dsn = dsn),
    "DIMA" = gather.gap.terradat(dsn = dsn),
    "LMF" = gather.gap.lmf(dsn = dsn, file.type = file.type),
    "NRI" = gather.gap.lmf(dsn = dsn, file.type = file.type)
  )

  # Add source field so that we know where the data came from
  gap$source <- toupper(source)

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  if (any(str(gap) %in% c("POSIXct", "POSIXt"))) {

     change.vars <- names(gap)[do.call(rbind, vapply(gap, class))[, 1] %in%
                                c("POSIXct", "POSIXt")]

     # Update fields
     gap <- dplyr::mutate_at(gap, dplyr::vars(change.vars),
                             dplyr::funs(as.character))
  }


  return(gap)
}
