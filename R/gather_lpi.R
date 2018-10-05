#' Gather TerrADat LPI data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param source Character string. The data source format. \code("AIM", "NRI", "LMF, "TerrADat") are valid options.
#' @importFrom magrittr %>%
#' @name gather_lpi
#' @family <gather>
#' @return A data frames containing the data from the LPI pin intercepts


#' @export gather.lpi.terradat
#' @rdname gather_lpi
## Function to make tall format of LPI data from TerrADat
gather.lpi.terradat <- function(dsn) {

  # Read LPI information from TerrADat
  lpi.detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "tblLPIDetail"
  ))
  lpi.header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "tblLPIHeader"
  ))

  # Make a tall data frame with the hit codes by layer and the checkbox designation

  lpi.hits.tall <-  lpi.detail %>% dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(PrimaryKey, PointLoc, PointNbr, RecKey, ShrubShape,
                  TopCanopy, SoilSurface, dplyr::matches("^Lower")) %>%
    tidyr::gather(key = layer,
                  value = code,
                  TopCanopy, SoilSurface, dplyr::matches("^Lower"))

  # Remove all records where no hit was recorded (e.g., "None", "NA"

  lpi.hits.tall <- dplyr::filter(
    .data = lpi.hits.tall,
    !is.na(code),
    code != "",
    code != "None",
    !is.na(PrimaryKey),
    !is.na(RecKey)
  )


  ## Make a tall data frame the checkbox status by layer

  lpi.chkbox.tall <- lpi.detail %>%
    dplyr::select(PrimaryKey, PointLoc, PointNbr, RecKey,
                  dplyr::matches("^Chkbox")) %>%
    tidyr::gather(key = layer, value = "chckbox",
                  dplyr::matches("^Chkbox"))



  # Remove Woody and Herbaceous Checkbox
  lpi.chkbox.tall <- lpi.chkbox.tall[!(lpi.chkbox.tall$chckbox %in%
                                         c("ChckboxWoody",
                                           "ChckboxHerbaceous")), ]

  ## Make the names in the layer variable match
  lpi.chkbox.tall$layer <- gsub(lpi.chkbox.tall$layer,
    pattern = "^Chkbox",
    replacement = ""
  )

  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Top"] <- "TopCanopy"
  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Soil"] <- "SoilSurface"

  # Print update because this function can take a while
  message("Merging LPI Header and LPI Detail tables")

  # Merge checkbox and hit data as well as the header data
  lpi.tall <- suppressWarnings(dplyr::left_join(
    x = lpi.hits.tall,
    y = lpi.chkbox.tall,
    all.x = TRUE,
    by = c("PrimaryKey", "PointLoc", "PointNbr", "RecKey", "layer")
  ) %>%
    dplyr::left_join(
      x = dplyr::select(
        lpi.header,
        LineKey:CheckboxLabel,
        PrimaryKey,
        DBKey
      ),
      y = .,
      by = c("PrimaryKey", "RecKey")
    ))

  return(lpi.tall)
  ## Output the list
}

#' @export gather.lpi.lmf
#' @rdname gather_lpi

gather.lpi.lmf <- function(dsn,
                           file.type = "gdb" ) {

  # Read  PINTERCEPT table in .txt or .gdb

  pintercept <- switch(file.type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn = dsn, layer = "PINTERCEPT"))
    },
    "txt" = {
      read.table(paste(dsn, "pintercept.txt", sep = ""),
                 stringsAsFactors = FALSE,
                 strip.white = TRUE,
                 header = FALSE, sep = "|")
    }
  )

  # if it is in a text file, there are no field names assigned.
  if (file.type == "txt") {
    colnames <- as.vector(as.data.frame(subset(
      terradactyl::nri.data.column.explanations,
      TABLE.NAME == "PINTERCEPT",
      select = FIELD.NAME)))
    colnames <- colnames$FIELD.NAME
    colnames <- colnames[1:ncol(pintercept)] %>% na.omit()
    names(pintercept) <- colnames
  }

  # remove any NA field names that may have been introduced
  pintercept <- pintercept[, !is.na(colnames(pintercept))]

  # We need to establish and/or fix the PrimaryKey so it exists in a single field.
  pintercept$PrimaryKey <- paste(pintercept$SURVEY,
                                 pintercept$STATE,
                                 pintercept$COUNTY,
                                 pintercept$PSU,
                                 pintercept$POINT,
                                 sep = "")


  # For line point intercept data (cover calculations--point number 75 is recorded twice—once on each transect.
  # We only want to use it once in the calculations.
  # Prior to doing these calculations, it would be beneficial to remove one of the point 75’s from the data set.
  # Remove the nesw transect—that would be all rows in pintercept where column 6 = “nesw” AND column 7 = 75.
  pintercept <- pintercept %>% subset(!(MARK == 75 & TRANSECT == "nesw"))


  # Where there is a Soil hit, LMF records "None" in BASAL and leaves NONSOIL blank. Let's fill in an "S" to indicate soil
  levels(pintercept$NONSOIL) <- c(levels(pintercept$NONSOIL), "S")
  pintercept$NONSOIL[pintercept$BASAL == "None" & pintercept$NONSOIL == ""] <- "S"



  # Identify the pin drop variables
  pin.drop <- c(
    colnames(pintercept)[grepl(pattern = "^HIT[1-6]$", x = colnames(pintercept))],
    "BASAL",
    "NONSOIL"
  )

  pintercept <- dplyr::select(pintercept, -c(SURVEY:POINT))
  # Create a tall table
  lpi.hits.tall <- data.table::melt(
    data = pintercept,
    id.vars = colnames(pintercept)[!colnames(pintercept) %in% pin.drop],
    measure.vars = pin.drop,
    variable.name = "layer",
    value.name = "code",
    na.rm = TRUE
  )


  # Remove blank fields with no data
  lpi.hits.tall <- lpi.hits.tall %>% subset(code != "")

  # Rename "BASAL" and "NONSOIL" to "SoilSurface"
  lpi.hits.tall$layer <- stringr::str_replace_all(
    string = lpi.hits.tall$layer,
    pattern = "BASAL|NONSOIL",
    replacement = "SoilSurface"
  )


  # Rename "Hit1" as "TopCanopy"
  lpi.hits.tall$layer <- stringr::str_replace_all(
    string = lpi.hits.tall$layer,
    pattern = "HIT1",
    replacement = "TopCanopy"
  )



  # Change "Transect and Mark to common names to DIMA schema

  lpi.hits.tall <- dplyr::rename(lpi.hits.tall, LineKey = TRANSECT, PointNbr = MARK, ShrubShape = SAGEBRUSH_SHAPE)

  # Convert to factor
  lpi.hits.tall <- lpi.hits.tall %>% dplyr::mutate_if(is.character, dplyr::funs(factor))

  # Convert ShrubShape values to be consistent with DIMA schema,
  #1==Columnar, 2=Spreading, 3=Mixed, 0 is NA
  lpi.hits.tall$ShrubShape[lpi.hits.tall$ShrubShape == 1] <- "C"
  lpi.hits.tall$ShrubShape[lpi.hits.tall$ShrubShape == 2] <- "S"
  lpi.hits.tall$ShrubShape[lpi.hits.tall$ShrubShape == 3] <- "M"
  lpi.hits.tall$ShrubShape[lpi.hits.tall$ShrubShape == 0] <- NA

  return(lpi.hits.tall)
}

#' @export gather.lpi
#' @rdname gather_lpi

# Wrapper gather.lpi function
gather.lpi <- function(dsn,
                       file.type = "gdb",
                       source) {
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"))
    stop("No valid source provided"))

  # Gather LPI using the appropriate gather function
  lpi <- switch(toupper(source),
    "AIM" = gather.lpi.terradat(dsn = dsn),
    "TERRADAT" = gather.lpi.terradat(dsn = dsn),
    "DIMA" = gather.lpi.terradat(dsn = dsn),
    "LMF" = gather.lpi.lmf(dsn = dsn, file.type = file.type),
    "NRI" = gather.lpi.lmf(dsn = dsn, file.type = file.type)
  )

  # Add source field
  lpi$source <- toupper(source)

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  change.vars <- names(lpi)[do.call(rbind, sapply(lpi, class))[, 1] %in%
                              c("POSIXct", "POSIXt")]
  # Update fields
  lpi <- dplyr::mutate_at(lpi, dplyr::vars(change.vars), dplyr::funs(as.character))

  return(lpi)
}
