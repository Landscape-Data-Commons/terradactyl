#' Gather TerrADat LPI data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param source Character string. The data source format.
#' @importFrom magrittr %>%
#' @name gather_lpi
#' @family <gather>
#' @return A data frames containing the data from the LPI pin intercepts


#' @export gather_lpi_terradat
#' @rdname gather_lpi
## Function to make tall format of LPI data from TerrADat
gather_lpi_terradat <- function(dsn) {

  # Read LPI information from TerrADat
  lpi_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "tblLPIDetail"
  ))
  lpi_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "tblLPIHeader"
  ))

  # Make a tall data frame with the hit codes by layer and the checkbox designation
  lpi_hits_tall <- lpi_detail %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(
      "PrimaryKey",
      "PointLoc",
      "PointNbr",
      "RecKey",
      "ShrubShape",
      "TopCanopy",
      "SoilSurface", dplyr::matches("^Lower")
    ) %>%

    tidyr::gather(
      key = "layer",
      value = "code",
      "TopCanopy", "SoilSurface", dplyr::matches("^Lower")
    )

  # Remove all records where no hit was recorded (e.g., "None", "NA"

  lpi_hits_tall <- dplyr::filter(
    .data = lpi_hits_tall,
    !is.na(code),
    code != "",
    code != "None",
    !is.na(PrimaryKey),
    !is.na(RecKey)
  )


  ## Make a tall data frame the checkbox status by layer

  lpi_chkbox_tall <- lpi_detail %>%
    dplyr::select(
      "PrimaryKey",
      "PointLoc",
      "PointNbr",
      "RecKey",
      dplyr::matches("^Chkbox")
    ) %>%
    tidyr::gather(
      key = "layer",
      value = "chckbox",
      dplyr::matches("^Chkbox")
    )

  # Remove Woody and Herbaceous Checkbox
  lpi_chkbox_tall <- lpi_chkbox_tall[!(lpi_chkbox_tall$chckbox %in%
    c(
      "ChckboxWoody",
      "ChckboxHerbaceous"
    )), ]

  ## Make the names in the layer variable match
  lpi_chkbox_tall$layer <- gsub(lpi_chkbox_tall$layer,
    pattern = "^Chkbox",
    replacement = ""
  )

  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Top"] <- "TopCanopy"
  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Soil"] <- "SoilSurface"

  # Print update because this function can take a while
  message("Merging LPI Header and LPI Detail tables")

  # Merge checkbox and hit data as well as the header data
  lpi_tall <- suppressWarnings(dplyr::left_join(
    x = lpi_hits_tall,
    y = lpi_chkbox_tall,
    all.x = TRUE,
    by = c("PrimaryKey", "PointLoc", "PointNbr", "RecKey", "layer")
  ) %>%
    dplyr::left_join(
      x = dplyr::select(
        lpi_header,
        "LineKey":"CheckboxLabel",
        "PrimaryKey",
        "DBKey"
      ),
      y = .,
      by = c("PrimaryKey", "RecKey")
    ))

  return(lpi_tall)
  ## Output the list
}

#' @export gather_lpi_lmf
#' @rdname gather_lpi

gather_lpi_lmf <- function(dsn,
                           file_type = "gdb") {

  # Read  PINTERCEPT table in .txt or .gdb or from a preformatted csv

  pintercept <- switch(file_type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn = dsn, layer = "PINTERCEPT"))
    },
    "txt" = {
      utils::read.table(paste(dsn, "pintercept.txt", sep = ""),
        stringsAsFactors = FALSE,
        strip.white = TRUE,
        header = FALSE, sep = "|"
      )
      },
      "csv" = {
        read.csv(file = dsn, header = TRUE)
      }

  )

  # if it is in a text file, there are no field names assigned.
  if (file_type == "txt") {
    colnames <- terradactyl::nri.data.column.explanations$FIELD.NAME[
      terradactyl::nri.data.column.explanations$TABLE.NAME == "PINTERCEPT"
    ]

    colnames <- colnames[1:ncol(pintercept)] %>% subset(!is.na(.))
    names(pintercept) <- colnames
  }

  # remove any NA field names that may have been introduced
  pintercept <- pintercept[, !is.na(colnames(pintercept))]


  # For line point intercept data (cover calculations--
  # point number 75 is recorded twice—once on each transect.
  # We only want to use it once in the calculations.
  # Prior to doing these calculations, it would be beneficial to
  # remove one of the point 75’s from the data set.
  # Remove the nesw transect—that would be all rows in pintercept
  # where transect == “nesw” AND mark = 75.
  pintercept <- pintercept[!(pintercept$TRANSECT== "nesw" & pintercept$MARK == 75),]


  # Where there is a Soil hit, LMF records "None" in BASAL and leaves NONSOIL
  # blank. Let's fill in an "S" to indicate soil
  levels(pintercept$NONSOIL) <- c(levels(pintercept$NONSOIL), "S")
  pintercept$NONSOIL[pintercept$BASAL == "None" &
                       pintercept$NONSOIL == ""] <- "S"
  pintercept$NONSOIL[pintercept$BASAL == "None" &
                      is.na(pintercept$NONSOIL)] <- "S"


  # Identify the pin drop variables
  pin_drop <- c(
    colnames(pintercept)[grepl(pattern = "^HIT[1-6]$",
                               x = colnames(pintercept))],
    "BASAL",
    "NONSOIL"
  )

  # Remove unneeded columns
  pintercept <- pintercept[,!colnames(pintercept) %in% c("SURVEY","COUNTY",
                                                        "PSU","POINT",
                                                        "created_user",
                                                        "created_date",
                                                        "last_edited_user",
                                                        "last_edited_date",
                                                        "GlobalID", "X")]



  # Create a tall table
  lpi_hits_tall <- tidyr::gather(
    data = pintercept,
    key = "layer",
    value = "code",
    pin_drop)



  # Remove blank fields with no data
  lpi_hits_tall <- lpi_hits_tall %>% subset("code" != "")

  # Rename "BASAL" and "NONSOIL" to "SoilSurface"
  lpi_hits_tall$layer <- stringr::str_replace_all(
    string = lpi_hits_tall$layer,
    pattern = "BASAL|NONSOIL",
    replacement = "SoilSurface"
  )

  # Remove "None" and NA values from SoilSurface
  lpi_hits_tall$code[lpi_hits_tall$layer == "SoilSurface" &
                       lpi_hits_tall$code =="None"] <- NA
  lpi_hits_tall <- lpi_hits_tall %>% subset(!is.na(code))

  # Rename "Hit1" as "TopCanopy"
  lpi_hits_tall$layer <- stringr::str_replace_all(
    string = lpi_hits_tall$layer,
    pattern = "HIT1",
    replacement = "TopCanopy"
  )

  # rename Hit2-Hit6 as "LowerLayer
  lpi_hits_tall$layer <- dplyr::recode(
    lpi_hits_tall$layer,
    "HIT2" = "Lower1",
    "HIT3" = "Lower2",
    "HIT4" = "Lower3",
    "HIT5" = "Lower4",
    "HIT6" = "Lower5"

  )



  # Change "Transect and Mark to common names to DIMA schema

  lpi_hits_tall <- dplyr::rename(lpi_hits_tall,
                                 "LineKey" = "TRANSECT",
                                 "PointNbr" = "MARK",
                                 "ShrubShape" = "SAGEBRUSH_SHAPE")

  # Convert to factor
  lpi_hits_tall <- lpi_hits_tall %>%
    dplyr::mutate_if(is.character, list(factor))

  # Convert ShrubShape values to be consistent with DIMA schema,
  # 1==Columnar, 2=Spreading, 3=Mixed, 0 is NA
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 1] <- "C"
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 2] <- "S"
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 3] <- "M"
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 0] <- NA

  return(lpi_hits_tall)
}

#' @export gather_lpi
#' @rdname gather_lpi

# Wrapper gather.lpi function
gather_lpi <- function(dsn,
                       file_type = "gdb",
                       source) {
  # Check for a valid source
  try(
    !toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"),
    stop("No valid source provided")
  )

  # Gather LPI using the appropriate gather function
  lpi <- switch(toupper(source),
    "AIM" = gather_lpi_terradat(dsn = dsn),
    "TERRADAT" = gather_lpi_terradat(dsn = dsn),
    "DIMA" = gather_lpi_terradat(dsn = dsn),
    "LMF" = gather_lpi_lmf(dsn = dsn, file_type = file_type),
    "NRI" = gather_lpi_lmf(dsn = dsn, file_type = file_type)
  )

  # Add source field
  lpi$source <- toupper(source)

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  change_vars <- names(lpi)[class(lpi) %in%
    c("POSIXct", "POSIXt")]

  # Update fields
  lpi <- dplyr::mutate_at(lpi, dplyr::vars(change_vars),
                          list(as.character))

  return(lpi)
}
