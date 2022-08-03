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
                                tblLPIHeader = NULL) {

  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(tblLPIDetail) & !is.null(tblLPIHeader)) {
    lpi_detail <- tblLPIDetail
    lpi_header <- tblLPIHeader
  } else if(!is.null(dsn)){

    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
    }

    lpi_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "tblLPIDetail",
      stringsAsFactors = FALSE, quiet = T
    ))
    lpi_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "tblLPIHeader",
      stringsAsFactors = FALSE, quiet = T
    ))
  } else {
    stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing those tables")
  }

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
    code != "N",
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

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  change_vars <- names(lpi_tall)[class(lpi_tall) %in%
                                   c("POSIXct", "POSIXt")]

  # Update fields
  lpi_tall <- dplyr::mutate_at(
    lpi_tall, dplyr::all_of(dplyr::vars(all_of(change_vars))),
    list(as.character)
  )


  ## drops
  lpi_tall <- lpi_tall %>% dplyr::select_if(!names(.) %in% c(

     "DateModified", "FormType", "FormType",
       "DataEntry", "DataErrorChecking")
  )


  return(lpi_tall)
  ## Output the list
}

#' @export gather_lpi_lmf
#' @rdname gather_lpi

# Gather LPI data from the Landscape Monitoring Framework or NRI
gather_lpi_lmf <- function(dsn = NULL,
                           file_type = "gdb",
                           PINTERCEPT = NULL) {

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
                           suppressWarnings(sf::st_read(
                             dsn = dsn, layer = "PINTERCEPT",
                             stringsAsFactors = FALSE, quiet = T
                           ))
                         },
                         "txt" = {
                           utils::read.table(paste(dsn, "pintercept.txt", sep = ""),
                                             stringsAsFactors = FALSE,
                                             strip.white = TRUE,
                                             header = FALSE, sep = "|"
                           )
                         },
                         "csv" = {
                           read.csv(file = dsn, header = TRUE, stringsAsFactors = FALSE)
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
  } else {
    stop("Supply either PINTERCEPT or the path to a GDB containing that table")
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
  pintercept <- pintercept[!(pintercept$TRANSECT == "nesw" & pintercept$MARK == 75), ]


  # Where there is a Soil hit, LMF records "None" in BASAL and leaves NONSOIL
  # blank. Let's fill in an "S" to indicate soil
  pintercept$NONSOIL[pintercept$BASAL == "None" &
                       pintercept$NONSOIL == ""] <- "S"
  pintercept$NONSOIL[pintercept$BASAL == "None" &
                       is.na(pintercept$NONSOIL)] <- "S"

  # where there is a soil code over BR, retain only the BR
  pintercept$BASAL[pintercept$BASAL != "None" &
                     pintercept$NONSOIL == "BR"] <- "None"


  # Identify the pin drop variables
  pin_drop <- c(
    colnames(pintercept)[grepl(
      pattern = "^HIT[1-6]$",
      x = colnames(pintercept)
    )],
    "BASAL",
    "NONSOIL"
  )

  # Remove unneeded columns
  pintercept <- pintercept[, !colnames(pintercept) %in% c(
    "SURVEY", "COUNTY",
    "PSU", "POINT",
    "created_user",
    "created_date",
    "last_edited_user",
    "last_edited_date",
    "GlobalID", "X"
  )]



  # Create a tall table
  lpi_hits_tall <- tidyr::gather(
    data = pintercept,
    key = "layer",
    value = "code",
    dplyr::all_of(pin_drop)
  )



  # Remove blank fields with no data
  lpi_hits_tall <- lpi_hits_tall %>% subset(code != "")

  # Rename "BASAL" and "NONSOIL" to "SoilSurface"
  lpi_hits_tall$layer <- stringr::str_replace_all(
    string = lpi_hits_tall$layer,
    pattern = "BASAL|NONSOIL",
    replacement = "SoilSurface"
  )

  # Remove "None" and NA values from SoilSurface
  lpi_hits_tall$code[lpi_hits_tall$layer == "SoilSurface" &
                       lpi_hits_tall$code == "None"] <- NA
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
                                 "ShrubShape" = "SAGEBRUSH_SHAPE"
  )

  # # Convert to factor
  # lpi_hits_tall <- lpi_hits_tall %>%
  #   dplyr::mutate_if(is.character, list(factor))

  # Convert ShrubShape values to be consistent with DIMA schema,
  # 1==Columnar, 2=Spreading, 3=Mixed, 0 is NA
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 1] <- "C"
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 2] <- "S"
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 3] <- "M"
  lpi_hits_tall$ShrubShape[lpi_hits_tall$ShrubShape == 0] <- NA

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  change_vars <- names(lpi_hits_tall)[class(lpi_hits_tall) %in%
                                        c("POSIXct", "POSIXt")]

  # Update fields
  lpi_hits_tall <- dplyr::mutate_at(
    lpi_hits_tall, dplyr::all_of(dplyr::vars(all_of(change_vars))),
    list(as.character)
  )

  lpi_hits_tall <- lpi_hits_tall %>% dplyr::select_if(!names(.) %in% c(
    'STATE', 'PLOTKEY')
  )

  return(lpi_hits_tall)
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

#' @export gather_lpi
#' @rdname gather_lpi

# Wrapper gather.lpi function
gather_lpi <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblLPIDetail = NULL,
                       tblLPIHeader = NULL,
                       PINTERCEPT = NULL) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    lpi <- gather_lpi_terradat(dsn = dsn,
                               tblLPIDetail = tblLPIDetail,
                               tblLPIHeader = tblLPIHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    lpi <- gather_lpi_lmf(dsn = dsn,
                          file_type = file_type,
                          PINTERCEPT = PINTERCEPT)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # Add source field
  # lpi$source <- toupper(source)
  lpi$source <- source

  if("sf" %in% class(lpi)) lpi <- sf::st_drop_geometry(lpi)

  if (any(class(lpi) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(lpi)[do.call(rbind, vapply(lpi,
                                                     class))[, 1] %in% c("POSIXct", "POSIXt")]
    lpi <- dplyr::mutate_at(lpi, dplyr::vars(change_vars),
                             dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  lpi <- lpi %>%
    dplyr::select(PrimaryKey, DBKey, LineKey, RecKey, tidyselect::everything())

  return(lpi)
}
