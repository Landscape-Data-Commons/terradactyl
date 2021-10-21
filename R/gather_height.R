#' Convert height data into a tall, tidy data frame
#' 
#' @description Given wide format line-point intercept data, create a tall 
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file 
#' extension) of the geodatabase or text file containing the table of interest.  
#' This field is unnecessary if you provide either both of tblLPIDetail and 
#' tblLPIHeader (AIM/DIMA/TerrADat) or PASTUREHEIGHTS (LMF/NRI).
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
#' @param PASTUREHEIGHTS Dataframe of the data structure PASTUREHEIGHTS from the
#' LMF/NRI database; alternately provide dsn.
#' @param file_type Character string that denotes the source file type of the 
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for 
#' AIM/DIMA/TerrADat, or if PASTUREHEIGHTS is provided.
#' @importFrom magrittr %>%
#' @name gather_height
#' @family <gather>
#' @return A tall data frame containing the data from the height measurements.
#' @examples
#' gather_height(dsn = "Path/To/AIM_Geodatabase.gdb", 
#'               source = "AIM")
#' gather_height(dsn = "Path/To/LMF_Geodatabase.gdb", 
#'               source = "LMF")
#' 
#' aim_lpidetail <- read.csv("Path/To/tblLPIDetail.csv")
#' aim_lpiheader <- read.csv("Path/To/tblLPIHeader.csv")
#' gather_height(source = "AIM", 
#'               tblLPIDetail = aim_lpidetail, 
#'               tblLPIHeader = aim_lpiheader)
#' 
#' lmf_heights <- read.csv("Path/To/PASTUREHEIGHTS.csv")
#' gather_height(source = "LMF", 
#'               PASTUREHEIGHTS = lmf_heights)

## Gather Height Data
#' @export gather_height_terradat
#' @rdname gather_height
gather_height_terradat <- function(dsn = NULL,
                                   tblLPIDetail= NULL, 
                                   tblLPIHeader = NULL) {
  
  if(!is.null(tblLPIDetail) & !is.null(tblLPIHeader)){
    lpi_detail <- tblLPIDetail
    lpi_header <- tblLPIHeader
  } else if(!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
    }
    
    # Read in the LPI tables from the geodatabase
    lpi_detail <- suppressWarnings(sf::st_read(
      dsn = dsn, layer = "tblLPIDetail",
      stringsAsFactors = FALSE, quiet = T
    ))
    lpi_header <- suppressWarnings(sf::st_read(
      dsn = dsn, layer = "tblLPIHeader",
      stringsAsFactors = FALSE, quiet = T
    ))
  } else {
    stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing those tables")  }
  
  if (any(colnames(lpi_header) %in% "DBKey")) {
    levels <- rlang::quos(PrimaryKey, "DBKey")
  } else {
    levels <- rlang::quos(PrimaryKey)
  }
  
  # we only want to carry a subset of the lpi_header fields forward
  lpi_header <- dplyr::select(lpi_header, !!!levels, LineKey:CheckboxLabel)
  
  lpi_height_tall_woody <- dplyr::select(
    .data = lpi_detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Woody$")
  ) %>% dplyr::mutate(type = "woody")
  # Strip out the extra name stuff so woody and herbaceous variable names match.
  names(lpi_height_tall_woody) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody),
    pattern = "Woody$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi_height_tall_woody$GrowthHabit_measured <- "Woody"
  
  # Herbaceous height
  lpi_height_tall_herb <- dplyr::select(
    .data = lpi_detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Herbaceous$")
  ) %>% dplyr::mutate(type = "herbaceous")
  names(lpi_height_tall_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_herb),
    pattern = "Herbaceous$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi_height_tall_herb$GrowthHabit_measured <- "NonWoody"
  
  # Gather lower herbaceous heights
  lpi_height_tall_lower_herb <- dplyr::select(
    .data = lpi_detail,
    !!!levels,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("LowerHerb$")
  ) %>% dplyr::mutate(type = "lower.herbaceous")
  names(lpi_height_tall_lower_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_lower_herb),
    pattern = "LowerHerb$",
    replacement = ""
  )
  # Add observed growth habit field
  lpi_height_tall_lower_herb$GrowthHabit_measured <- "NonWoody"
  
  # Merge all three gather types together
  lpi_height <- rbind(
    lpi_height_tall_woody,
    lpi_height_tall_herb,
    lpi_height_tall_lower_herb
  )
  lpi_height <- lpi_height %>%
    dplyr::full_join(x = ., y = lpi_header, by = c("PrimaryKey", "DBKey", "RecKey")) %>%
    subset(., !is.na(Height))
  
  # Add NA to fields with no species
  lpi_height$Species[!grepl(pattern = "[[:digit:]]|[[:alpha:]]", lpi_height$Species)] <- NA
  
  # Remove orphaned records and duplicates, if they exist
  lpi_height <- unique(lpi_height)
  lpi_height <- lpi_height[!is.na(lpi_height$PrimaryKey), ]
  
  # Make sure height is a numeric field
  lpi_height$Height <- suppressWarnings(as.numeric(lpi_height$Height))
  
  # final drop
  lpi_height <- lpi_height %>% 
    dplyr::select_if(!names(.) %in% c(
      'DataErrorChecking', 'DataEntry',
       'DateModified', 'FormType', 'Observer', 'Recorder')
  )
  
  
  # Output the woody/herbaceous level data
  return(lpi_height)
}

#' @export gather_height_lmf
#' @rdname gather_height

# Gather Height for LMF/NRI
gather_height_lmf <- function(dsn = NULL,
                              file_type = "gdb",
                              PASTUREHEIGHTS = NULL) {
  
  if(!is.null(PASTUREHEIGHTS)){
    vegheight <- PASTUREHEIGHTS
  } else if (!is.null(dsn)) {
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing PASTUREHEIGHTS")
    }
    
    # Read in the data as .txt or .gdb
    vegheight <- switch(file_type,
                        "gdb" = {
                          suppressWarnings(sf::st_read(dsn,
                                                       layer = "PASTUREHEIGHTS",
                                                       stringsAsFactors = FALSE,
                                                       quiet = T
                          ))
                        },
                        "txt" = {
                          read.table(paste(dsn, "pastureheights.txt", sep = ""),
                                     stringsAsFactors = FALSE,
                                     header = FALSE,
                                     sep = "|",
                                     strip.white = TRUE
                          )
                        },
                        "csv" = {
                          read.csv(dsn)
                        }
    )
    
    if (file_type == "txt") {
      # if it is in a text file, there are no field names assigned.
      colnames <- subset(
        terradactyl::nri.data.column.explanations,
        TABLE.NAME == "PASTUREHEIGHTS"
      ) %>%
        dplyr::pull(FIELD.NAME) %>%
        unique()
      
      vegheight <- vegheight[seq_len(length(colnames))]
      names(vegheight) <- colnames
      
      # We need to establish and/or fix the PLOTKEY so it exists in a single field.
      vegheight$PrimaryKey <- paste(vegheight$SURVEY,
                                    vegheight$STATE,
                                    vegheight$COUNTY,
                                    vegheight$PSU,
                                    vegheight$POINT,
                                    sep = ""
      )
      
      # Assign DBKey
      vegheight$DBKey <- vegheight$SURVEY
    }
    
    
    
    
  } else {
    stop("Supply either PASTUREHEIGHTS or a path to a gdb containing that table")
  }
  
  # For height data
  # point number 75 is recorded twice—once on each transect.
  # We only want to use it once in the calculations.
  # Prior to doing these calculations, it would be beneficial to
  # remove one of the point 75’s from the data set.
  # Remove the nesw transect—that would be all rows in pintercept
  # where transect == “nesw” AND mark = 75.
  vegheight <- vegheight[!(vegheight$TRANSECT == "nesw" & vegheight$DISTANCE == 75), ]
  
  
  height_woody <- dplyr::select(
    .data = vegheight,
    PrimaryKey,
    DBKey,
    TRANSECT,
    DISTANCE,
    dplyr::matches("^W")
  ) %>% dplyr::mutate(
    type = "woody",
    GrowthHabit_measured = "Woody"
  )
  # remove the "W" from the names
  names(height_woody) <- stringr::str_replace_all(
    string = names(height_woody),
    pattern = "W",
    replacement = ""
  )
  
  height_herbaceous <- dplyr::select(
    .data = vegheight,
    PrimaryKey,
    DBKey,
    TRANSECT,
    DISTANCE,
    dplyr::matches("^H")
  ) %>% dplyr::mutate(
    type = "herbaceous",
    GrowthHabit_measured = "NonWoody"
  )
  
  # remove the "H" from the "HPLANT" field
  names(height_herbaceous)[names(height_herbaceous) == "HPLANT"] <- "PLANT"
  height <- rbind(height_woody, height_herbaceous)
  
  # remove NA values
  height <- subset(height, !is.na(HEIGHT))
  
  
  # The height units are concatenated in the field,
  # separate so that we can convert to metric appopriately
  height <- tidyr::separate(height, "HEIGHT", c("HEIGHT", "UOM"),
                            sep = " ", extra = "drop", fill = "right"
  )
  
  # Convert to metric
  height$HEIGHT <- suppressWarnings(as.numeric(height$HEIGHT))
  
  # convert to centimeters
  height$HEIGHT <- height$HEIGHT * 2.54
  height$UOM[is.na(height$UOM) | height$UOM == "in"] <- "cm"
  height$HEIGHT[height$UOM == "ft"] <- height$HEIGHT[height$UOM == "ft"] * 12
  height$UOM <- "cm"
  
  
  # rename field names
  height <- dplyr::rename(height,
                          LineKey = TRANSECT,
                          PointNbr = DISTANCE,
                          Height = HEIGHT,
                          Species = PLANT,
                          HeightUOM = UOM
  )
  
  # Make sure height is a numeric field
  height$Height <- suppressWarnings(as.numeric(height$Height))
  
  # last drop
  # commented out to preserve code for future modification, but we need linekey
  # height <- height %>% dplyr::select(
  #   -c(LineKey)
  # )
  
  # return height
  return(height)
}

#' Gather Height wrapper for all data types
#' @export gather_height
#' @rdname gather_height

gather_height <- function(dsn = NULL,
                          file_type = "gdb",
                          source,
                          tblLPIDetail = NULL,
                          tblLPIHeader = NULL,
                          PASTUREHEIGHTS = NULL) {
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    height <- gather_height_terradat(
      dsn = dsn, 
      tblLPIHeader = tblLPIHeader, 
      tblLPIDetail = tblLPIDetail
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    height <- gather_height_lmf(
      dsn = dsn, file_type = file_type,
      PASTUREHEIGHTS = PASTUREHEIGHTS
    )
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }
  
  height$source <- toupper(source)
  
  if("sf" %in% class(height)) height <- sf::st_drop_geometry(height)
  
  if (any(class(height) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(height)[do.call(rbind, vapply(height, 
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    height <- dplyr::mutate_at(height, dplyr::vars(change_vars), 
                            dplyr::funs(as.character))
  }
  
  
  # Output height
  return(height)
}
