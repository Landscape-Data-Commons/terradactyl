#' Convert species inventory data into tall, tidy data frame
#' 
#' @description Given species inventory data create a tall format data frame 
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file 
#' extension) of the geodatabase containing the table of interest. This field 
#' is unnecessary if you supply either both of tblSpecRichDetail and 
#' tblSpecRichHeader (AIM/DIMA/TerrADat) or PLANTCENSUS (LMF/NRI).
#' @param source Character string. The data source format, 
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblSpecRichDetail Dataframe of the data structure tblSpecRichDetail 
#' from the DIMA database with the addition of PrimaryKey and DBKey fields. 
#' Use with tblSpecRichHeader when data source is AIM, DIMA, or TerrADat; 
#' alternately provide dsn.
#' @param tblSpecRichHeader Dataframe of the data structure tblSpecRichHeader 
#' from the DIMA database with the addition of PrimaryKey and DBKey fields. 
#' Use with tblSpecRichDetail when data source is AIM, DIMA, or TerrADat; 
#' alternately provide dsn.
#' @param PLANTCENSUS Dataframe of the data structure PLANTCENSUS from LMF/NRI 
#' database with the addition of PrimaryKey and DBKey fields. Use when data 
#' source is LMF or NRI; alternately provide dsn. 
#' @importFrom magrittr %>%
#' @name gather_species_inventory
#' @family <gather>
#' @return A tall data frame containing species inventory data.
#' @examples 
#' gather_species_inventory(dsn = "Path/To/AIM_Geodatabase.gdb", 
#'                          source = "AIM")
#' gather_species_inventory(dsn = "Path/To/LMF_Geodatabase.gdb", 
#'                          source = "LMF")
#' 
#' aim_specrichdetail <- read.csv("Path/To/tblSpecRichDetail.csv")
#' aim_specrichheader <- read.csv("Path/To/tblSpecRichHeader.csv")
#' gather_species_inventory(source = "AIM", 
#'                          tblSpecRichDetail = aim_specrichdetail, 
#'                          tblSpecRichHeader = aim_specrichheader)
#' 
#' lmf_census <- read.csv("Path/To/PLANTCENSUS.csv")
#' gather_species_inventory(source = "LMF", 
#'                          PLANTCENSUS = lmf_census)

#' @export gather_species_inventory_terradat
#' @rdname gather_species_inventory
gather_species_inventory_terradat <- function(dsn = NULL, 
                                              tblSpecRichDetail = NULL, 
                                              tblSpecRichHeader = NULL) {
  
  if(!is.null(tblSpecRichDetail) & !is.null(tblSpecRichHeader)) {
    species_inventory_detail <- tblSpecRichDetail
    species_inventory_header <- tblSpecRichHeader
  } else if (!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblSpecRichDetail and tblSpecRichHeader")
    }
    
    
    # load raw tables
    species_inventory_detail <- suppressWarnings(sf::st_read(dsn,
                                                             layer = "tblSpecRichDetail",
                                                             stringsAsFactors = FALSE, quiet = T
    ))
    species_inventory_header <- suppressWarnings(sf::st_read(dsn,
                                                             layer = "tblSpecRichHeader",
                                                             stringsAsFactors = FALSE, quiet = T
    ))
    
  } else {
    stop("Supply either tblSpecRichDetail and tblSpecRichHeader, or the path to a GDB containing those tables")
  }
  
  
  # Make Species Inventory Detail  a tall dataframe
  species_detail_tall <- tall_species(species_inventory_detail = species_inventory_detail)
  
  # Join with header data and strip out NA codes
  species_inventory_tall <- dplyr::left_join(
    x = species_inventory_header,
    y = species_detail_tall,
    by = c("RecKey", "PrimaryKey")
  ) %>%
    subset(!is.na(Species)) %>% 
    dplyr::select(
    -c(LineKey, RecKey, DateModified, FormType, Observer, Recorder, DataEntry, 
       DataErrorChecking, DateLoadedInDb, created_user, created_date, last_edited_user, last_edited_date, GlobalID)
  )
  
  return(species_inventory_tall)
}
#' @export species_count
#' @rdname gather_species_inventory
species_count <- function(species_inventory_tall, ...) {
  grouping_variables <- rlang::quos(...)
  
  if ("DBKey" %in% colnames(species_inventory_tall)) {
    levels <- rlang::quos(DBKey, PrimaryKey)
  } else {
    levels <- rlang::quos(PrimaryKey)
  }
  
  # make sure that there are a unique set of species for each grouping level
  species_inventory_tall <- species_inventory_tall %>%
    dplyr::select(
      !!!grouping_variables,
      !!!levels,
      Species
    ) %>%
    unique()
  
  species_count <- species_inventory_tall %>%
    dplyr::count(!!!levels, !!!grouping_variables) %>%
    tidyr::unite(indicator, !!!grouping_variables, sep = ".") %>%
    dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
  
  
  
  return(species_count)
}

#' @export tall_species
#' @rdname gather_species_inventory
tall_species <- function(species_inventory_detail) {
  tall_list <- lapply(1:nrow(species_inventory_detail), FUN = function(X, df) {
    # split species strings concatenated in a single field
    codes <- stringr::str_split(df[X, "SpeciesList"], pattern = ";")[[1]]
    
    # Format output
    output <- data.frame(
      "PrimaryKey" = df$PrimaryKey[X],
      "RecKey" = df$RecKey[X],
      "Species" = codes
    )
    return(output)
  }, df = species_inventory_detail)
  # Combine output
  output <- dplyr::bind_rows(tall_list)
  
  # Remove NAs and blanks
  output <- dplyr::filter(output, !(Species %in% c("", NA)))
  
  return(output)
}

# Gather LMF data
#' @export gather_species_inventory_lmf
#' @rdname gather_species_inventory
gather_species_inventory_lmf <- function(dsn = NULL, 
                               file_type = "gdb", 
                               PLANTCENSUS = NULL) {
  if(!is.null(PLANTCENSUS)){
    plantcensus <- PLANTCENSUS
  } else if(!is.null(dsn)){
    
    plantcensus <- switch(file_type,
                          "gdb" = {
                            suppressWarnings(sf::st_read(dsn,
                                                         layer = "PLANTCENSUS",
                                                         stringsAsFactors = FALSE, quiet = T
                            ))
                          },
                          "txt" = {
                            read.table(paste(dsn, "plantcensus.txt", sep = ""),
                                       stringsAsFactors = FALSE,
                                       header = FALSE, sep = "|",
                                       strip.white = TRUE
                            )
                          },
                          "csv" = {
                            read.csv(dsn,
                                     stringsAsFactors = FALSE
                            )
                          }
    )
    
    # if it is in a text file, there are no field names assigned.
    if (file_type == "txt") {
      plantcensus <- name_variables_nri(
        data = plantcensus,
        table_name = "PLANTCENSUS"
      )
    }
    
  } else {
    stop("Supply either PLANTCENSUS or the path to a GDB containing that table")
  }
  
  # Get species count
  species_inventory <- plantcensus %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(., SpeciesCount = dplyr::n(), .groups = "drop") %>%
    merge(., plantcensus)
  
  # rename fields
  species_inventory <- dplyr::rename(species_inventory,
                                     Species = CPLANT
  ) %>% dplyr::select(., -c(SURVEY:SEQNUM, GlobalID, created_user,
                            created_date, last_edited_user, last_edited_date))
  
  return(species_inventory)
}

#' Species Inventory Gather wrapper
#' @export gather_species_inventory
#' @rdname gather_species_inventory

gather_species_inventory <- function(dsn = NULL, 
                                     source, 
                                     tblSpecRichDetail = NULL,
                                     tblSpecRichHeader = NULL, 
                                     PLANTCENSUS = NULL,
                                     file_type = "gdb") {
  
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    species_inventory <- gather_species_inventory_terradat(
      dsn = dsn, 
      tblSpecRichDetail = tblSpecRichDetail, 
      tblSpecRichHeader = tblSpecRichHeader
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    species_inventory <- gather_species_inventory_lmf(
      dsn = dsn, file_type = file_type,
      PLANTCENSUS = PLANTCENSUS
    )
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }
  
  # Add source field so that we know where the data came from
  species_inventory$source <- toupper(source)
  
  if("sf" %in% class(species_inventory)) species_inventory <- sf::st_drop_geometry(species_inventory)
  
  return(species_inventory)
}
