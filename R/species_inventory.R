#' Species Inventory
#' @description Create a tall species inventory data frame.
#' @param dsn Character string. The full filepath and filename (including file extension)
#' of the geodatabase containing the table of interest.
#' @param source Character string. Data format source,
#' \code{"AIM", "TerrADat", "LMF", "NRI"} are all valid options.
#' @param ... Grouping variables for species counts
#' @return A data frames containing the data from the species inventory data in tall format.


#' @export gather_species_inventory_terradat
#' @rdname species_inventory



gather_species_inventory_terradat <- function(dsn) {
  # load raw tables
  species_inventory_detail <- suppressWarnings(sf::st_read(dsn,
                                                           layer = "tblSpecRichDetail",
                                                           stringsAsFactors = FALSE))
  species_inventory_header <- suppressWarnings(sf::st_read(dsn,
                                                           layer = "tblSpecRichHeader",
                                                           stringsAsFactors = FALSE))

  # Make Species Inventory Detail  a tall dataframe
  species_detail_tall <- tall_species(species_inventory_detail = species_inventory_detail)

  # Join with header data and strip out NA codes
  species_inventory_tall <- dplyr::left_join(x = species_inventory_header,
                                             y = species_detail_tall) %>%
    subset(!is.na(Species))

  return(species_inventory_tall)
}

#' @export species_count
#' @rdname species_inventory
species_count <- function(species_inventory_tall, ...) {
  grouping_variables <- rlang::quos(...)

  if ("DBKey" %in% colnames(species_inventory_tall)) {
    levels <- rlang::quos(DBKey, PrimaryKey)
  } else {
    levels <- rlang::quos(PrimaryKey)
  }

  # make sure that there are a unique set of species for each grouping level
  species_inventory_tall <- species_inventory_tall %>%
    dplyr::select(!!!grouping_variables,
                  !!!levels,
                  Species) %>%
    unique()

  species_count <- species_inventory_tall %>%
    dplyr::count(!!!levels, !!!grouping_variables) %>%
    tidyr::unite(indicator, !!!grouping_variables, sep = ".") %>%
    dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))



  return(species_count)
}
#' @export tall_species
#' @rdname species_inventory
tall_species <- function(species_inventory_detail) {
  tall_list <- lapply(1:nrow(species_inventory_detail), FUN = function(X, df) {
    # split species strings concatenated in a single field
    codes <- stringr::str_split(df[X, "SpeciesList"], pattern = ";")[[1]]

    # Format output
    output <- data.frame("PrimaryKey" = df$PrimaryKey[X],
                         "RecKey" = df$RecKey[X],
                         "Species" = codes)
    return(output)
  }, df = species_inventory_detail)
  # Combine output
  output <- dplyr::bind_rows(tall_list)

  # Remove NAs and blanks
  output <- dplyr::filter(output, !(Species %in% c("", NA)))

  return(output)
}


# Gather LMF data
#' @export gather_species_lmf
#' @rdname species_inventory
gather_species_lmf <- function(dsn, file_type = "gdb") {
  plantcensus <- switch(file_type,
                        "gdb" = {
                          suppressWarnings(sf::st_read(dsn,
                                                       layer = "PLANTCENSUS",
                                                       stringsAsFactors = FALSE))
                        },
                        "txt" = {
                          read.table(paste(dsn, "plantcensus.txt", sep = ""),
                                     stringsAsFactors = FALSE,
                                     header = FALSE, sep = "|",
                                     strip.white = TRUE)
                        },
                        "csv" = {
                          read.csv(dsn,
                                   stringsAsFactors = FALSE)
                        }
  )


  # if it is in a text file, there are no field names assigned.
  if (file_type == "txt") {
    plantcensus <- name_variables_nri(data = plantcensus,
                                      table_name = "PLANTCENSUS")
  }


  # Get species count
  species_inventory <- plantcensus %>% dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(., SpeciesCount = dplyr::n()) %>%
    merge(., plantcensus)

  # rename fields
  species_inventory <- dplyr::rename(species_inventory,
                                     Species = CPLANT
  ) %>% dplyr::select(., -c(SURVEY:SEQNUM))

  return(species_inventory)
}


#' Species Inventory Gather wrapper
#' @export gather_species_inventory
#' @rdname species_inventory

gather_species_inventory <- function(dsn, source, file_type = "gdb") {

  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"))
    stop("No valid source provided"))

  # Gather species_inventory using the appropriate method
  species_inventory <- switch(toupper(source),
                              "AIM" = gather_species_inventory_terradat(dsn = dsn),
                              "TERRADAT" = gather_species_inventory_terradat(dsn = dsn),
                              "DIMA" = gather_species_inventory_terradat(dsn = dsn),
                              "LMF" = gather_species_lmf(dsn = dsn,
                                                         file_type = file_type),
                              "NRI" = gather_species_lmf(dsn = dsn,
                                                         file_type = file_type)
  )

  # Add source field so that we know where the data came from
  species_inventory$source <- toupper(source)

  return(species_inventory)
}
