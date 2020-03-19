#' Accumulated species across all methods, by height and cover
#' @param lpi_tall Source of lpi Rdata file
#' @param height_tall Source of height Rdata file
#' @param species_inventory_tall Source of species inventory Rdata file
#' @param species_file File path to species file if you want species attributes or updated species. Geodatabase or csv allowed.
#' @param header Source of header Rdata file
#' @param ... Filtering expression to subset the number of plots
#' @examples
#' # Get a list of all species occurring on a plot across methods (LPI, height, species inventory)
#' # This method also adds cover and height by species. Be aware that sample sizes may be insufficient to make an accurate estimate

#'accumulated_species <- terradactyl::accumulated_species(lpi_tall = "~/AIM/Data/lpi_tall.Rdata",
#'                                                       spp_inventory_tall = "~/AIM/Data/spp_inventory_tall.Rdata",
#'                                                        height_tall = "~/AIM/Data/height_tall.Rdata",
#'                                                        header = "~/AIM/Data/header.Rdata",
#'                                                        species_file = "',
#'                                                        SpeciesState %in% "NM")


#'@rdname accumulated_species
#'@export accumulated_species
#'

accumulated_species <- function (lpi_tall,
                                 height_tall,
                                 spp_inventory_tall,
                                 header,
                                 species_file = "",
                                 dead = TRUE,
                                 source = c("TerrADat", "AIM", "LMF", "NRI"),
                                 ...) {
  # Set the filter expressions
  filter_exprs <- rlang::quos(...)

  # Subset the header by the filter expressions
  header_sub <- readRDS(header) %>% dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(PrimaryKey, PlotID, DBKey, SpeciesState,source)

  # read in LPI and join to species table
  lpi_tall_header <- readRDS(lpi_tall) %>%
    dplyr::left_join(dplyr::select(
      header_sub,
      "PrimaryKey",
      "DBKey",
      "SpeciesState"
    ),
    .,
    by = c("PrimaryKey", "DBKey")
    )

  lpi_species <- species_join(
    data = lpi_tall_header,
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else("TerrADat" %in% source,
                                               TRUE,
                                               FALSE)
  ) %>% dplyr::distinct()

  # calculate cover by species
  species_cover <- pct_cover_species(lpi_tall = lpi_species)%>%
    # Omit 0 cover species
    subset(percent > 0)

  # If dead == TRUE then calculate live and dead hits as well
  if(dead) {
    species_cover_live_dead <- pct_cover_live(lpi_tall = readRDS(lpi_tall) %>%
                                           subset(PrimaryKey %in% header_sub$PrimaryKey),
                                         hit = "any",
                                         tall = TRUE,
                                         by_year = FALSE,
                                         by_line = FALSE,
                                         code) %>% subset(percent > 0)
    species_cover_live_dead_split <- species_cover_live_dead  %>%
      # split out Live and Dead into a separate column
      tidyr::separate(indicator, c( "status", "Species"), sep = "\\.") %>%
      # Add AH as prefix and Cover as a suffix
      dplyr::mutate(status = paste("AH_Species", status, "Cover", sep = "")) %>%
      # Pivot to wide so that Live and Dead are separate fields
      tidyr::pivot_wider(names_from = status,
                         values_from = percent)

    # merge back with species_cover
    species_cover <- dplyr::left_join(species_cover,
                                      species_cover_live_dead_split)
  }

  # add n of hits
  species_cover <- lpi_species %>%
    subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    subset(nchar(as.character(code)) >= 3 & code != "None") %>%
    dplyr::distinct(PrimaryKey, LineKey, PointNbr, code) %>%
    dplyr::count(PrimaryKey, code) %>%
    dplyr::left_join(species_cover, .,
                     by = c("PrimaryKey", "Species" = "code"))



  # Read in height and join species
  height <- readRDS(height_tall) %>%

    # subset by PK and add the SpeciesState from the header
    dplyr::left_join(dplyr::select(header_sub, PrimaryKey, SpeciesState), .)

  # Join to species list
  height_species <- species_join(
    data = height,
    data_code = "Species",
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else("TerrADat" %in% source,
                                               TRUE,
                                               FALSE)
  )

  # calculate height by species
  species_height <- mean_height(height_tall = height_species,
                                method = "mean",
                                by_line = FALSE,
                                omit_zero = TRUE,
                                tall = TRUE,
                                Species)

  # add n of samples for each calculation
  species_height <- height_species %>%
    subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    dplyr::count(PrimaryKey, Species) %>%
    dplyr::left_join(., species_height,
                     by = c("PrimaryKey", "Species" = "indicator")) %>%

    # remove "None" codes
    subset(Species != "None")

  if(dead) {
    species_height_live_dead <- mean_height(height_tall = readRDS(height_tall) %>%
                                              subset(PrimaryKey %in% header_sub$PrimaryKey),
                                            method = "mean",
                                            by_line = FALSE,
                                            omit_zero = TRUE,
                                            tall = TRUE,
                                            Chkbox, Species)
    species_height_live_dead_split <- species_cover_live_dead  %>%
      # Identify 0 as Live and 1 as dead
      dplyr::mutate(indicator = stringr::str_replace_all(indicator,
                                                    c("1\\." = "Dead\\.",
                                                      "0\\." = "Live\\."))
                                                    )  %>%
      # split out Live and Dead into a separate column
      tidyr::separate(indicator, c( "status", "Species"), sep = "\\.") %>%
      # Add AH as prefix and Cover as a suffix
      dplyr::mutate(status = paste("Hgt_Species", status, "_Avg", sep = "")) %>%
      # Pivot to wide so that Live and Dead are separate fields
      tidyr::pivot_wider(names_from = status,
                         values_from = percent)

    # merge back with species_cover
    species_height <- dplyr::left_join(species_height_live_dead_split,
                                       species_height,
                                      by = c("Species" = "indicator",
                                             "PrimaryKey"))
  }


  # read species inventory data and join species list
  species_inventory <- readRDS(spp_inventory_tall) %>%
    # Join to the header to get the relevant PrimaryKeys and SpeciesSate
    dplyr::left_join(dplyr::select(header_sub, PrimaryKey, SpeciesState), .,
                     by = "PrimaryKey"
    )

  # Join to State Species List
  spp_inventory_species <- species_join(
    data = species_inventory,
    data_code = "Species",
    species_file = species_file,
    overwrite_generic_species = dplyr::if_else("TerrADat" %in% source,
                                               TRUE,
                                               FALSE)
  )

  # get list of species occurring in species inventory
  species_inventory <- spp_inventory_species %>%
    dplyr::select(PrimaryKey, Species) %>%
    dplyr::distinct()

  # Join height and cover calculations together
  species <- dplyr::full_join(species_cover, species_height,
                              by = c("PrimaryKey", "Species"))

  # find the species that do not occur from the joined species list but are
  # present in the species inventory table and append those to the species list
  all_species <- dplyr::anti_join(species_inventory, species,
                                  by = c("PrimaryKey", "Species")) %>%
    # append to end of the species list
    dplyr::bind_rows(species, .)

  # Remove non-species codes
  all_species <- all_species %>%
    subset(nchar(Species) > 2 & !is.na(Species))

  # back to header
  all_species_header <-dplyr::full_join(header_sub, all_species,
                                        by = "PrimaryKey") %>%
    # create formal output table
    dplyr::rename (AH_SpeciesCover = percent,
                   Hgt_Species_Avg = mean_height,
                   AH_SpeciesCover_n = n.x,
                   Hgt_Species_Avg_n = n.y
                   )



  # if a species list is provided, join to species list
  if (species_file != "") {
    all_species_header <- species_join(data = all_species_header,
                                           data_code = "Species",
                                           species_file = species_file)
  }


}

