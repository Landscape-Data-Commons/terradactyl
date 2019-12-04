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
                                 ...) {
  # Set the filter expressions
  filter_exprs <- rlang::quos(...)

  # Subset the header by the filter expressions
  header_sub <- readRDS(header) %>% dplyr::filter(!!!filter_exprs)

  # calculate cover by species
  species_cover <- pct_cover_species(lpi_tall = readRDS(lpi_tall) %>%
                                       subset(PrimaryKey %in% header_sub$PrimaryKey))%>%
    # Omit 0 cover species
    subset(percent >0)

  # add n of hits
  species_cover<- readRDS(lpi_tall) %>%
    subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    subset(nchar(as.character(code)) >= 3 & code != "None") %>%
    dplyr::distinct(PrimaryKey, LineKey, PointNbr, code) %>%
    dplyr::count(PrimaryKey, code) %>%
    dplyr::left_join(species_cover, .,
                     by = c("PrimaryKey", "Species" = "code"))


  # calculate height by species
  species_height <- mean_height(height_tall = readRDS(height_tall) %>%
                                  subset(PrimaryKey %in% header_sub$PrimaryKey),
                                method = "mean",
                                by_line = FALSE,
                                omit_zero = TRUE,
                                tall = TRUE,
                                Species)

  # add n of samples for each calculation
  species_height <- readRDS(height_tall) %>%
    subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    dplyr::count(PrimaryKey, Species) %>%
    dplyr::left_join(species_height, .,
                     by = c("PrimaryKey", "indicator" = "Species")) %>%

    # remove "None" codes
    subset(indicator != "None")



  # get list of species occurring in species inventory
  species_inventory <- readRDS(spp_inventory_tall) %>%
    subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    dplyr::select(PrimaryKey, Species) %>%
    dplyr::distinct()

  # Join height and cover calculations together
  species <- dplyr::full_join(species_cover, species_height,
                              by = c("PrimaryKey", "Species" = "indicator"))

  # find the species that do not occur from the joined species list but are
  # present in the species inventory table and append those to the species list
  all_species <- dplyr::anti_join(species_inventory, species,
                                  by = c("PrimaryKey", "Species")) %>%
    # append to end of the species list
    dplyr::bind_rows(species, .)

  # back to header
  all_species_header <-dplyr::full_join(header_sub, all_species,
                                        by = "PrimaryKey") %>%
    # create formal output table
    dplyr::select (PrimaryKey,
                   PlotID,
                   DBKey,
                   Species,
                   AH_SpeciesCover = percent,
                   Hgt_Species_Avg = mean_height,
                   AH_SpeciesCover_n = n.x,
                   Hgt_Species_Avg_n = n.y,
                   SpeciesState,
                   source)


  # if a species list is provided, join to species list
  if (species_file != "") {
    all_species_header <- species_join(data = all_species_header,
                                           data_code = "Species",
                                           species_file = species_file) %>%
      dplyr::select(PrimaryKey,
                    PlotID,
                    DBKey,
                    Species,
                    AH_SpeciesCover,
                    Hgt_Species_Avg,
                    AH_SpeciesCover_n,
                    Hgt_Species_Avg_n,
                    GrowthHabit,
                    GrowthHabitSub,
                    Duration,
                    Noxious,
                    SG_Group,
                    SpeciesState,
                    source)
  }


}

