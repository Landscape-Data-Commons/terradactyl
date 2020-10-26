#' QC Data Inventory
#'

#' @export qc_data_inventory
#' @rdname qc_data_inventory
qc_data_inventory <- function(header,
                              lpi_tall,
                              gap_tall,
                              height_tall,
                              spp_inventory_tall,
                              soil_stability_tall,
                              dsn) {
  header <- readRDS(header) %>% dplyr::select(-DateVisited)
  lpi <- readRDS(lpi_tall) %>%
    dplyr::group_by(PrimaryKey, RecKey) %>%
    dplyr::filter(layer == "SoilSurface") %>%
    dplyr::tally(name = "n_lpi_hits") %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::add_tally(name = "n_lpi_lines") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey, n_lpi_lines) %>%
    dplyr::summarise(n_lpi_hits = sum(n_lpi_hits))

  gap <- readRDS(gap_tall) %>%
    dplyr::select(PrimaryKey, RecKey) %>%
    dplyr::distinct() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::tally(name = "n_gap_lines")

  height <- readRDS(height_tall) %>%
    dplyr::group_by(PrimaryKey, RecKey) %>%
    dplyr::filter(!is.na(Height)) %>%
    dplyr::tally(name = "n_height_hits") %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::add_tally(name = "n_height_lines") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey, n_height_lines) %>%
    dplyr::summarise(n_height_hits = sum(n_height_hits))

  spp_inventory <- readRDS(spp_inventory_tall) %>%
    dplyr::select(PrimaryKey, RecKey) %>%
    dplyr::distinct() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::tally(name = "n_species_inventory_lines")

  soil_stability <- readRDS(soil_stability_tall) %>%
    dplyr::select(PrimaryKey, RecKey) %>%
    dplyr::distinct() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::tally(name = "n_soil_stability_lines")

  rangehealth <- gather_rangeland_health(dsn = dsn, source = "TerrADat") %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::tally(name = "n_rangeland_health")

  # Merge it all together
  data_inventory <- list(header, lpi, height, gap, spp_inventory, soil_stability, rangehealth) %>%
    Reduce(function(dtf1, dtf2) dplyr::full_join(dtf1, dtf2, by = "PrimaryKey"), .)
}
