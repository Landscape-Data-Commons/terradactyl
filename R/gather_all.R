#' Build tall tables for all AIM methods
#' @param dsn String. Filepath to data
#' @param folder Folder location for tall Rdata files
#' @name gather_all
#'
#' @export
#' @rdname gather_all

gather_all <- function(dsn, folder){
  # Gap
  gap_tall_aim <- tryCatch(gather_gap(dsn = dsn, source = "AIM"),
                           error = function(c) NA)
  gap_tall_lmf <- tryCatch(gather_gap(dsn = dsn, source = "LMF"),
                           error = function(c) NA)
  gap_tall <- dplyr::full_join(gap_tall_aim, gap_tall_lmf)
  saveRDS(gap_tall,
          file = paste(folder, "gap_tall.Rdata", sep =""))

  # Soil stability
  soil_stability_tall_aim <- gather_soil_stability(dsn = dsn, source = "AIM")
  soil_stability_tall_lmf <- gather_soil_stability(dsn = dsn, source = "LMF")
  soil_stability_tall <- dplyr::full_join(soil_stability_tall_aim, soil_stability_tall_lmf)
  saveRDS(soil_stability_tall,
          file = paste(folder, "soil_stability_tall.Rdata",sep = ""))

  # LPI
  aim_lpi <- gather_lpi(dsn = dsn, file_type = "gdb", source = "AIM")
  lmf_lpi <- gather_lpi(dsn = dsn, file_type = "gdb", source = "LMF")
  lpi <- dplyr::bind_rows(aim_lpi, lmf_lpi)
  saveRDS(lpi,
          file = paste(folder,"lpi_tall.Rdata", sep = ""))

  # Height
  aim_height <- gather_height(dsn = dsn, file_type = "gdb", source = "AIM")
  lmf_height <- gather_height(dsn = dsn, file_type = "gdb", source = "LMF")
  height <- dplyr::bind_rows(aim_height, lmf_height)
  saveRDS(height,
          file = paste(folder, "height_tall.Rdata", sep = ""))

  # Species inventory
  spp_inventory <- gather_species_inventory(dsn = dsn, source = "AIM", file_type = "gdb")
  spp_inventory_lmf <- gather_species_inventory(dsn = dsn, source = "LMF", file_type = "gdb")
  spp_inventory <- dplyr::bind_rows(spp_inventory, spp_inventory_lmf)
  saveRDS(spp_inventory,
          file = paste(folder, "spp_inventory_tall.Rdata", sep = ""))

  # header
  header_aim <- header_build(dsn = dsn, source = "AIM")
  header_lmf <- header_build(dsn = dsn, source = "LMF")
  header <- dplyr::bind_rows(header_aim, header_lmf)
  saveRDS(header,
          file = paste(folder, "header.Rdata", sep = ""))
}
