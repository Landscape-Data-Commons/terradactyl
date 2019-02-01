#' Build tall tables
#' @param dsn. String. Filepath to data
#' @param out Rdata file of all core methods tables in tall.
#' @param method String. Methods to append
#' @param append Logical. Append data to existing table
#' @name gather.all
#'
#' @export
#' @rdname gather.all

# gather.all <- function(dsn, out,
#                        method = c("gap",
#                                   "lpi",
#                                   "height",
#                                   "species inventory",
#                                   "soil stability",
#                                   "rangeland health"),
#                        append = TRUE,
#                        source = c("AIM", "LMF"),
#                        file.type = "gdb") {
#
#   # Future functionality:Subset by PrimaryKey and/or DBKey
#

#   # Gather header information
#   header <- dplyr::bind_rows(
#     header_build(dsn, source = "AIM",  !is.na(PrimaryKey)),
#     header_build(dsn, source = "LMF",  !is.na(PrimaryKey))
#   )
#
#  save(header, file =paste(out, "header.Rdata", sep = ""))
#
#
#   # Gather Gap and write to a geodatabase
#   if ("gap" %in% method) {
#
#     # gather new gap data
#     gap <- dplyr::bind_rows(
#       gather.gap(dsn = dsn,
#                  source = "AIM",
#                  file.type = file.type),
#       gather.gap(dsn = dsn,
#                  source = "LMF",
#                  file.type = file.type)
#     )
#
#     # # Look for duplicates in original
#     # # If append=TRUE read in the existing data to join the new data to.
#     # if (append) {
#     #   old.gap <- sf::st_read(out, layer = "tall_gap")
#     #
#     #   gap <- dplyr::full_join(gap, old.gap)
#     # }
#
#   }
#
#   # Gather LPI and write to text file
#   if ("lpi" %in% method) {
#     # Gather LPI data
#     lpi <- dplyr::bind_rows(gather.lpi(dsn = dsn,
#                                        file.type = "gdb",
#                                        source = "AIM"),
#                             gather.lpi(dsn = dsn,
#                                        file.type = "gdb",
#                                        source = "LMF")
#
#     )
# save(lpi, file =paste(out, "lpi_tall.Rdata", sep = ""))
#
#   # Gather LPI and write to text file
#   if ("height" %in% method) {
#     # Gather LPI data
#     height.terradat <- gather.height(dsn = dsn,
#                                      file.type = "gdb",
#                                      source = "AIM")
#     height.terradat <- height.terradat %>% dplyr::mutate(Height = as.numeric(Height))
#     height.lmf <-gather.height(dsn = dsn,
#                                file.type = "gdb",
#                                source = "LMF")
#
#
#     height <- dplyr::bind_rows(height.terradat, height.lmf)
#
#    save(height, file = paste(out, "height_tall.Rdata", sep = ""))
#   }
#
#   # Gather LPI and write to text file
#   if ("species inventory" %in% method) {
#     # Gather LPI data
#     spp_inventory <- dplyr::bind_rows(gather.species.inventory(dsn = dsn,
#                                                                file.type = "gdb",
#                                                                source = "AIM"),
#                                       gather.species.inventory(dsn = dsn,
#                                                                file.type = "gdb",
#                                                                source = "LMF")
#     )
#
#     save(spp_inventory,
#             file =paste(out, "spp_inventory_tall.Rdata", sep = "") )
#
#   }
#
#
#   }
