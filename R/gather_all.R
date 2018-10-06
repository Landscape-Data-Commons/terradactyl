#' Build tall tables
#' @param dsn. String. Filepath to data
#' @param out
#' @param method String. Methods to append
#' @param append Logical. Append data to existing table
#' @name gather.all
#'
#' @export
#' @rdname gather.all

gather.all <- function(dsn, out,
                       method = c("gap",
                                  "lpi",
                                  "height",
                                  "species inventory",
                                  "soil stability",
                                  "rangeland health"),
                       append = TRUE,
                       source = c("AIM", "LMF"),
                       file.type = "gdb") {

  # Future functionality:Subset by PrimaryKey and/or DBKey

  arcgisbinding::arc.check_product()

  # Gather header information
  header <- dplyr::bind_rows(
    header_build(dsn, source = AIM,  !is.na(PrimaryKey)),
    header_build(dsn, source = AIM,  !is.na(PrimaryKey))
  )

  arcgisbinding::arc.write(
    path = paste(out, "/header", sep = ""),
    data = header,
    overwrite = TRUE )


  # Gather Gap and write to a geodatabase
  if ("gap" %in% method) {

    # gather new gap data
    gap <- dplyr::bind_rows(
      gather.gap(dsn = dsn,
                 source = "AIM",
                 file.type = file.type),
      gather.gap(dsn = dsn,
                 source = "LMF",
                 file.type = file.type)
    )

    # # Look for duplicates in original
    # # If append=TRUE read in the existing data to join the new data to.
    # if (append) {
    #   old.gap <- sf::st_read(out, layer = "tall_gap")
    #
    #   gap <- dplyr::full_join(gap, old.gap)
    # }
    arcgisbinding::arc.write(
      path = paste(out, "/gap_tall", sep = ""),
      data = gap,
      overwrite = TRUE
    )
  }

  # Gather LPI and write to text file
  if ("lpi" %in% method) {
    # Gather LPI data
    lpi <- dplyr::bind_rows(gather.lpi(dsn = dsn,
                                       file.type = "gdb",
                                       source = "AIM"),
                            gather.lpi(dsn = dsn,
                                       file.type = "gdb",
                                       source = "LMF")

    )

    # # If Append=TRUE, read on the original data and join the new data to it.
    # if (append) {
    #   old.lpi <- sf::st_read(out, layer = "tall_lpi")
    #
    #   lpi <- dplyr::full_join(lpi, old.lpi) %>% dplyr::distinct()
    # }
    # # Write  out to data
    arcgisbinding::arc.write(
      path = paste(out, "/lpi_tall", sep = ""),
      data = lpi,
      overwrite = TRUE
    )
  }
  # Gather LPI and write to text file
  if ("height" %in% method) {
    # Gather LPI data
    height <- dplyr::bind_rows(gather.height(dsn = dsn,
                                             file.type = "gdb",
                                             source = "AIM"),
                               gather.height(dsn = dsn,
                                             file.type = "gdb",
                                             source = "LMF")

    )

    arcgisbinding::arc.write(
      path = paste(out, "/height_tall", sep = ""),
      data = height,
      overwrite = TRUE
    )
  }

  # Gather LPI and write to text file
  if ("species inventory" %in% method) {
    # Gather LPI data
    spp_inventory <- dplyr::bind_rows(gather.species.inventory(dsn = dsn,
                                                               file.type = "gdb",
                                                               source = "AIM"),
                                      gather.species.inventory(dsn = dsn,
                                                               file.type = "gdb",
                                                               source = "LMF")
    )

    arcgisbinding::arc.write(
      path = paste(out, "/spp_inventory_tall", sep = ""),
      data = spp_inventory,
      overwrite = TRUE )

} }
