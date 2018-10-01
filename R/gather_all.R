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
                       method = c("gap", "lpi", "species.inventory"),
                       append = TRUE,
                       source = c("AIM", "LMF"),
                       file.type = "gdb") {

  # Future functionality:Subset by PrimaryKey and/or DBKey

  # Gather Gap and write to a text file
  if ("gap" %in% method) {

    # gather new gap data
    gap <- gather.gap(
      dsn = dsn,
      source = source,
      file.type = file.type
    )

    # Look for duplicates in original
    # If append=TRUE read in the existing data to join the new data to.
    if (append) {
      old.gap <- sf::st_read(out, layer = "tall_gap")

      gap <- dplyr::full_join(gap, old.gap)
    }
    arcgisbinding::arc.write(
      path = paste(out, "/tall_gap", sep = ""),
      data = gap,
      overwrite = TRUE
    )
  }

  # Gather LPI and write to text file
  if ("lpi" %in% method) {
    # Gather LPI data
    lpi <- gather.lpi(
      dsn = dsn,
      file.type = "gdb",
      source = source
    )
    # If Append=TRUE, read on the original data and join the new data to it.
    if (append) {
      old.lpi <- sf::st_read(out, layer = "tall_lpi")

      lpi <- dplyr::full_join(lpi, old.lpi) %>% dplyr::distinct()
    }
    # Write  out to data
    arcgisbinding::arc.write(
      path = paste(out, "/tall_lpi", sep = ""),
      data = lpi,
      overwrite = TRUE
    )
  }
}
