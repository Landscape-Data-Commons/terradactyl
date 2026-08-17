#' RHEM Input Calculations
#' @param lpi_species Dataframe of lpi tall table joined with RHEM species attributes in the field "RHEM_Habit".
#' @param slope_shape Dataframe of slope tall table with field SlopeShape = Vertical Slope Shape.
#' @param header Dataframe of header with PrimaryKey, Latitude, and Longitude fields.
#'
#' @examples
#' RHEM(lpi_species = lpi_species,
#' header = header,
#' slope_shape = slope_shape)

#' @export RHEM
#' @rdname RHEM
RHEM <- function(lpi_species,
                 header,
                 slope_shape,
                 verbose = FALSE) {

  #check we have header info for all data
  if (verbose) {
    message("Restricting data to PrimaryKeys found in header.")
  }

  lpi_species <- dplyr::filter(.data = lpi_species,
                               PrimaryKey %in% header$PrimaryKey)
  slope_shape <- dplyr::filter(.data = slope_shape,
                               PrimaryKey %in% header$PrimaryKey)
  # lpi_species <-lpi_species %>% subset(PrimaryKey %in% header$PrimaryKey)
  # slope_shape <- slope_shape %>%  subset(PrimaryKey %in% header$PrimaryKey)

  # Total Foliar Cover
  total_foliar <- pct_cover_total_foliar(lpi_tall = lpi_species,
                                         by_line = FALSE,
                                         tall = TRUE) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(x = c("PrimaryKey",
                                           "FH_TotalFoliarCover" = "percent")))

  # RHEM functional group cover ####
  ah_cover_rhem <- pct_cover(lpi_tall = lpi_species,
                             hit = "any",
                             by_line = FALSE,
                             tall = TRUE,
                             RHEM_Habit) |>
    dplyr::mutate(.data = _,
                  indicator = stringr::str_to_camel(string = indicator,
                                                    first_upper = TRUE) |>
                    stringr::str_c("AH_", .x = _, "Cover")) |>
    tidyr::pivot_wider(data = _,
                       names_from = "indicator",
                       values_from = "percent")

  fh_cover_rhem <- pct_cover(lpi_tall = lpi_species,
                             hit = "first",
                             by_line = FALSE,
                             tall = TRUE,
                             RHEM_Habit) |>
    dplyr::mutate(.data = _,
                  indicator = stringr::str_to_camel(string = indicator,
                                                    first_upper = TRUE) |>
                    stringr::str_c("FH_", .x = _, "Cover")) |>
    tidyr::pivot_wider(data = _,
                       names_from = "indicator",
                       values_from = "percent")

  # These are considered litter for RHEM purposes.
  # Only records where the lowest canopy record was one of these values will
  # be considered for SurfaceLitter calculations.
  recognized_litter_codes <- c("L", "HL", "WL", "EL", "D", "AL", "OM", "AL", "NL")
  # Identify Litter above soil
  lpi_species <- lpi_species |>
    # Make sure that the records are ordered from topmost to bottommost so we
    # can use dplyr::last() to get the lowest during summarization.
    dplyr::mutate(.data = _,
                  layer = factor(layer,
                                 levels = c("TopCanopy",
                                            # Just to be safe, we're going to add 15 Lower options,
                                            # but this should really be more generalized eventually.
                                            paste0("Lower",
                                                   1:15),
                                            # "Lower1",
                                            # "Lower2",
                                            # "Lower3",
                                            # "Lower4",
                                            # "Lower5",
                                            # "Lower6",
                                            # "Lower7",
                                            "SoilSurface"))) |>
    dplyr::arrange(.data = _,
                   layer) |>
    # Remove the SoilSurface record because litter is only in the canopy records
    # and the point here is to create a new LowestCanopy variable with the value
    # "SurfaceLitter" associated with the layer value "SoilSurface"
    # We'll also chuck any records with a non-hit code. because we want to avoid
    # accidentally considering a value like NA when that was somehow below a
    # litter code in the canopy records.
    dplyr::filter(.data = _,
                  layer != "SoilSurface",
                  !(code %in% c("", NA, "None", "N"))) |>
    # Summarizing by each pin drop, create LowestCanopy which is TRUE where a
    # litter code was in the lowest canopy record and FALSE where any other kind
    # of value was in the lowest record.
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(x = c("PrimaryKey",
                                                    "LineKey",
                                                    "PointNbr")),
                     LowestCanopy_litter = dplyr::last(code) %in% recognized_litter_codes) |>
    # Add the layer variable with the value "SoilSurface" to all records for
    # joining purposes
    dplyr::mutate(.data = _,
                  layer = "SoilSurface") |>
    dplyr::left_join(x = lpi_species,
                     y = _,
                     by = c("PrimaryKey",
                            "LineKey",
                            "PointNbr",
                            "layer"),
                     relationship = "one-to-one") |>
    # Replace the code value with "SoilSurface" for qualifying records, but
    # otherwise keep the current value.
    # Qualifying records are those where all of the following are true:
    #   LowestCanopy_litter is TRUE
    #   The string in the code variable is less than three characters long
    #   The string in the code variable is not "M" or "LC" which can happen
    #     where one of those was the original SoilSurface record and which
    #     preclude a valid "SurfaceLitter" designation.
    # Additional qualifying records are those where the following is true:
    #   The string in the code variable is "D" for duff.
    dplyr::mutate(code = dplyr::case_when(LowestCanopy_litter &
                                            nchar(code) < 3 &
                                            !code %in% c("M", "LC") ~ "SurfaceLitter",
                                          # "D" for duff is a special case
                                          # because it can count toward
                                          # SurfaceLitter but unlike other
                                          # litter codes can be associated with
                                          # a layer value of "SoilSurface"
                                          code == "D" ~ "SurfaceLitter",
                                          .default = code)) |>
    # remove Lowest canopy field
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = c("LowestCanopy"))) |>
    # Condense Rock codes
    dplyr::mutate(.data = _,
                  code = dplyr::case_when(code %in% c("RF",
                                                      "R",
                                                      "GR",
                                                      "CB",
                                                      "ST",
                                                      "BY",
                                                      "BR") ~ "Rock",
                                          code %in% c("S", "CY", "PC", "LM", "FG") ~ "Soil",
                                          .default = code))

  # Total Basal Cover
  basal_cover <- pct_cover(lpi_tall = lpi_species,
                           hit = "basal",
                           by_line = FALSE,
                           tall = TRUE,
                           code)

  # Litter, Rock, Soil Cover
  litter_rock_soil <- basal_cover |>
    dplyr::filter(.data = _,
                  toupper(indicator) %in% c("ROCK", "SOIL", "SURFACELITTER")) |>
    dplyr::mutate(.data = _,
                  indicator = stringr::str_to_camel(string = indicator,
                                                    first_upper = TRUE) |>
                    stringr::str_c("AH_", .x = _, "Cover")) |>
    tidyr::pivot_wider(data = _,
                       names_from = indicator,
                       values_from = percent) |>
    # add total ground cover
    dplyr::mutate(.data = _,
                  AH_TotalGroundCover = 100 - AH_SoilCover) |>
    # rename Surface Litter
    dplyr::rename(.data = _,
                  tidyselect::any_of(x = c("AH_SurfaceLitterCover" = "AH_SurfacelitterCover",
                                           "AH_BareSoilCover" = "AH_SoilCover")))

  # This is for all the vascular basal cover, so excluding anything that was
  # surface litter, rock, soil, or generic moss or lichen codes.
  basal_cover_sum <- basal_cover |>
    dplyr::filter(.data = _,
                  !toupper(indicator) %in% c("ROCK", "SOIL", "SURFACELITTER", "2MOSS", "2LICHN", "M", "LC")) |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(x = c("PrimaryKey")),
                     BasalCover = sum(percent,
                                      na.rm = TRUE))

  # Slope Shape
  slope_shape <- dplyr::mutate(.data = slope_shape,
                               SlopeShape = SlopeShape |>
                                 stringr::str_to_camel(string = _,
                                                       first_upper = TRUE) |>
                                 dplyr::replace_values(x = _,
                                                       from = "Uniform",
                                                       to = "Linear"))
  # join all indicators together
  # rhem_indicators <- dplyr::left_join(ah_cover_rhem, fh_cover_rhem, by = "PrimaryKey") %>%
  #   dplyr::left_join(litter_rock_soil) %>%
  #   dplyr::left_join(basal_cover_sum) %>%
  #   dplyr::left_join(total_foliar) %>%
  #   dplyr::left_join(slope_shape)

  # This is a left join, but that makes me nervous because any PrimaryKeys
  # somehow missing from ah_cover_rhem will be excluded from the output even if
  # they appear in other data frames. I added header reduced to PrimaryKey as a
  # safety.
  purrr::reduce(.x = list(dplyr::select(.data = header,
                                        tidyselect::all_of(x = c("PrimaryKey"))) |>
                            dplyr::distinct(),
                          ah_cover_rhem,
                          fh_cover_rhem,
                          litter_rock_soil,
                          basal_cover_sum,
                          total_foliar,
                          slope_shape),
                .f = dplyr::left_join,
                by = "PrimaryKey",
                relationship = "one-to-one")
}
