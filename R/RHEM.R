#' RHEM Input Calculations
#' @param lpi_species Dataframe of lpi tall table joined with RHEM species attributes in the field "RHEM_Habit"
#' @param slope_shape Dataframe of slope tall table with field SlopeShape = Vertical Slope Shape
#'
#' @export RHEM
#' @rdname RHEM
RHEM <- function(
                 lpi_species,
                 header,
                 slope_shape) {
  # RHEM functional group cover ####
  ah_cover_rhem <- pct_cover(
    lpi_tall = lpi_species,
    hit = "any",
    by_year = FALSE,
    by_line = FALSE,
    tall = TRUE,
    RHEM_Habit
  )

  ah_cover_rhem_clean <- ah_cover_rhem %>%
    dplyr::mutate(indicator = indicator %>% snakecase::to_upper_camel_case() %>%
      stringr::str_c("AH_", ., "Cover")) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = "percent")

  fh_cover_rhem <- pct_cover(
    lpi_tall = lpi_species,
    hit = "first",
    by_year = FALSE,
    by_line = FALSE,
    tall = TRUE,
    RHEM_Habit
  )

  fh_cover_rhem_clean <- fh_cover_rhem %>%
    dplyr::mutate(indicator = indicator %>% snakecase::to_upper_camel_case() %>%
      stringr::str_c("FH_", ., "Cover")) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = "percent")



  # Identify Litter above soil
  lpi_species <- lpi_species %>%
    dplyr::mutate(
      layer = factor(layer,
        levels = c(
          "TopCanopy",
          "Lower1",
          "Lower2",
          "Lower3",
          "Lower4",
          "Lower5",
          "Lower6",
          "Lower7",
          "SoilSurface"
        )
      )
    ) %>%
    dplyr::arrange(layer)

  lpi_species <- lpi_species %>%
    # remove soil surface
    dplyr::filter(layer != "SoilSurface") %>%

    # Strip out all the non-hit codes
    dplyr::filter(!(code %in% c("", NA, "None", "N"))) %>%
    dplyr::group_by(PrimaryKey, LineKey, PointNbr) %>%

    # Get the first hit at a point
    dplyr::summarize(LowestCanopy = dplyr::last(code)) %>%

    # Identify Litter
    dplyr::mutate(
      LowestCanopy = dplyr::case_when(
        LowestCanopy %in% c("L", "HL", "WL", "EL") ~ "SurfaceLitter"
      ),
      layer = "SoilSurface"
    ) %>%
    dplyr::filter(!is.na(LowestCanopy)) %>%
    dplyr::left_join(lpi_species, .) %>%

    # Identify instance of Litter of surface code
    dplyr::mutate(code = dplyr::case_when(LowestCanopy == "SurfaceLitter" & nchar(code) < 3 ~ "SurfaceLitter", TRUE ~ code)) %>%

    # remove Lowest canopy field
    dplyr::select(-LowestCanopy) %>%

    # Condense Rock codes
    dplyr::mutate(
      code =
        dplyr::case_when(
          code %in% c(
            "RF",
            "R",
            "GR",
            "CB",
            "ST",
            "BY",
            "BR"
          ) ~ "Rock",
          code %in% "S" ~ "Soil",
          TRUE ~ code
        )
    )

  # Total Basal Cover
  basal_cover <- pct_cover(
    lpi_tall = lpi_species,
    hit = "basal",
    by_year = FALSE,
    by_line = FALSE,
    tall = TRUE, code
  )

  litter_rock_soil <- basal_cover %>%
    dplyr::filter(indicator %in% c("ROCK", "SOIL", "SURFACELITTER")) %>%
    dplyr::mutate(indicator = indicator %>% snakecase::to_upper_camel_case() %>%
      stringr::str_c("AH_", ., "Cover")) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = percent) %>%
    # add total ground cover
    dplyr::mutate(AH_TotalGroundCover = 100 - AH_SoilCover) %>%
    # rename Surface Litter
    dplyr::rename("AH_SurfaceLitterCover" = "AH_SurfacelitterCover")

  basal_cover_sum <- basal_cover %>%
    dplyr::filter(!indicator %in% c("ROCK", "SOIL", "SURFACELITTER", "2MOSS", "2LICHN")) %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarise(BasalCover = sum(percent))


  # Total Foliar Cover
  total_foliar <- pct_cover_total_foliar(
    lpi_tall = lpi_species,
    tall = TRUE
  )

  total_foliar <- total_foliar %>% dplyr::select(PrimaryKey, FH_TotalFoliarCover = percent)


  # Slope Shape
  slope_shape <- slope_shape %>% dplyr::mutate(SlopeShape = SlopeShape %>%
    snakecase::to_upper_camel_case() %>%
    dplyr::recode(
      "Uniform" = "Linear"
    ))
  # join all indicators together
  rhem_indicators <- dplyr::left_join(ah_cover_rhem_clean, fh_cover_rhem_clean, by = "PrimaryKey") %>%
    dplyr::left_join(litter_rock_soil) %>%
    dplyr::left_join(basal_cover_sum) %>%
    dplyr::left_join(total_foliar) %>%
    dplyr::left_join(slope_shape)
}
