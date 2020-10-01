#' RHEM Input Calculations
#' @param lpi_species Dataframe of lpi tall table joined with RHEM species attributes in the field "RHEM_Habit"
#' @param slope_shape Dataframe of slope tall table with field SlopeShape = Vertical Slope Shape
#'
#' @export RHEM
#' @rdname RHEM
RHEM <- function(
  lpi_species,
  header,
  slope_shape
) {
  # RHEM functional group cover ####
  ah_cover_rhem <- pct_cover(lpi_tall = lpi_species,
                                          hit = "any",
                                          by_year = FALSE,
                                          by_line = FALSE,
                                          tall = TRUE,
                                          RHEM_Habit)

  ah_cover_rhem_clean <- ah_cover_rhem %>% dplyr::mutate(indicator =  indicator %>% snakecase::to_upper_camel_case() %>%
                                                           stringr::str_c("AH_", ., "Cover")) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = "percent")

  fh_cover_rhem <- pct_cover(lpi_tall = lpi_species,
                                          hit = "first",
                                          by_year = FALSE,
                                          by_line = FALSE,
                                          tall = TRUE,
                                          RHEM_Habit)

  fh_cover_rhem_clean <- fh_cover_rhem %>% dplyr::mutate(indicator =  indicator %>% snakecase::to_upper_camel_case() %>%
                                                           stringr::str_c("FH_", ., "Cover")) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = "percent")


  # # add litter and Biological Crucs category ####
  # lpi_crust <- lpi_species %>% dplyr::mutate(Crust =
  #                                      dplyr::case_when(code %in% c("M",  "LC")~ "BioCrust"))
  #
  # crust_cover <- pct_cover(lpi_tall = lpi_crust,
  #                                       hit = "any",
  #                                       by_year = FALSE,
  #                                       by_line = FALSE,
  #                                       tall = TRUE,
  #                                       Crust)
  #
  # crusts_clean <- crust_cover %>% dplyr::mutate(indicator =  indicator %>% snakecase::to_upper_camel_case() %>%
  #                                                 stringr::str_c("AH_", ., "Cover")) %>%
  #   tidyr::pivot_wider(names_from = "indicator", values_from = "percent")

  # Total Basal Cover
  basal_cover <- pct_cover(lpi_tall = lpi_species,
                                        hit = "basal",
                                        by_year = FALSE,
                                        by_line = FALSE,
                                        tall = TRUE, code
  )
  basal_cover_sum <- basal_cover %>% dplyr::filter(nchar(indicator) >=3) %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarise(BasalCover = sum(percent))


  # Total Foliar Cover
  total_foliar <- pct_cover_total_foliar(lpi_tall = lpi_species,
                                                      tall=TRUE)

  total_foliar <- total_foliar %>% dplyr::select(PrimaryKey, FH_TotalFoliarCover = percent)

  # Rock cover ####
  lpi_rock <- lpi_species %>% dplyr::mutate(Rock =
                                      dplyr::case_when(code %in% c("RF",
                                                                   "R",
                                                                   "GR",
                                                                   "CB",
                                                                   "ST",
                                                                   "BY",
                                                                   "BR") ~ "Rock"))
  # rock cover ####
  rock_cover <- pct_cover(lpi_tall = lpi_rock,
                                       hit = "any",
                                       by_year = FALSE,
                                       by_line = FALSE,
                                       tall = TRUE,
                                       Rock)

  rock_cover <- rock_cover %>% dplyr::mutate(indicator =  indicator %>% snakecase::to_upper_camel_case() %>%
                                               stringr::str_c("AH_", ., "Cover")) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = "percent")

  # Soil cover
  soil_cover <- pct_cover(lpi_tall = lpi_species, tall = TRUE, hit = "any", by_year = FALSE, by_line = FALSE, code)

  soil_cover <- soil_cover %>% dplyr::filter (indicator == "S") %>%
    dplyr::select(PrimaryKey, AH_SoilCover = percent)

  total_groundcover <- soil_cover %>% dplyr::mutate(AH_TotalGroundCover = 100 - AH_SoilCover) %>%
    dplyr::select(-AH_SoilCover)

  # Litter above soil
  lpi_surface_litter <- lpi_species %>% dplyr::mutate(
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
  ) %>% dplyr::arrange(layer)

  # remove soil surface
  lpi_surface_litter <- lpi_surface_litter %>% dplyr::filter(layer != "SoilSurface")

  lpi_surface_litter <- lpi_surface_litter %>%

    # Strip out all the non-hit codes
    dplyr::filter(!(code %in% c("", NA, "None", "N"))) %>%
    dplyr::group_by(PrimaryKey, LineKey, PointNbr) %>%
    # Get the first hit at a point
    dplyr::summarize(LowestCanopy = dplyr::last(code)) %>%
    dplyr::left_join(lpi_surface_litter, .)

  surface_litter <- pct_cover(lpi_tall = lpi_surface_litter,
                                           hit = "any",
                                           tall = TRUE,
                                           by_year = FALSE,
                                           by_line = FALSE,
                                           LowestCanopy) %>% subset(indicator %in% c("L","HL", "WL", "EL", "AL", "NL"))

  surface_litter_sum <- surface_litter %>% dplyr::group_by(PrimaryKey) %>%
    dplyr::summarise(AH_SurfaceLitterCover = sum(percent))


  # Slope Shape
 slope_shape <- slope_shape %>%   dplyr::mutate(SlopeShape = SlopeShape %>%
                                                   snakecase::to_upper_camel_case() %>%
                                                   dplyr::recode(
                                                     "Uniform" = "Linear"
                                                   ))
  # join all indicators together
  rhem_indicators <- dplyr::left_join(ah_cover_rhem_clean,fh_cover_rhem_clean, by = "PrimaryKey") %>%

    dplyr::left_join(crusts_clean) %>%
    dplyr::left_join(surface_litter_sum) %>%
    dplyr::left_join(basal_cover_sum) %>% dplyr::left_join(rock_cover) %>%
    dplyr::left_join(soil_cover) %>%
    dplyr::left_join(total_foliar) %>%
    dplyr::left_join(total_groundcover) %>%
    dplyr::left_join(slope_shape)
}
