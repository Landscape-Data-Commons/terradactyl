#' Core Cover Indicators
#' Generates cover calculations from the Line-point intercept method that are likely generalizable across programs (e.g., TotalFoliarCover, TotalLitterCover, BareSoilCover, cover by GrowthHabit, etc)
#' @param lpi_species Dataframe in environment. An LPI tall table generated from \code{gather_lpi()} and \code{species_join()}.
#' @examples
#' library(terradactyl)
#' cover <- core_cover_indicators(lpi_species)

#' @export core_cover_indicators
#' @rdname core_cover_indicators
core_cover_indicators <- function(lpi_species) {

  # Correct the Non-Woody to NonWoody
  lpi_species$GrowthHabit[grepl(
    pattern = "Non-woody|Nonwoody|Non-Woody",
    x = lpi_species$GrowthHabit
  )] <- "NonWoody"

  lpi_species$GrowthHabit[lpi_species$GrowthHabitSub %in%
                            c("Forb/herb", "Forb", "Graminoid", "Grass", "Forb/Herb")] <- "ForbGraminoid"

  # If non-vascular in GrowthHabitSub, indicate that in GrowthHabit
  lpi_species$GrowthHabit[grepl(
    pattern = "NonVascular|Nonvascular|Non-vascular",
    x = lpi_species$GrowthHabitSub
  )] <- "Nonvascular"

  # If non-vascular in GrowthHabitSub, indicate that in GrowthHabit
  lpi_species$GrowthHabitSub[grepl(
    pattern = "NonVascular|Nonvascular|Non-vascular",
    x = lpi_species$GrowthHabitSub
  )] <- NA

  # Correct the Sub-shrub to SubShrub
  lpi_species$GrowthHabitSub[grepl(
    pattern = "Sub-Shrub|subshrub|Sub-shrub|Subshrub|sub-shrub",
    x = lpi_species$GrowthHabitSub
  )] <- "SubShrub"

  lpi_species$GrowthHabitSub[grepl(
    pattern = "Forb|Forb/Herb",
    x = lpi_species$GrowthHabitSub
  )] <- "Forb"

  # Add a Shrub/SubShrub/Succulent field
  lpi_species$ShrubSucculent[grepl(
    pattern = "SubShrub|Shrub|Succulent",
    x = lpi_species$GrowthHabitSub
  )] <- "ShrubSucculent"

  # Correct to Graminoid
  lpi_species$GrowthHabitSub[grepl(pattern = "Grass|Graminoid|Sedge",
                                   x = lpi_species$GrowthHabitSub)] <- "Graminoid"

  # Calculate Total Foliar Cover ----
  total_foliar <- pct_cover_total_foliar(
    lpi_tall = lpi_species,
    tall = TRUE,
    by_line = FALSE
  )

  # Calculate between plant cover (includes bare soil) ----
  between.plant.cover <- pct_cover_between_plant(
    lpi_tall = lpi_species,
    by_line = FALSE,
    tall = TRUE
  )

  # Clean up indicator names so they are compatible with the AIM.gdb schema

  # Assign string replacements
  between.plant.replace <- c(
    "\\bL\\b" = "HerbLitter",
    "HL" = "HerbLitter",
    "AM" = "HerbLitter",
    "DN" = "HerbLitter",
    "ER" = "HerbLitter",
    "TH" = "HerbLitter",
    "HT" = "NonVegLitter",
    "NL" = "NonVegLitter",
    "AL" = "NonVegLitter",
    "OM" = "NonVegLitter",
    "DS" = "DepSoil",
    "\\bD\\b" = "Duff",
    "LC" = "Lichen",
    "\\bM\\b" = "Moss",
    "WL" = "WoodyLitter",
    "CY" = "Cyanobacteria",
    "EL" = "EmbLitter",
    "\\bW\\b" = "Water",
    "WA" = "Water",
    "RF" = "Rock",
    "\\bR\\b" = "Rock",
    "GR" = "Rock",
    "CB" = "Rock",
    "ST" = "Rock",
    "BY" = "Rock",
    "VL" = "VagrLichen",
    "AG" = "BareSoil",
    "CM" = "BareSoil",
    "LM" = "BareSoil",
    "FG" = "BareSoil",
    "PC" = "BareSoil",
    "BR" = "Rock",
    "\\bS\\b" = "BareSoil",
    "[[:punct:]]" = ""
  )

  # Perform replacements
  between.plant.cover <- between.plant.cover %>%
    # Substitute the field names for those that are human readable
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., between.plant.replace)) %>%

    # Add FH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = "")) %>%

    # Remove "FH_" from the BareSoilCover indicator
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace(., "FH_BareSoilCover", "BareSoilCover"))

  # Because the renaming processing lumps categories,
  # we need to get a summed value (e.g., Soil =S+FG+LM_CM+AG)
  between.plant.cover <- between.plant.cover %>%
    dplyr::group_by(PrimaryKey, indicator) %>%
    dplyr::summarise(percent = sum(percent))

  # Add a Total Litter Indicator
  between.plant.cover <- between.plant.cover %>%
    # Filter Litter Indicators
    dplyr::filter(grepl(pattern = "Litter", x = indicator)) %>%
    # Sum all indicator hits
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(
      indicator = "FH_TotalLitterCover",
      percent = sum(percent)
    ) %>%
    # Add back to the rest of the between plant cover indicators
    dplyr::bind_rows(between.plant.cover, .)

  # Any hit litter ####
  lpi_species_litter <- lpi_species %>%
    dplyr::mutate(
      Litter = dplyr::case_when(
        code %in% c("HL", "L", "DN", "ER", "AM", "TH") ~ "HerbLitter",
        code %in% "WL" ~ "WoodyLitter"
      ),
      TotalLitter = dplyr::case_when(
        code %in% c(
          "HL",
          "L",
          "DN",
          "ER",
          "AM",
          "WL",
          "NL",
          "EL",
          "HT",
          "AL",
          "OM",
          "TH"
        ) ~ "TotalLitter"
      )
    )

  litter <- pct_cover(lpi_species_litter,
                      tall = TRUE,
                      by_line = FALSE,
                      hit = "any",
                      Litter
  ) %>%
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "HERBLITTER" ~ "HerbLitter",
      indicator == "WOODYLITTER" ~ "WoodyLitter"
    ))

  total_litter <- pct_cover(lpi_species_litter,
                            tall = TRUE,
                            hit = "any",
                            by_line = FALSE,
                            TotalLitter
  ) %>%
    dplyr::mutate(indicator = indicator %>% dplyr::recode("TOTALLITTER" = "TotalLitter"))

  litter <- dplyr::bind_rows(litter, total_litter) %>%
    dplyr::mutate(indicator = paste("AH_", indicator, "Cover", sep = ""))


  # Species Group Cover ----
  # Set the replacement values for valid indicator names ----
  spp.cover.replace <- c(
    "NON" = "Non",
    "^NO\\." = "NonNox",
    "NO$" = "NonNox",
    "^YES" = "Nox",
    "ANNUAL" = "Ann",
    "PERENNIAL" = "Peren",
    "[[:punct:]]" = "",
    "GRAMINOID" = "Graminoid",
    "FORB" = "Forb",
    "NON" = "No",
    "SUBSHRUB" = "SubShrub",
    "SHRUB" = "Shrub",
    "SUCCULENT" = "Succulent",
    "TREE" = "Tree",
    " " = "",
    "STATURE" = "",
    "SAGEBRUSH" = "Sagebrush",
    "GRASS" = "Graminoid",
    "SHORT" = "Short",
    "TALL" = "Tall",
    "0" = "Live",
    "1" = "Dead",
    "PREFERRED" = "Preferred",
    "WOODY" = "Woody",
    "VINE" = "Vine",
    "DECIDUOUS" = "Deciduous",
    "EVERGREEN" = "Evergreen",
    "LICHENOUS" = "Lichenous",
    "VASCULAR" = "Vascular",
    "SEDGE" = "Graminoid"
  )


  # Any hit cover ----
  ah_spp_group_cover <- dplyr::bind_rows(

    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              GrowthHabitSub
    ),

    # Cover Duration and GrowthHabit
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              Duration, GrowthHabit
    ),

    # Cover by GrowthHabit
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_line = FALSE,
              GrowthHabit
    ))



  # Fix to indicator names so they are valid
  ah_spp_group_cover <- ah_spp_group_cover %>%
    # Substitute "NonNox" for "NO
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("AH_", indicator, "Cover", sep = ""))


  # First hit cover ----
  fh_spp_group_cover <- rbind(
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_line = FALSE,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_line = FALSE,
              GrowthHabitSub
    ))

  fh_spp_group_cover <- fh_spp_group_cover %>%
    # Substitute for Field friendly names
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add FH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = ""))




  # Combine  all LPI based cover indicators----
  lpi_cover <- dplyr::bind_rows(
    ah_spp_group_cover,
    fh_spp_group_cover,
    total_foliar,
    between.plant.cover,
    litter
  ) %>%

    dplyr::distinct() %>%

    # Remove NULL columns
    subset(!stringr::str_detect(indicator, "NULL")) |>

    # Spread to a wide format
    tidyr::pivot_wider(names_from = indicator,
                        values_from = percent,
                        values_fill=0)

  # Clean up fields
  if("AH_NACover" %in% names(lpi_cover)){
    lpi_cover <- lpi_cover %>% dplyr::select(-AH_NACover)
  }

  if("AH_NACover" %in% names(lpi_cover)){
    lpi_cover <- lpi_cover %>% dplyr::select(-AH_NACover)
  }
  return(lpi_cover) # in this case we need the return otherwise the if state returns a null value if FALSE
}

