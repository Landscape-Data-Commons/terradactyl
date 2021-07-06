#' Gather TerrADat and LMF Soil data into a summary data frame
#'
#' @description Given soil horizon and pit data, create a tall format data frame
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param source Character string. The data source format, can be AIM, TerrADat, NRI, or LMF. (Case independent)

#' @param tblSoilPitHorizons Dataframe of the data structure tblSoilPitHorizons from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' @param tblSoilPits Dataframe of the data structure tblSoilPits from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from LMF/NRI data with the addition of PrimaryKey and DBKey fields.

#' @importFrom magrittr %>%
#' @name gather_soil_summary
#' @family <gather>
#' @return A data frame summarizing horizon data to the soil pit

#' @export gather_soil_summary_lmf
#' @export gather_soil_summary_aim
#' @export gather_soil_summary

## To do:
# last checks on data output format
# do we need more data input support for NRI?
# are the '@ sections above correct?


gather_soil_summary_lmf <- function(dsn = NULL, source = "LMF", SOILHORIZON = NULL){
  ### input ####
  if (!is.null(SOILHORIZON)){
    hz_lmf_raw <- SOILHORIZON
  } else if(!is.null(dsn)){
    hz_lmf_raw <- switch(source, LMF = {
      suppressWarnings(sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T))
    }, NRI = {
      readRDS(dsn)
    })
  } else{
    stop("One or more necessary inputs missing")
  }
  
  
  ### prepare hz data ####
  hz_lmf <- hz_lmf_raw  %>%
    dplyr::select(PrimaryKey, DBKey, HorizonKey = SEQNUM,
                  HorizonDepthLower = DEPTH, Effer = EFFERVESCENCE_CLASS,
                  Texture = HORIZON_TEXTURE, TextureModifier = TEXTURE_MODIFIER,
                  PitNotes = UNUSUAL_FEATURES
    )%>%
    # arrange by hz depth in order to id upper depth
    arrange(PrimaryKey, HorizonDepthLower)  %>%
    dplyr::mutate(
      HorizonDepthLower = 2.54 * suppressWarnings(as.numeric(HorizonDepthLower)), # is lmf data ALWAYS in inches?
      DepthUOM = "cm",
      HorizonKey = as.character(HorizonKey),
      # when sorted by key and depth, upper depth is the lower depth of the previous row
      HorizonDepthUpper =
        sapply(unique(PrimaryKey), function(x) {
          ### POTENTIAL BUG FLAG References a data table by name. Might break?
          lower <- hz_lmf_raw$DEPTH[hz_lmf_raw$PrimaryKey == x] # DEPTH = lower depth, var not yet renamed in raw file
          upper <- c(0, lower[1:length(lower) - 1])
          return(upper)}) %>% unlist(),
      # get data-at-depths. Mirrors code above, cutting s-s-c and fragment lines (not in lmf/nri)
      Texture0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Texture, NA_character_),
      Texture15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Texture, NA_character_),
      Texture30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Texture, NA_character_),
      Texture60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Texture, NA_character_),
      TextureModifier0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, TextureModifier, NA_character_),
      TextureModifier15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, TextureModifier, NA_character_),
      Effer0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Effer, NA_character_),
      Effer15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Effer, NA_character_),
      Effer30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Effer, NA_character_),
      Effer60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Effer, NA_character_),
    )
  
  # get min-max effervescence data, mirrors code above
  # tacked on min hz depth, because we're already summarizing by primary key here
  hzeff_lmf <- hz_lmf %>%
    dplyr::select(
      PrimaryKey, Effer, HorizonDepthLower
    ) %>%
    # make ordinal effer field, to get min and max
    dplyr::mutate(
      EfferOrdinal = case_when(
        Effer == "NE" ~ 1,
        Effer == "VS" ~ 2,
        Effer == "SL" ~ 3,
        Effer == "ST" ~ 4,
        Effer == "VE" ~ 5),
    ) %>%
    group_by(PrimaryKey) %>%
    summarize(MinEffer = min(EfferOrdinal),
              MaxEffer = max(EfferOrdinal),
              TotalPitDepth = max(HorizonDepthLower),
              .groups = "drop") %>%
    # send the ordinal effervescence back to the character/factor data
    mutate(
      MinEffer = case_when(
        MinEffer == 1 ~ "NE",
        MinEffer == 2 ~ "VS",
        MinEffer == 3 ~ "SL",
        MinEffer == 4 ~ "ST",
        MinEffer == 5 ~ "VE"),
      MaxEffer = case_when(
        MaxEffer == 1 ~ "NE",
        MaxEffer == 2 ~ "VS",
        MaxEffer == 3 ~ "SL",
        MaxEffer == 4 ~ "ST",
        MaxEffer == 5 ~ "VE"),
    )
  
  soil_lmf <- hz_lmf %>% # edited from aim version above
    dplyr::group_by(
      PrimaryKey, DBKey,
    ) %>%
    # arrange by depth and take top horizons
    # data at depths is preserved because the summarize command ignores NAs
    arrange(DBKey, PrimaryKey, HorizonDepthUpper) %>%
    dplyr::summarize(across(everything(), ~ first(na.omit(.x))),
                     .groups = "drop") %>%
    # only need at-depth vars calculated above, the effer data, and the keys
    dplyr::select(
      -HorizonDepthLower, -Effer, -DepthUOM, -Texture, -HorizonDepthUpper,
      -Texture, -TextureModifier, -HorizonKey) %>%
    left_join(hzeff_lmf, by = "PrimaryKey")
  
  if("sf" %in% class(soil_lmf)) soil_lmf <- soil_lmf %>% sf::st_drop_geometry()
  
  return(soil_lmf)
  
}

gather_soil_summary_aim <- function(dsn = NULL, source = "AIM", tblSoilPitHorizons = NULL, tblSoilPits = NULL){
  
  if (!is.null(tblSoilPits) & !is.null(tblSoilPitHorizons)) {
    hz_aim_raw <- tblSoilPitHorizons
    pit_aim_raw <- tblSoilPits
  } else if(!is.null(dsn)){
    hz_aim_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
                                               stringsAsFactors = FALSE, quiet = T))
    pit_aim_raw <- suppressWarnings(
      sf::st_read(dsn = dsn, layer = "tblSoilPits", stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("One or more necessary inputs missing")
  }
  
  ### prepare hz data, incl effervescence ####
  hz_aim <- hz_aim_raw %>%
    dplyr::select(
      PrimaryKey, DBKey, SoilKey, HorizonKey,
      HorizonDepthUpper, HorizonDepthLower, DepthUOM = DepthMeasure,
      Texture, TextureModifier = ESD_HorizonModifier,
      PercentClay = ESD_PctClay, PercentSand = ESD_PctSand, Effer, RockFragments,
      Fragment1VolPct = ESD_FragVolPct, Fragment1Type = ESD_FragmentType,
      Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
      Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3
    ) %>% dplyr::mutate(
      # unify depth units
      HorizonDepthLower = case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower) * 2.54),
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
      HorizonDepthUpper = case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper) * 2.54),
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
      DepthUOM = "cm",
      # prep pct clay sand silt. Add an estimate function?
      PercentClay = suppressWarnings(as.numeric(PercentClay)),
      PercentSand = suppressWarnings(as.numeric(PercentSand)),
      PercentSilt = 100 - PercentClay - PercentSand,
      # parse fragment class columns.
      TopHorizonGravelPct = suppressWarnings(as.numeric(case_when(
        Fragment1Type == "1" ~ Fragment1VolPct,
        Fragment2Type == "1" ~ Fragment2VolPct,
        Fragment3Type == "1" ~ Fragment3VolPct))),
      TopHorizonCobblePct = suppressWarnings(as.numeric(case_when(
        Fragment1Type == "2" ~ Fragment1VolPct,
        Fragment2Type == "2" ~ Fragment2VolPct,
        Fragment3Type == "2" ~ Fragment3VolPct))),
      TopHorizonStonePct = suppressWarnings(as.numeric(case_when(
        Fragment1Type == "6" ~ Fragment1VolPct,
        Fragment2Type == "6" ~ Fragment2VolPct,
        Fragment3Type == "6" ~ Fragment3VolPct))),
      TopHorizonNodulePct = suppressWarnings(as.numeric(case_when(
        Fragment1Type == "8" ~ Fragment1VolPct,
        Fragment2Type == "8" ~ Fragment2VolPct,
        Fragment3Type == "8" ~ Fragment3VolPct))),
      TopHorizonDurinodePct = suppressWarnings(as.numeric(case_when(
        Fragment1Type == "9" ~ Fragment1VolPct,
        Fragment2Type == "9" ~ Fragment2VolPct,
        Fragment3Type == "9" ~ Fragment3VolPct))),
    ) %>%
    dplyr::mutate(
      TopHorizonLargeFragmentPct = TopHorizonCobblePct + TopHorizonStonePct,
      # get soil at depth data, adapted from allie's code
      Clay0  = ifelse(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentClay, NA_integer_),
      Clay15 = ifelse(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentClay, NA_integer_),
      Clay30 = ifelse(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentClay, NA_integer_),
      Clay60 = ifelse(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentClay, NA_integer_),
      Sand0  = ifelse(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentSand, NA_integer_),
      Sand15 = ifelse(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentSand, NA_integer_),
      Sand30 = ifelse(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentSand, NA_integer_),
      Sand60 = ifelse(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentSand, NA_integer_),
      Silt0  = ifelse(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentSilt, NA_integer_),
      Silt15 = ifelse(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentSilt, NA_integer_),
      Silt30 = ifelse(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentSilt, NA_integer_),
      Silt60 = ifelse(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentSilt, NA_integer_),
      Texture0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Texture, NA_character_),
      Texture15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Texture, NA_character_),
      Texture30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Texture, NA_character_),
      Texture60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Texture, NA_character_),
      TextureModifier0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, TextureModifier, NA_character_),
      TextureModifier15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, TextureModifier, NA_character_),
      Effer0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Effer, NA_character_),
      Effer15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Effer, NA_character_),
      Effer30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Effer, NA_character_),
      Effer60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Effer, NA_character_),
      RockFragments0  = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, RockFragments, NA_real_),
      RockFragments15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, RockFragments, NA_real_),
      RockFragments30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, RockFragments, NA_real_),
      RockFragments60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, RockFragments, NA_real_),
    )
  
  # get min and max effer data, then rejoin to hz table
  hzeff_aim <- hz_aim %>%
    dplyr::select(
      SoilKey, Effer
    ) %>%
    # make ordinal effer field, to get min and max
    dplyr::mutate(
      EfferOrdinal = case_when(
        Effer == "NE" ~ 1,
        Effer == "VS" ~ 2,
        Effer == "SL" ~ 3,
        Effer == "ST" ~ 4,
        Effer == "VE" ~ 5),
    ) %>%
    group_by(SoilKey) %>%
    summarize(MinEffer = min(EfferOrdinal),
              MaxEffer = max(EfferOrdinal),
              .groups = "drop") %>%
    mutate( # send the ordinal effervescence back to the character/factor data
      MinEffer = case_when(
        MinEffer == 1 ~ "NE",
        MinEffer == 2 ~ "VS",
        MinEffer == 3 ~ "SL",
        MinEffer == 4 ~ "ST",
        MinEffer == 5 ~ "VE"),
      MaxEffer = case_when(
        MaxEffer == 1 ~ "NE",
        MaxEffer == 2 ~ "VS",
        MaxEffer == 3 ~ "SL",
        MaxEffer == 4 ~ "ST",
        MaxEffer == 5 ~ "VE"),
    )
  
  hzjoin_aim <- hz_aim %>%
    dplyr::group_by(
      PrimaryKey, DBKey, SoilKey,
    ) %>%
    # arrange by depth and take top horizons
    # data at depth is preserved because the summarize command ignores NAs
    arrange(DBKey, PrimaryKey, SoilKey, HorizonDepthUpper) %>%
    dplyr::summarize(across(everything(), ~ first(na.omit(.x))),
                     .groups = "drop") %>%
    # only need at-depth vars calculated above, the effer data, and the keys
    dplyr::select(
      -HorizonDepthUpper, -HorizonDepthLower, -DepthUOM,
      -Texture, -TextureModifier, -PercentClay, -PercentSand, -PercentSilt,
      -Effer, -RockFragments,
      -Fragment1VolPct, -Fragment1Type, -Fragment2VolPct, -Fragment2Type,
      -Fragment3VolPct, -Fragment3Type, -HorizonKey,
      -TopHorizonCobblePct, -TopHorizonStonePct,
    ) %>%
    left_join(hzeff_aim, by = "SoilKey")
  
  ### prepare pit and plot level data ####
  # pit: mostly for metadata, also total pit depth and total rock frags
  pit_aim <- pit_aim_raw %>%
    dplyr::select(
      PrimaryKey, DBKey, SoilKey,
      TotalFragments = RockFragments, TotalPitDepth = SoilDepthLower, PitDepthMeasure = DepthMeasure,
      PitNotes = Notes) %>%
    # unify depth measures and drop depth measure var
    mutate(
      TotalPitDepth = case_when(PitDepthMeasure == "in" ~ TotalPitDepth * 2.54,
                           PitDepthMeasure == "cm" | is.na(PitDepthMeasure) ~ TotalPitDepth),
    ) %>%
    select(-PitDepthMeasure)
  
  ### join all aim-style data, final touches ####
  soil_aim <-
    left_join(hzjoin_aim, pit_aim, by = c("PrimaryKey", "DBKey", "SoilKey")) %>%
    # final drop line
    dplyr::select(-SoilKey)
  
  if("sf" %in% class(soil_aim)) soil_aim <- soil_aim %>% sf::st_drop_geometry()
  
  return(soil_aim)
}

gather_soil_summary <- function(dsn = NULL, source,
                                tblSoilPitHorizons = NULL, tblSoilPits = NULL,
                                SOILHORIZON = NULL){
  
  source <- toupper(source)
  
  if(source %in% c("AIM", "TERRADAT")) {
    if(is.null(dsn) & (is.null(tblSoilPits) | is.null(tblSoilPitHorizons))){
      stop("If source is AIM or TerrADat, you must provide either a geodatabase or tblSoilPits and tblSoilPitHorizons")}
    soil <- gather_soil_summary_aim(dsn = dsn, source = source, tblSoilPits = tblSoilPits,
                                    tblSoilPitHorizons = tblSoilPitHorizons)}
  
  if(source %in% c("LMF", "NRI")){
    if(is.null(dsn) & is.null(SOILHORIZON)){
      stop("If source is LMF or NRI, you must provide either a geodatabase or table SOILHORIZON")}
    soil <- gather_soil_summary_lmf(dsn = dsn, source = source, SOILHORIZON = SOILHORIZON)
  }
  
  soil$Source <- source
  
  return(soil)
}
