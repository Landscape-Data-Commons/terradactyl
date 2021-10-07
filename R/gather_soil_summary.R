#' Gather TerrADat and LMF Soil data into a summary data frame
#'
#' @description Given soil horizon and pit data, create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file 
#' extension) of the geodatabase containing the table of interest. This field 
#' is unnecessary if you supply either both of tblSoilPitHorizons and 
#' tblSoilPits (AIM/DIMA/TerrADat) or SOILHORIZON (LMF/NRI).
#' @param source Character string. The data source format, 
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblSoilPitHorizons Dataframe of the data structure tblSoilPitHorizons 
#' from the DIMA database with the addition of PrimaryKey and DBKey fields. 
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param tblSoilPits Dataframe of the data structure tblSoilPits from the DIMA 
#' database with the addition of PrimaryKey and DBKey fields. Use when data 
#' source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from LMF/NRI 
#' database with the addition of PrimaryKey and DBKey fields. Use when data 
#' source is LMF or NRI; alternately provide dsn. 
#' @importFrom magrittr %>%
#' @name gather_soil_summary
#' @family <gather>
#' @return A tall data frame summarizing horizon data to the soil pit
#' @examples
#' gather_soil_summary(dsn = "Path/To/AIM_Geodatabase.gdb", 
#'                     source = "AIM")
#' gather_soil_summary(dsn = "Path/To/LMF_Geodatabase.gdb", 
#'                     source = "LMF")
#' 
#' aim_horizons <- read.csv("Path/To/tblSoilPitHorizons.csv")
#' aim_pits <- read.csv("Path/To/tblSoilPits.csv")
#' gather_soil_summary(source = "AIM", 
#'                     tblSoilPitHorizons = aim_horizons,
#'                     tblSoilPits = aim_pits)
#' 
#' lmf_horizons <- read.csv("Path/To/SOILHORIZON.csv")
#' gather_soil_summary(source = "LMF", 
#'                     SOILHORIZON = lmf_horizons)

#' @export gather_soil_summary_lmf
#' @rdname gather_soil_summary
gather_soil_summary_lmf <- function(dsn = NULL, SOILHORIZON = NULL){
  ### input ####
  # print("a")
  if (!is.null(SOILHORIZON)){
    hz_lmf_raw <- SOILHORIZON
  } else if(!is.null(dsn)){
    hz_lmf_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T))
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
    dplyr::arrange(PrimaryKey, HorizonDepthLower)  %>%
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
      Texture0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Texture, NA_character_),
      Texture15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Texture, NA_character_),
      Texture30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Texture, NA_character_),
      Texture60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Texture, NA_character_),
      TextureModifier0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, TextureModifier, NA_character_),
      TextureModifier15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, TextureModifier, NA_character_),
      Effer0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Effer, NA_character_),
      Effer15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Effer, NA_character_),
      Effer30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Effer, NA_character_),
      Effer60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Effer, NA_character_),
    )
  
  # get min-max effervescence data, mirrors code above
  # tacked on min hz depth, because we're already summarizing by primary key here
  hzeff_lmf <- hz_lmf %>%
    dplyr::select(
      PrimaryKey, Effer, HorizonDepthLower
    ) %>%
    # make ordinal effer field, to get min and max
    dplyr::mutate(
      EfferOrdinal = dplyr::case_when(
        Effer == "NE" ~ 1,
        Effer == "VS" ~ 2,
        Effer == "SL" ~ 3,
        Effer == "ST" ~ 4,
        Effer == "VE" ~ 5),
    ) %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(MinEffer = min(EfferOrdinal),
              MaxEffer = max(EfferOrdinal),
              TotalPitDepth = max(HorizonDepthLower),
              .groups = "drop") %>%
    # send the ordinal effervescence back to the character/factor data
    dplyr::mutate(
      MinEffer = dplyr::case_when(
        MinEffer == 1 ~ "NE",
        MinEffer == 2 ~ "VS",
        MinEffer == 3 ~ "SL",
        MinEffer == 4 ~ "ST",
        MinEffer == 5 ~ "VE"),
      MaxEffer = dplyr::case_when(
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
    dplyr::arrange(DBKey, PrimaryKey, HorizonDepthUpper) %>%
    dplyr::summarize(across(everything(), ~ dplyr::first(na.omit(.x))),
                     .groups = "drop") %>%
    # only need at-depth vars calculated above, the effer data, and the keys
    dplyr::select(
      -HorizonDepthLower, -Effer, -DepthUOM, -Texture, -HorizonDepthUpper,
      -Texture, -TextureModifier, -HorizonKey) %>%
    dplyr::left_join(hzeff_lmf, by = "PrimaryKey")
  
  if("sf" %in% class(soil_lmf)) soil_lmf <- soil_lmf %>% sf::st_drop_geometry()
  
  return(soil_lmf)
  
}

#' @export gather_soil_summary_terradat
#' @rdname gather_soil_summary
gather_soil_summary_terradat <- function(dsn = NULL, tblSoilPitHorizons = NULL, tblSoilPits = NULL){
  
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
      HorizonDepthLower = dplyr::case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower) * 2.54),
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
      HorizonDepthUpper = dplyr::case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper) * 2.54),
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
      DepthUOM = "cm",
      # prep pct clay sand silt. Add an estimate function?
      PercentClay = suppressWarnings(as.numeric(PercentClay)),
      PercentSand = suppressWarnings(as.numeric(PercentSand)),
      PercentSilt = 100 - PercentClay - PercentSand,
      # parse fragment class columns.
      TopHorizonGravelPct = suppressWarnings(as.numeric(dplyr::case_when(
        Fragment1Type == "1" ~ Fragment1VolPct,
        Fragment2Type == "1" ~ Fragment2VolPct,
        Fragment3Type == "1" ~ Fragment3VolPct))),
      TopHorizonCobblePct = suppressWarnings(as.numeric(dplyr::case_when(
        Fragment1Type == "2" ~ Fragment1VolPct,
        Fragment2Type == "2" ~ Fragment2VolPct,
        Fragment3Type == "2" ~ Fragment3VolPct))),
      TopHorizonStonePct = suppressWarnings(as.numeric(dplyr::case_when(
        Fragment1Type == "6" ~ Fragment1VolPct,
        Fragment2Type == "6" ~ Fragment2VolPct,
        Fragment3Type == "6" ~ Fragment3VolPct))),
      TopHorizonNodulePct = suppressWarnings(as.numeric(dplyr::case_when(
        Fragment1Type == "8" ~ Fragment1VolPct,
        Fragment2Type == "8" ~ Fragment2VolPct,
        Fragment3Type == "8" ~ Fragment3VolPct))),
      TopHorizonDurinodePct = suppressWarnings(as.numeric(dplyr::case_when(
        Fragment1Type == "9" ~ Fragment1VolPct,
        Fragment2Type == "9" ~ Fragment2VolPct,
        Fragment3Type == "9" ~ Fragment3VolPct))),
    ) %>%
    dplyr::mutate(
      TopHorizonLargeFragmentPct = TopHorizonCobblePct + TopHorizonStonePct,
      # get soil at depth data, adapted from allie's code
      Clay0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentClay, NA_real_),
      Clay15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentClay, NA_real_),
      Clay30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentClay, NA_real_),
      Clay60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentClay, NA_real_),
      Sand0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentSand, NA_real_),
      Sand15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentSand, NA_real_),
      Sand30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentSand, NA_real_),
      Sand60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentSand, NA_real_),
      Silt0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentSilt, NA_real_),
      Silt15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentSilt, NA_real_),
      Silt30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentSilt, NA_real_),
      Silt60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentSilt, NA_real_),
      Texture0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Texture, NA_character_),
      Texture15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Texture, NA_character_),
      Texture30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Texture, NA_character_),
      Texture60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Texture, NA_character_),
      TextureModifier0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, TextureModifier, NA_character_),
      TextureModifier15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, TextureModifier, NA_character_),
      TextureModifier60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, TextureModifier, NA_character_),
      Effer0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, Effer, NA_character_),
      Effer15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, Effer, NA_character_),
      Effer30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, Effer, NA_character_),
      Effer60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, Effer, NA_character_),
      RockFragments0  = dplyr::if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, RockFragments, NA_real_),
      RockFragments15 = dplyr::if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, RockFragments, NA_real_),
      RockFragments30 = dplyr::if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, RockFragments, NA_real_),
      RockFragments60 = dplyr::if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, RockFragments, NA_real_),
    )
  
  # get min and max effer data, then rejoin to hz table
  hzeff_aim <- hz_aim %>%
    dplyr::select(
      SoilKey, Effer
    ) %>%
    # make ordinal effer field, to get min and max
    dplyr::mutate(
      EfferOrdinal = dplyr::case_when(
        Effer == "NE" ~ 1,
        Effer == "VS" ~ 2,
        Effer == "SL" ~ 3,
        Effer == "ST" ~ 4,
        Effer == "VE" ~ 5),
    ) %>%
    dplyr::group_by(SoilKey) %>%
    dplyr::summarize(MinEffer = min(EfferOrdinal),
              MaxEffer = max(EfferOrdinal),
              .groups = "drop") %>%
    dplyr::mutate( # send the ordinal effervescence back to the character/factor data
      MinEffer = dplyr::case_when(
        MinEffer == 1 ~ "NE",
        MinEffer == 2 ~ "VS",
        MinEffer == 3 ~ "SL",
        MinEffer == 4 ~ "ST",
        MinEffer == 5 ~ "VE"),
      MaxEffer = dplyr::case_when(
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
    dplyr::arrange(DBKey, PrimaryKey, SoilKey, HorizonDepthUpper) %>%
    dplyr::summarize(across(everything(), ~ dplyr::first(na.omit(.x))),
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
    dplyr::left_join(hzeff_aim, by = "SoilKey")
  
  ### prepare pit and plot level data ####
  # pit: mostly for metadata, also total pit depth and total rock frags
  pit_aim <- pit_aim_raw %>%
    dplyr::select(
      PrimaryKey, DBKey, SoilKey,
      TotalFragments = RockFragments, TotalPitDepth = SoilDepthLower, PitDepthMeasure = DepthMeasure,
      PitNotes = Notes) %>%
    # unify depth measures and drop depth measure var
    dplyr::mutate(
      TotalPitDepth = dplyr::case_when(PitDepthMeasure == "in" ~ TotalPitDepth * 2.54,
                           PitDepthMeasure == "cm" | is.na(PitDepthMeasure) ~ TotalPitDepth),
    ) %>%
    dplyr::select(-PitDepthMeasure)
  
  ### join all aim-style data, final touches ####
  soil_aim <-
    dplyr::left_join(hzjoin_aim, pit_aim, by = c("PrimaryKey", "DBKey", "SoilKey")) %>%
    # final drop line
    dplyr::select(-SoilKey)
  
  if("sf" %in% class(soil_aim)) soil_aim <- soil_aim %>% sf::st_drop_geometry()
  
  return(soil_aim)
}

#' @export gather_soil_summary
#' @rdname gather_soil_summary
gather_soil_summary <- function(dsn = NULL, source,
                                tblSoilPitHorizons = NULL, tblSoilPits = NULL,
                                SOILHORIZON = NULL){
  
 
  if(toupper(source) %in% c("AIM", "TERRADAT")) {
    if(is.null(dsn) & (is.null(tblSoilPits) | is.null(tblSoilPitHorizons))){
      stop("If source is AIM or TerrADat, you must provide either a geodatabase or tblSoilPits and tblSoilPitHorizons")}
    soil <- gather_soil_summary_terradat(dsn = dsn, tblSoilPits = tblSoilPits,
                                    tblSoilPitHorizons = tblSoilPitHorizons)}
  
  if(toupper(source) %in% c("LMF", "NRI")){
    if(is.null(dsn) & is.null(SOILHORIZON)){
      stop("If source is LMF or NRI, you must provide either a geodatabase or table SOILHORIZON")}
    soil <- gather_soil_summary_lmf(dsn = dsn, SOILHORIZON = SOILHORIZON)
  }
  
  soil$source <- source
  
  if("sf" %in% class(soil)) soil <- sf::st_drop_geometry(soil)
  
  # I do not remember why I though this was necessary. Remove in a later release if nothing breaks
  #if(length(class(soil) > 1)) soil <- as.data.frame(soil)
  
  return(soil)
}

