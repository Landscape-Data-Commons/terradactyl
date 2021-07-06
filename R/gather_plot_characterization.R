


gather_plot_characterization <- function(dsn = NULL,
                                         source,
                                         tblPlots = NULL,
                                         POINT = NULL,
                                         file_type = "gdb"){
  
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    plotchar <- gather_plot_characterization_terradat(dsn = dsn,
                                                      tblPlots = tblPlots
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    # stop("NRI/LMF data not yet supported")
    plotchar <- gather_plot_characterization_lmf(dsn = dsn,
                                                 file_type = file_type,
                                                 POINT = POINT,
                                                 source = source)
  } else {
    stop("No valid source provided")
  }
  
  plotchar$Source <- toupper(source)  
  
  if("sf" %in% class(plotchar)) plotchar <- sf::st_drop_geometry(plotchar)
  
  return(plotchar)
}

gather_plot_characterization_terradat <- function(dsn = NULL, tblPlots = NULL){
  if(!is.null(tblPlots)){
    plot_raw <- tblPlots
  } else if(!is.null(dsn)){
    plot_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblPlots", stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("Supply either tblPlots or the path to a GDB containing that table")
  }
  
  plot_tall <- plot_raw %>%
    dplyr::select(
      # keys
      PrimaryKey, DBKey, #PlotID, 
      
      # data
      EcolSite, Soil, ParentMaterial, Slope, Aspect, SlopeShape = ESD_SlopeShape,
      LandscapeType, LandscapeTypeSecondary, HillslopeType,
      
      # plot history data -- keep?
      # make disturbanceAny?
      RecentWeatherPast12, RecentWeatherPrevious12,
      DisturbWildfire, DisturbRodents, DisturbMammals, DisturbWater, DisturbWind,
      DisturbWaterSoilDep, DisturbWindSoilDep, DisturbUndgroundUtils, DisturbOverhdTransLines, DisturbOther,
      DisturbOtherDesc,
      WildlifeUse, MgtHistory, OffsiteInfluences,
      
      # # esd data # how many of these are really needed
      EcolSite, SoilSeries = ESD_Series, # MLRA = ESD_MLRA, CRA = ESD_CRA, ESD_Region, 
      # ESD_Investigators, ESD_Bedrock, ESD_MajorLandform,
      # ESD_ComponentLandform, ESD_GeomorphicComp, ESD_RunIn_RunOff, ESD_SlopeComplexity,
      
      # metadata
      EstablishDate, State, County,
      
      # convert distub vars into logical, calc an any disturbance var
      # recode slope shape and split into 2 vars (hz and vert)
    ) %>% mutate(
      across(c(DisturbWildfire:DisturbOther), as.logical),
      DisturbAny =
        DisturbWildfire | DisturbRodents | DisturbMammals | DisturbWater |
        DisturbWind | DisturbWaterSoilDep | DisturbWindSoilDep |
        DisturbUndgroundUtils | DisturbOverhdTransLines | DisturbOther,
      SlopeShapeVertical = case_when(
        SlopeShape %in% c("CC", "CV", "CL", "concave concave", "concave convex", "concave linear") ~ "concave",
        SlopeShape %in% c("LC", "LV", "LL", "linear concave", "linear convex", "linear linear") ~ "linear",
        SlopeShape %in% c("VC", "VV", "VL", "convex concave", "convex convex", "convex linear") ~ "convex"
      ),
      SlopeShapeHorizontal = case_when(
        SlopeShape %in% c("CC", "LC", "VC", "concave concave", "linear concave", "convex concave") ~ "concave",
        SlopeShape %in% c("CL", "LL", "VL", "concave linear", "linear linear", "convex linear") ~ "linear",
        SlopeShape %in% c("CV", "LV", "VV", "concave convex", "linear convex", "convex convex") ~ "convex"
      ),
      Aspect = suppressWarnings(as.numeric(Aspect)),
      
    ) %>% dplyr::select(-SlopeShape)
  
  return(plot_tall)
}

gather_plot_characterization_lmf <-   function(dsn = NULL, SOILHORIZON = NULL, ESFSG = NULL, COUNTYNM = NULL, POINT = NULL,  file_type = file_type, source) {
  ### input ####
  if (!is.null(SOILHORIZON) & !is.null(ESFSG) & !is.null(COUNTYNM) & !is.null(POINT)){
    hz_lmf_raw <- SOILHORIZON
    esg_lmf_raw <- ESFSG
    co_lmf_raw <- COUNTYNM
    point_lmf_raw <- POINT
  } else if(!is.null(dsn)){
    hz_lmf_raw <- switch(source, LMF = {
      sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T)
    }, NRI = {
      readRDS(dsn)
    })
    
    # get ecolsite fromt table LMF? is LMF data in NRI?    
    esg_lmf_raw <- switch(source, LMF = {
      sf::st_read(dsn = dsn, layer = "ESFSG", stringsAsFactors = FALSE, quiet = T)
    }, NRI = {
      readRDS(dsn)
    })
    
    co_lmf_raw <- switch(source, LMF = {
      sf::st_read(dsn = dsn, layer = "COUNTYNM", stringsAsFactors = FALSE, quiet = T)
    }, NRI = {
      readRDS(dsn)
    })
    
    point_lmf_raw <-
      switch(source, LMF = {
        sf::st_read(dsn = dsn, layer = "POINT", stringsAsFactors = FALSE, quiet = T)
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
    )%>% 
    # arrange by hz depth in order to id upper depth
    arrange(PrimaryKey, HorizonDepthLower)  %>%
    dplyr::mutate(
      HorizonDepthLower = as.numeric(2.54 * HorizonDepthLower), # is lmf data ALWAYS in inches? 
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
              PitDepth = max(HorizonDepthLower),
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
  
  hzjoin_lmf <- hz_lmf %>% # edited from aim version above
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
      -HorizonKey, -HorizonDepthLower, -Effer, -DepthUOM, -Texture, -HorizonDepthUpper,
      -Texture, -TextureModifier,) %>%
    left_join(hzeff_lmf, by = "PrimaryKey") 
  
  ### prepare non-soil level data ####
  ### CUT THIS? LMF TABLE HAS THE PARSED ECOLSITE ALREADY. MAY NOT BE IN NRI, DONT CUT YET
  esg_lmf <- esg_lmf_raw %>% dplyr::select(
    PrimaryKey, StateNo = STATE, CountyNo = COUNTY,
    ESD_State = ESFSG_STATE, ESD_MLRA = ESFSG_MLRA, ESD_Site = ESFSG_SITE,
    PlotKey = PLOTKEY, 
  ) %>% dplyr::mutate(
    EcolSite = paste0(
      ESD_State, ESD_MLRA, ESD_Site ### LOTS OF QC NEEDED HERE!
    ),
  ) %>% left_join(
    co_lmf_raw %>% select(StateNo = STATE, CountyNo = COUNTY, State = PrimaryKey, County = COUNTYNM),
    by = c("StateNo", "CountyNo")
  ) %>% dplyr::select(
    -StateNo, -CountyNo, 
    -ESD_State, -ESD_Site, -ESD_MLRA, # readd these? could parse out in AIM data
  ) %>% dplyr::group_by(PrimaryKey) %>% 
    dplyr::summarize(across(everything(), ~ first(na.omit(.x))),
                     .groups = "drop")
  
  # get slope shape from POINT
  point_lmf <- point_lmf_raw %>% dplyr::select(
    DBKey, PrimaryKey, PlotKey = PLOTKEY, SlopeShapeVertical = VERTICAL_SLOPE_SHAPE, 
    SlopeShapeHorizontal = HORIZONTAL_SLOPE_SHAPE,
    Slope = SLOPE_PERCENT, Aspect = SLOPE_ASPECT
  ) %>% dplyr::mutate(
    # reclass aspect into degrees
    Aspect = suppressWarnings(as.numeric(recode(Aspect,
                                                "0" = "N", "45" = "NE","90" = "E", "135" = "SE", 
                                                "180" = "S", "225" = "SW", "270" = "W","315" = "NW")))
  )
  
  ### join lmf type data ####
  soil_lmf <- left_join(
    esg_lmf, hzjoin_lmf, by = c("PrimaryKey")
  ) %>% left_join(
    point_lmf, by = c("PrimaryKey", "PlotKey", "DBKey")
  )
  
  return(soil_lmf)
  
}
