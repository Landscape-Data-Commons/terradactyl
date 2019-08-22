#' Gather soil horizon data
#'
#'
dsn <- "~/AIM/Data/TerradatCalcs/FullCopyfor2018UTCOIDMTNMCAORWAIngest8-19-19.gdb/FullCopyfor2018UTCOIDMTNMCAORWAIngest.gdb"
source = "LMF"

gather_soil_horizons <- function(dsn, source) {
  # Load soil horizon data
  if(source %in% c("AIM", "TerrADat")) {

    soil_aim <- dplyr::left_join(sf::st_read(dsn = dsn, layer="tblSoilPitHorizons"),
                                          sf::st_read(dsn = dsn, layer="tblSoilPits")) %>%
      dplyr::select(PrimaryKey, DBKey, DateLoadedInDb,
                    HorizonKey, DepthUOM = DepthMeasure, HorizonDepthUpper,
                    HorizonDepthLower,
                    PitDescription = "PitDesc",
                    Texture, TotalRockFragmentsPct = RockFragments,
                    Effervescence = Effer,
                    HorizonName = ESD_Horizon,
                    HorizonName_Modifier = ESD_HorizonModifier,
                    FragmentVolumePct = ESD_FragVolPct,
                    FragmentType = ESD_FragmentType,
                    PetrocalcicRubble = ESD_PetrocalcicRubble,
                    Gypsic = ESD_Gypsic,
                    ClayPct = ESD_PctClay,
                    SandPct = "ESD_PctSand",
                    Hue = ESD_Hue,
                    Value = ESD_Value,
                    Chroma = ESD_Chroma,
                    Color = ESD_Color,
                    Grade = ESD_Grade,
                    Size = ESD_Size,
                    Structure = ESD_Structure,
                    StructureQuality = ESD_StructQual,
                    Grade2 = ESD_Grade2,
                    Size2 = ESD_Size2,
                    Structure2 = ESD_Structure2,
                    RuptureResistance = ESD_RuptureResistance,
                    ClayFilm = ESD_ClayFilm,
                    CarbonateStage = ESD_CarbonateStage,
                    CaCO3EquivalentPct = ESD_CaCO3EquivPct,
                    EC = ESD_EC,
                    pH = ESD_pH,
                    Gypsum_Pct = ESD_GypsumPct,
                    NAabsorptionRatio = ESD_NAabsorptionRatio,
                    HorizonNotes = ESD_Notes,
                    GravelClassPctFine = "ESD_GravelClassPctFine",
                    GravelClassPctMed = "ESD_GravelClassPctMed",
                    GravelClassPctCoarse = "ESD_GravelClassPctCoarse",
                    GravelCarbonateCoatPct = "ESD_GravelCarbonateCoatPct",
                    FragmentRoundness = "ESD_FragmentRoundness",
                    RootSize = "ESD_RootSize",
                    RootQty = "ESD_RootQty", PoresSize = "ESD_PoresSize",
                    PoresQty = "ESD_PoresQty",
                    SandFractPctVeryFine = "ESD_SandFractPctVeryFine",
                    SandFractPctFine = "ESD_SandFractPctFine",
                    SandFractPctMed = "ESD_SandFractPctMed",
                    SandFractPctCoarse = "ESD_SandFractPctCoarse",
                    SandFractPctVeryCoarse = "ESD_SandFractPctVeryCoarse",
                    FragmentVolPct2 = "ESD_FragVolPct2",
                    FragmentType2 = "ESD_FragmentType2",
                    FragmentVolumePct3 = "ESD_FragVolPct3",
                    FragmentType3 = "ESD_FragmentType3",
                    PSA_SandPct = "ESD_PSAPctSand",
                    PSA_SiltPct = "ESD_PSAPctSilt",
                    PSA_ClayPct ="ESD_PSAPctClay",
                    LabGravelPctFine = "ESD_LabGravelPctFine",
                    LabGravelPctMed = "ESD_LabGravelPctMed",
                    LabGravelPctCoarse = "ESD_LabGravelPctCoarse"
                    )

    # convert horizon depth lower and horizon depth upper to numerice
    soils_aim <- soil_aim %>%
      dplyr::mutate(HorizonDepthLower = stringr::str_extract(HorizonDepthLower, "[[:digit:]]")
                    %>% as.numeric(),
                    HorizonDepthUpper = stringr::str_extract(HorizonDepthUpper, "[[:digit:]]")
                    %>% as.numeric(),
                    # add source field
                    source = "LMF")

  }

  if (source %in% c("LMF", "NRI")){
    soil_lmf <- sf::st_read(dsn = dsn, layer="SOILHORIZON") %>%
    dplyr::select(PrimaryKey, DBKey,
                  HorizonKey = SEQNUM,
                  HorizonDepthLower=DEPTH,
                  Efferervescence=EFFERVESCENCE_CLASS,
                  Texture=HORIZON_TEXTURE,
                  TextureModifier = TEXTURE_MODIFIER,
                  Notes = UNUSUAL_FEATURES
                  ) %>%

    # convert to cm
      dplyr::mutate(DepthUOM = "in",
                    HorizonKey = as.character(HorizonKey),
                    # add source field
                    source = "LMF",
                    # Add upper limits to the horizon
                    HorizonDepthUpper = sapply(unique(soil_lmf$PrimaryKey),
                                   function(x){
                                     lower <- soil_lmf$HorizonDepthLower[soil_lmf$PrimaryKey == x]
                                     upper <- c(0,lower[1:length(lower)-1])
                                     return(upper)
                                   }

                    ) %>% unlist()
      )



  }

  # combine soil tables
  soil <- dplyr::bind_rows(if(exists("soil_aim")) soil_aim,
                           if(exists("soil_lmf")) soil_lmf)

  # update units in inches to centimeters
  soil <- soil %>%
    dplyr::mutate(HorizonDepthLower = dplyr::case_when(DepthUOM == "in" ~ HorizonDepthLower*2.54,
                                                       DepthUOM == "cm" ~ HorizonDepthLower),
                  HorizonDepthUpper = dplyr::case_when(DepthUOM == "in" ~ HorizonDepthUpper*2.54,
                                                       DepthUOM == "cm" ~ HorizonDepthUpper),
                  DepthUOM = "cm")


  }
