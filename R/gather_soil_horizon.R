#' Convert horizon data into a tall, tidy data frame
#'
#' @description Given wide format soil horizon data, create a tall 
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file 
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either tblSoilPitHorizons
#' (AIM/DIMA/TerrADat) or SOILHORIZON (LMF/NRI).
#' @param source Character string. The data source format, 
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblSoilPitHorizons Dataframe of the data structure tblSoilPitHorizons 
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from the 
#' LMF/NRI database with the addition of PrimaryKey and DBKey fields;
#' alternately provide dsn.
#' @importFrom magrittr %>%
#' @name gather_soil_horizon
#' @family <gather>
#' @return A tall data frame containing soil horzon data.

#' @export gather_soil_horizon_terradat
#' @rdname gather_soil_horizon

gather_soil_horizon_terradat <- function(dsn = NULL, tblSoilPitHorizons = NULL){
  
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(tblSoilPitHorizons)) {
    hz_aim_raw <- tblSoilPitHorizons
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblSoilPitHorizons")
    }
    
    hz_aim_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
                                               stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("Supply either tblSoilPitHorizons, or the path to a GDB containing those tables")
  }
  
  horizons_aim <- hz_aim_raw %>%
    ### select ###
    dplyr::select(
      PrimaryKey, DBKey, HorizonKey, HorizonDepthUpper, HorizonDepthLower, 
      DepthUOM = DepthMeasure, HorizonName = ESD_Horizon, 
      
      RockFragments, Texture, TextureModifier = ESD_HorizonModifier,
      pH = ESD_pH, EC = ESD_EC, Effervescence = Effer, 
      ClayPct = ESD_PctClay, SandPct = ESD_PctSand,
      
      StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure, 
      StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2, 
      StructureQuality = ESD_StructQual,
      
      PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic, ClayFilm = ESD_ClayFilm,
      Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color, 
      RootSize = ESD_RootSize, RootQty = ESD_RootQty,
      
      Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
      Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
      Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,
      
      HorizonNotes = ESD_Notes, 
      
      ### all of these variables arent present in the ldc data as of v0.9. Disable (no code has been deleted, merely commented)
      # RuptureResistance = ESD_RuptureResistance, # data almost entirely missing
      # sar = ESD_NAabsorptionRatio, # NAabsorptionRatio = ESD_NAabsorptionRatio, 
      # caco3 = ESD_CaCO3EquivPct, #CaCO3EquivalentPct = ESD_CaCO3EquivPct, ## caco3 data not present
      # sandvf = ESD_SandFractPctVeryFine, #SandFractPctVeryFine = "ESD_SandFractPctVeryFine",
      # sandfine = ESD_SandFractPctFine, #SandFractPctFine = "ESD_SandFractPctFine", 
      # sandmed = ESD_SandFractPctMed, #SandFractPctMed = "ESD_SandFractPctMed",
      # sandco = ESD_SandFractPctCoarse, # SandFractPctCoarse = "ESD_SandFractPctCoarse",
      # sandvc = ESD_SandFractPctVeryCoarse, # SandFractPctVeryCoarse = "ESD_SandFractPctVeryCoarse",
      # gypsum = ESD_GypsumPct, #Gypsum_Pct = ESD_GypsumPct,
      # fraground = ESD_FragmentRoundness, #FragmentRoundness = "ESD_FragmentRoundness",
      # poresize = ESD_PoresSize, #PoresSize = "ESD_PoresSize", 
      # poreqty = ESD_PoresQty, #PoresQty = "ESD_PoresQty", 
      # CarbonateStage = ESD_CarbonateStage, 
      # GravelClassPctFine = ESD_GravelClassPctFine,
      # GravelClassPctMed = ESD_GravelClassPctMed,
      # GravelClassPctCoarse = ESD_GravelClassPctCoarse,
      # GravelCarbonateCoatPct = ESD_GravelCarbonateCoatPct,
      # sandtotal_psa = ESD_PSAPctSand, #PSA_SandPct = ESD_PSAPctSand, 
      # silttotal_psa = ESD_PSAPctSilt, #PSA_SiltPct = ESD_PSAPctSilt,
      # claytotal_psa = ESD_PSAPctClay #PSA_ClayPct = ESD_PSAPctClay, 
      
      
      ### cleaning ###
    ) %>%
    mutate_all(
      stringr::str_trim # defensive, early qc seems to catch this well
    ) %>%
    ### recode class data###
    mutate(
      StructureGrade = recode(StructureGrade, 
                              "0" = "Structureless",
                              "1" = "Weak",
                              "2" = "Moderate",
                              "3" = "Strong"),
      StructureGrade2 = recode(StructureGrade2,
                               "0" = "Structureless",
                               "1" = "Weak",
                               "2" = "Moderate",
                               "3" = "Strong"),
      StructureSize = recode(StructureSize %>% tolower(), 
                             "vf" = "Very fine",
                             "vn" = "Very thin",
                             "f"  = "Fine",
                             "tn" = "Thin",
                             "m"  = "Medium",
                             "co" = "Coarse",
                             "tk" = "Thick",
                             "vc" = "Very coarse",
                             "vk" = "Very thick",
                             "ec" = "Extremely coarse"),
      StructureSize2 = recode(StructureSize2 %>% tolower(), 
                              "vf" = "Very fine",
                              "vn" = "Very thin",
                              "f"  = "Fine",
                              "tn" = "Thin",
                              "m"  = "Medium",
                              "co" = "Coarse",
                              "tk" = "Thick",
                              "vc" = "Very coarse",
                              "vk" = "Very thick",
                              "ec" = "Extremely coarse"),
      StructureType = recode(StructureType %>% tolower(),
                             "gr"  = "Granular",
                             "abk" = "Angular blocky",
                             "sbk" = "Subangular blocky",
                             "pl"  = "Platy",
                             "weg" = "Wedge",
                             "pr"  = "Prismatic",
                             "col" = "Columnar",
                             "sg"  = "Single grain",
                             "ma"  = "Massive",
                             "cdy" = "Cloddy",
                             "other" = "Other"),
      StructureType2 = recode(StructureType2 %>% tolower(),
                              "gr"  = "Granular",
                              "abk" = "Angular blocky",
                              "sbk" = "Subangular blocky",
                              "pl"  = "Platy",
                              "weg" = "Wedge",
                              "pr"  = "Prismatic",
                              "col" = "Columnar",
                              "sg"  = "Single grain",
                              "ma"  = "Massive",
                              "cdy" = "Cloddy",
                              "other" = "Other"),
      ## these columns unused. preserved code just in case
      # fraground = recode(fraground %>% tolower(),
      #                    "va" = "Very angular",
      #                    "an" = "Angular",
      #                    "sa" = "Subangular",
      #                    "sr" = "Subrounded",
      #                    "ro" = "Rounded",
      #                    "wr" = "Well rounded"),
      # poresize = recode(poresize %>% tolower(),
      #                   "vf" = "Very fine",
      #                   "f"  = "Fine",
      #                   "m"  = "Medium",
      #                   "c"  = "Coarse",
      #                   "vc" = "Very coarse"),
      # RuptureResistance = recode(RuptureResistance %>% tolower(),
      #                            "ef" = "Extr. Firm",
      #                            "eh" = "Extr. Hard",
      #                            "fi" =	"Firm",
      #                            "fr" =	"Friable",
      #                            "ha" =	"Hard",
      #                            "l"  =	"Loose",
      #                            "mh" =	"Mod. Hard",
      #                            "r"  =	"Rigid",
      #                            "s"  =	"Soft",
      #                            "sh" =	"Slightly Hard",
      #                            "sr" =	"Slightly Rigid",
      #                            "vfi" =	"Very Firm",
      #                            "vfr" =	"Very Friable",
      #                            "vh" =	"Very Hard",
      #                            "vr" =	"Very Rigid",
      #                            
      #                            # "" = NA_character_ # "" returns a zero-l varname error. Why?
      #                            # workaround may be over-general. This will eliminate any data validation errs that could be fixed (eg mispellings) 
      #                            .default = NA_character_),
    ) %>%
    ### complex mutates that depend on >1 var ###
    mutate(
      SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
      FragVolGravel = case_when(
        Fragment1Type == "1" ~ Fragment1VolPct,
        Fragment2Type == "1" ~ Fragment2VolPct,
        Fragment3Type == "1" ~ Fragment3VolPct
      ), 
      FragVolCobble = case_when(
        Fragment1Type == "2" ~ Fragment1VolPct,
        Fragment2Type == "2" ~ Fragment2VolPct,
        Fragment3Type == "2" ~ Fragment3VolPct
      ),
      FragVolStone = case_when(
        Fragment1Type == "6" ~ Fragment1VolPct,
        Fragment2Type == "6" ~ Fragment2VolPct,
        Fragment3Type == "6" ~ Fragment3VolPct
      ),      
      FragVolNodule = case_when(
        Fragment1Type == "8" ~ Fragment1VolPct,
        Fragment2Type == "8" ~ Fragment2VolPct,
        Fragment3Type == "8" ~ Fragment3VolPct
      ),      
      FragVolDurinode = case_when(
        Fragment1Type == "9" ~ Fragment1VolPct,
        Fragment2Type == "9" ~ Fragment2VolPct,
        Fragment3Type == "9" ~ Fragment3VolPct
      ),    
      HorizonDepthLower = case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
      HorizonDepthUpper = case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54,
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
      DepthUOM = "cm"
    ) %>%
    ### drop vars that are no longer relevant ###
    dplyr::select(
      -Fragment1Type,
      -Fragment2Type,
      -Fragment3Type,
      -Fragment1VolPct,
      -Fragment2VolPct,
      -Fragment3VolPct,
      HorizonKey,
    )  %>% group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
      PrimaryKey
    ) %>%
    mutate(HorizonNumber = as.character(row_number()),
           across(c(#caco3,gypsum #  data seems to not be used, disabled
             RockFragments), ~ suppressWarnings(as.integer(.x))),
           across(c(#sar, sandvf,
             pH, # sandfine, sandmed, sandco, sandvc,
             EC, ClayPct, SandPct, SiltPct, # poreqty, 
             FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule, 
             FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
             #sandtotal_psa, silttotal_psa, claytotal_psa, 
           ), ~ suppressWarnings(as.double(.x))),
           across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
    )
  
  if("sf" %in% class(horizons_aim)) horizons_aim <- horizons_aim %>% sf::st_drop_geometry()
  
  return(horizons_aim)
}

#' @export gather_soil_horizon_lmf
#' @rdname gather_soil_horizon
gather_soil_horizon_lmf <- function(dsn = NULL,  SOILHORIZON = NULL){
  
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(SOILHORIZON)){
    hz_lmf_raw <- SOILHORIZON
  } else if(!is.null(dsn)){
    
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing SOILHORIZON")
    }
    
    hz_lmf_raw <- switch(source, LMF = {
      suppressWarnings(sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T))
    }, NRI = {
      readRDS(dsn)
    })
  } else{
    stop("Supply either SOILHORIZON or the path to a GDB containing that table")
  }
  
  horizons_lmf <- hz_lmf_raw %>% 
    dplyr::select(
      PrimaryKey, DBKey, HorizonNumber = SEQNUM,
      HorizonDepthLower = DEPTH, Effervescence = EFFERVESCENCE_CLASS,
      Texture = HORIZON_TEXTURE, TextureModifier = TEXTURE_MODIFIER,
      HorizonNotes = UNUSUAL_FEATURES
    )
  
  horizons_lmf <- horizons_lmf %>% # have to have already created horizons_lmf before the HorizonDepthUpper parsing below, as it refers to the df by name
    mutate(
      DepthUOM = "cm", 
      HorizonNumber = as.character(HorizonNumber), 
      HorizonDepthUpper = sapply(unique(PrimaryKey), function(x) {
        lower <- horizons_lmf$HorizonDepthLower[horizons_lmf$PrimaryKey == x]
        upper <- c(0, lower[1:length(lower) - 1])
        return(upper)}
      ) %>% unlist()   
      ### ARE THEY ALWAYS INCHES? No measure type recorded, though they may use decifeet sometimes
      HorizonDepthLower = suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
      HorizonDepthUpper = suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54
    )
  
  return(horizons_lmf)
}

#' @export gather_soil_horizon
#' @rdname gather_soil_horizon
gather_soil_horizon <- function(dsn = NULL, source, SOILHORIZON = NULL, tblSoilPitHorizons = NULL) {
  
  if( toupper(source) %in% c("AIM", "TERRADAT")) {
    soil <- gather_soil_horizon_terradat(dsn = dsn, tblSoilPitHorizons = tblSoilPitHorizons)
  } else if( toupper(source) %in% c("LMF", "NRI")){
    soil <- gather_soil_horizon_lmf(dsn = dsn, SOILHORIZON = SOILHORIZON)
  } else {
    stop("source must be AIM, TerraDat, DIMA, LMF, or NRI (all case independent)")
  }
  
  soil$Source <- source
  
  if("sf" %in% class(soil)) soil <- sf::st_drop_geometry(soil)
  
  return(soil)
}