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
#' @examples
#' gather_soil_horizon(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                     source = "AIM")
#' gather_soil_horizon(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                     source = "LMF")
#'
#' aim_horizons <- read.csv("Path/To/tblSoilPitHorizons.csv")
#' gather_soil_horizon(source = "AIM",
#'                     tblSoilPitHorizons = aim_horizons)
#'
#' lmf_horizons <- read.csv("Path/To/SOILHORIZON.csv")
#' gather_soil_horizon(source = "LMF",
#'                     SOILHORIZON = lmf_horizons)

#' @export gather_soil_horizon_terradat
#' @rdname gather_soil_horizon
gather_soil_horizon_terradat <- function(dsn = NULL,
                                         tblSoilPitHorizons = NULL){

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
      Texture, TextureModifier = ESD_HorizonModifier,
      pH = ESD_pH, EC = ESD_EC, Effervescence = Effer,
      ClayPct = ESD_PctClay, SandPct = ESD_PctSand,

      StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure,
      # StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2,
      StructureQuality = ESD_StructQual,

      # PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic,
      # ClayFilm = ESD_ClayFilm,
      Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color,
      # RootSize = ESD_RootSize, RootQty = ESD_RootQty,

      Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
      Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
      Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,

      HorizonNotes = ESD_Notes

      ### cleaning ###
    ) %>%
    dplyr::mutate_all(
      stringr::str_trim # defensive, early qc seems to catch this well
    ) %>%
    ### recode class data###
    dplyr::mutate(
      StructureGrade = dplyr::recode(StructureGrade,
                              "0" = "Structureless",
                              "1" = "Weak",
                              "2" = "Moderate",
                              "3" = "Strong"),
      # StructureGrade2 = dplyr::recode(StructureGrade2,
      #                          "0" = "Structureless",
      #                          "1" = "Weak",
      #                          "2" = "Moderate",
      #                          "3" = "Strong"),
      StructureSize = dplyr::recode(StructureSize %>% tolower(),
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
      # StructureSize2 = dplyr::recode(StructureSize2 %>% tolower(),
      #                         "vf" = "Very fine",
      #                         "vn" = "Very thin",
      #                         "f"  = "Fine",
      #                         "tn" = "Thin",
      #                         "m"  = "Medium",
      #                         "co" = "Coarse",
      #                         "tk" = "Thick",
      #                         "vc" = "Very coarse",
      #                         "vk" = "Very thick",
      #                         "ec" = "Extremely coarse"),
      StructureType = dplyr::recode(StructureType %>% tolower(),
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
      # StructureType2 = dplyr::recode(StructureType2 %>% tolower(),
      #                         "gr"  = "Granular",
      #                         "abk" = "Angular blocky",
      #                         "sbk" = "Subangular blocky",
      #                         "pl"  = "Platy",
      #                         "weg" = "Wedge",
      #                         "pr"  = "Prismatic",
      #                         "col" = "Columnar",
      #                         "sg"  = "Single grain",
      #                         "ma"  = "Massive",
      #                         "cdy" = "Cloddy",
      #                         "other" = "Other"),
    ) %>%
    ### complex mutates that depend on >1 var ###
    dplyr::mutate(
      SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
      FragVolGravel = dplyr::case_when(
        Fragment1Type %in% c("GR", "Gravel", "1") ~ Fragment1VolPct,
        Fragment2Type %in% c("GR", "Gravel", "1") ~ Fragment2VolPct,
        Fragment3Type %in% c("GR", "Gravel", "1") ~ Fragment3VolPct
      ),
      FragVolCobble = dplyr::case_when(
        Fragment1Type %in% c("CB", "Cobble", "2") ~ Fragment1VolPct,
        Fragment2Type %in% c("CB", "Cobble", "2") ~ Fragment2VolPct,
        Fragment3Type %in% c("CB", "Cobble", "2") ~ Fragment3VolPct
      ),
      FragVolStone = dplyr::case_when(
        Fragment1Type %in% c("ST", "Stone", "6") ~ Fragment1VolPct,
        Fragment2Type %in% c("ST", "Stone", "6") ~ Fragment2VolPct,
        Fragment3Type %in% c("ST", "Stone", "6") ~ Fragment3VolPct
      ),
      FragVolNodule = dplyr::case_when(
        Fragment1Type %in% c("8", "Nodule") ~ Fragment1VolPct,
        Fragment2Type %in% c("8", "Nodule") ~ Fragment2VolPct,
        Fragment3Type %in% c("8", "Nodule") ~ Fragment3VolPct
      ),
      FragVolDurinode = dplyr::case_when(
        Fragment1Type %in% c("9", "Durinode") ~ Fragment1VolPct,
        Fragment2Type %in% c("9", "Durinode") ~ Fragment2VolPct,
        Fragment3Type %in% c("9", "Durinode") ~ Fragment3VolPct
      ),
      HorizonDepthLower = dplyr::case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
      HorizonDepthUpper = dplyr::case_when(
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
    )   %>%
    dplyr::arrange(PrimaryKey, HorizonDepthUpper) %>%
    dplyr::group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
      PrimaryKey
    ) %>%

    dplyr::mutate(HorizonNumber = as.character(dplyr::row_number()),
           # across(c(RockFragments), ~ suppressWarnings(as.integer(.x))),
           across(c(pH,
             EC, ClayPct, SandPct, SiltPct, # poreqty,
             FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule,
             FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
           ), ~ suppressWarnings(as.double(.x))),
           # across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
    )
  horizons_aim <- as.data.frame(horizons_aim)

  return(horizons_aim)
}

#' @export gather_soil_horizon_lmf
#' @rdname gather_soil_horizon
gather_soil_horizon_lmf <- function(dsn = NULL,
                                    SOILHORIZON = NULL){
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(SOILHORIZON)){
    hz_lmf_raw <- SOILHORIZON
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing SOILHORIZON")
    }
    hz_lmf_raw <- sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T)
  } else {
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
    dplyr::mutate(
      DepthUOM = "cm",
      HorizonNumber = as.character(HorizonNumber),
      HorizonDepthUpper = sapply(unique(PrimaryKey), function(x) {
        lower <- horizons_lmf$HorizonDepthLower[horizons_lmf$PrimaryKey == x]
        upper <- c(0, lower[1:length(lower) - 1])
        return(upper)}
      ) %>% unlist(),
      ### ARE THEY ALWAYS INCHES? No measure type recorded, though they may use decifeet sometimes
      HorizonDepthLower = suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
      HorizonDepthUpper = suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54
    )

  return(horizons_lmf)
}
#' @export gather_soil_horizon_survey123
#' @rdname gather_soil_horizon
gather_soil_horizon_survey123 <- function(dsn = NULL,
                                          PlotChar_0 = NULL,
                                          SoilPitHorizons_1 = NULL){

  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(SoilPitHorizons_1) & !is.null(PlotChar_0)) {
    hz_raw <- SoilPitHorizons_1
    plotchar_raw <- PlotChar_0
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblSoilPitHorizons")
    }

    hz_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
                                               stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("Supply either tblSoilPitHorizons, or the path to a GDB containing those tables")
  }

  # Survey123 data uses PlotKey instead of PrimaryKey
  hz_raw <- dplyr::left_join(hz_raw, plotchar_raw %>% dplyr::select(PrimaryKey = PlotKey, GlobalID), by = c("ParentGlobalID" = "GlobalID"))

  # Check for duplicate PrimaryKeys
  dupkeys <- hz_raw$PrimaryKey[duplicated(hz_raw$PrimaryKey)]
  if(length(dupkeys) > 0){
    dupnames <- paste(dupkeys, collapse = ", ")
    warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
  }

  horizons <- hz_raw %>%
    ### select ###
    dplyr::select(
      PrimaryKey, DBKey, HorizonKey, HorizonDepthUpper, HorizonDepthLower,
      DepthUOM = DepthMeasure, HorizonName = ESD_Horizon,
      Texture, TextureModifier = ESD_HorizonModifier,
      pH = ESD_pH, EC = ESD_EC, Effervescence = Effer,
      ClayPct = ESD_PctClay, SandPct = ESD_PctSand,

      StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure,
      # StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2,
      StructureQuality = ESD_StructQual,

      # PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic,
      # ClayFilm = ESD_ClayFilm,
      Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color,
      # RootSize = ESD_RootSize, RootQty = ESD_RootQty,

      Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
      Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
      Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,

      HorizonNotes = ESD_Notes

      ### cleaning ###
    ) %>%
    dplyr::mutate_all(
      stringr::str_trim # defensive, early qc seems to catch this well
    ) %>%
    ### recode class data###
    dplyr::mutate(
      StructureGrade = dplyr::recode(StructureGrade,
                                     "0" = "Structureless",
                                     "1" = "Weak",
                                     "2" = "Moderate",
                                     "3" = "Strong"),
      # StructureGrade2 = dplyr::recode(StructureGrade2,
      #                          "0" = "Structureless",
      #                          "1" = "Weak",
      #                          "2" = "Moderate",
      #                          "3" = "Strong"),
      StructureSize = dplyr::recode(StructureSize %>% tolower(),
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
      # StructureSize2 = dplyr::recode(StructureSize2 %>% tolower(),
      #                         "vf" = "Very fine",
      #                         "vn" = "Very thin",
      #                         "f"  = "Fine",
      #                         "tn" = "Thin",
      #                         "m"  = "Medium",
      #                         "co" = "Coarse",
      #                         "tk" = "Thick",
      #                         "vc" = "Very coarse",
      #                         "vk" = "Very thick",
      #                         "ec" = "Extremely coarse"),
      StructureType = dplyr::recode(StructureType %>% tolower(),
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
      # StructureType2 = dplyr::recode(StructureType2 %>% tolower(),
      #                         "gr"  = "Granular",
      #                         "abk" = "Angular blocky",
      #                         "sbk" = "Subangular blocky",
      #                         "pl"  = "Platy",
      #                         "weg" = "Wedge",
      #                         "pr"  = "Prismatic",
      #                         "col" = "Columnar",
      #                         "sg"  = "Single grain",
      #                         "ma"  = "Massive",
      #                         "cdy" = "Cloddy",
      #                         "other" = "Other"),
    ) %>%
    ### complex mutates that depend on >1 var ###
    dplyr::mutate(
      SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
      FragVolGravel = dplyr::case_when(
        Fragment1Type %in% c("GR", "Gravel", "1") ~ Fragment1VolPct,
        Fragment2Type %in% c("GR", "Gravel", "1") ~ Fragment2VolPct,
        Fragment3Type %in% c("GR", "Gravel", "1") ~ Fragment3VolPct
      ),
      FragVolCobble = dplyr::case_when(
        Fragment1Type %in% c("CB", "Cobble", "2") ~ Fragment1VolPct,
        Fragment2Type %in% c("CB", "Cobble", "2") ~ Fragment2VolPct,
        Fragment3Type %in% c("CB", "Cobble", "2") ~ Fragment3VolPct
      ),
      FragVolStone = dplyr::case_when(
        Fragment1Type %in% c("ST", "Stone", "6") ~ Fragment1VolPct,
        Fragment2Type %in% c("ST", "Stone", "6") ~ Fragment2VolPct,
        Fragment3Type %in% c("ST", "Stone", "6") ~ Fragment3VolPct
      ),
      FragVolNodule = dplyr::case_when(
        Fragment1Type %in% c("8", "Nodule") ~ Fragment1VolPct,
        Fragment2Type %in% c("8", "Nodule") ~ Fragment2VolPct,
        Fragment3Type %in% c("8", "Nodule") ~ Fragment3VolPct
      ),
      FragVolDurinode = dplyr::case_when(
        Fragment1Type %in% c("9", "Durinode") ~ Fragment1VolPct,
        Fragment2Type %in% c("9", "Durinode") ~ Fragment2VolPct,
        Fragment3Type %in% c("9", "Durinode") ~ Fragment3VolPct
      ),
      HorizonDepthLower = dplyr::case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
      HorizonDepthUpper = dplyr::case_when(
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
    )   %>%
    dplyr::arrange(PrimaryKey, HorizonDepthUpper) %>%
    dplyr::group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
      PrimaryKey
    ) %>%

    dplyr::mutate(HorizonNumber = as.character(dplyr::row_number()),
                  # across(c(RockFragments), ~ suppressWarnings(as.integer(.x))),
                  across(c(pH,
                           EC, ClayPct, SandPct, SiltPct, # poreqty,
                           FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule,
                           FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
                  ), ~ suppressWarnings(as.double(.x))),
                  # across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
    )
  horizons <- as.data.frame(horizons)

  return(horizons)
}


#' @export gather_soil_horizon
#' @rdname gather_soil_horizon
gather_soil_horizon <- function(dsn = NULL,
                                source,
                                SOILHORIZON = NULL,
                                tblSoilPitHorizons = NULL) {

  if(toupper(source) %in% c("AIM", "TERRADAT")) {
    soil <- gather_soil_horizon_terradat(dsn = dsn, tblSoilPitHorizons = tblSoilPitHorizons)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    soil <- gather_soil_horizon_lmf(dsn = dsn, SOILHORIZON = SOILHORIZON)
  } else {
    stop("source must be AIM, TerraDat, DIMA, LMF, or NRI (all case independent)")
  }

  soil$source <- source

  if("sf" %in% class(soil)) soil <- sf::st_drop_geometry(soil)

  if (any(class(soil) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(soil)[do.call(rbind, vapply(soil,
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    soil <- dplyr::mutate_at(soil, dplyr::vars(change_vars),
                            dplyr::funs(as.character))
  }

  # change from tibble to data frame
  soil <- as.data.frame(soil) %>%

  # reorder so that primary key is leftmost column
    dplyr::select(PrimaryKey, DBKey, tidyselect::everything())

  return(soil)
}
