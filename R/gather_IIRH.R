#' Convert Interpreting Indicators of Rangeland Health (IIRH) data into a tall,
#' tidy data frame
#'
#' @description Given wide format IIRH data, create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblQualHeader and
#' tblQualDetail (AIM/DIMA/TerrADat) or RANGEHEALTH (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblQualHeader Dataframe of the data structure tblQualHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblQualDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblQualDetail Dataframe of the data structure tblQualDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblQualHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param RANGEHEALTH Dataframe of the data structure RANGEHEALTH from the
#' LMF/NRI database. Use when data source if LMF or NRI; alternately provide
#' dsn.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if RANGEHEALT is provided.
#' @importFrom magrittr %>%
#' @name gather_rangeland_health
#' @family <gather>
#' @return A tall data frame containing the data from the rangeland health
#' measurements.
#' @examples
#' gather_IIRH(dsn = "Path/To/AIM_Geodatabase.gdb",
#'             source = "AIM")
#' gather_IIRH(dsn = "Path/To/LMF_Geodatabase.gdb",
#'             source = "LMF")
#'
#' aim_rhdetail <- read.csv("Path/To/tblQualDetail.csv")
#' aim_rhheader <- read.csv("Path/To/tblQualHeader.csv")
#' gather_IIRH(source = "AIM",
#'             tblQualDetail = aim_rhdetail,
#'             tblQualHeader = aim_rhheader)
#'
#' lmf_rh <- read.csv("Path/To/RANGEHEALTH.csv")
#' gather_IIRH(source = "LMF",
#'             RANGEHEALTH = lmf_rh)

#' @export gather_rangeland_health_terradat
#' @rdname IIRH
gather_rangeland_health_terradat <- function(dsn = NULL,
                                             tblQualHeader = NULL,
                                             tblQualDetail = NULL) {

  if(!is.null(tblQualHeader) & !is.null(tblQualDetail)){
    IIRH_header <- tblQualHeader
    IIRH_detail <- tblQualDetail
  } else if(!is.null(dsn)) {
    # check file
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblQualDetail and tblQualHeader")
    }

    # Read in tblQualHeader
    IIRH_header <- suppressWarnings(sf::st_read(dsn, layer = "tblQualHeader", stringsAsFactors = FALSE, quiet = T))

    # Read in tblQualDetail
    IIRH_detail <- suppressWarnings(sf::st_read(dsn, layer = "tblQualDetail", quiet = T))

  } else {
    stop("Provide either tblQualHeader and tblQualDetail or a path to a geodatabase containing those tables")
  }

  # Clean up the Indicators Table
  rangeland_health_indicators <- IIRH_detail %>%
    dplyr::mutate(
      indicator = Seq %>%
        as.character() %>%
        # Rename Seq from a number to an Indicator name
        stringr::str_replace_all(c(
          "\\b1\\b" = "RH_Rills",
          "\\b2\\b" = "RH_WaterFlowPatterns",
          "\\b3\\b" = "RH_PedestalsTerracettes",
          "\\b4\\b" = "RH_BareGround",
          "\\b5\\b" = "RH_Gullies",
          "\\b6\\b" = "RH_WindScouredAreas", #
          "\\b7\\b" = "RH_LitterMovement", #
          "\\b8\\b" = "RH_SoilSurfResisErosion", #
          "\\b9\\b" = "RH_SoilSurfLossDeg", #
          "\\b10\\b" = "RH_PlantCommunityComp", #
          "\\b11\\b" = "RH_Compaction", #
          "\\b12\\b" = "RH_FuncSructGroup", #
          "\\b13\\b" = "RH_DeadDyingPlantParts", #
          "\\b14\\b" = "RH_LitterAmount", #
          "\\b15\\b" = "RH_AnnualProd", #
          "\\b16\\b" = "RH_InvasivePlants", #
          "\\b17\\b" = "RH_ReprodCapabilityPeren"
        )),
      Rating = Rating %>%
        as.character() %>%
        stringr::str_replace_all(c(
          "1" = "NS",
          "2" = "SM",
          "3" = "M",
          "4" = "ME",
          "5" = "ET",
          "0" = NA
        ))
    ) %>%
    subset(!is.na(Rating)) %>%
    dplyr::select(RecKey, indicator, Rating) %>%
    dplyr::distinct() %>%
    tidyr::spread(key = indicator, value = Rating)

  # Attributes and then joined to Indicators
  IIRH <- dplyr::select(IIRH_header, DBKey, PrimaryKey, RecKey, DateLoadedInDb,
                        RH_HydrologicFunction = HFVxWRatingFinal,
                        RH_BioticIntegrity = BIVxWRatingFinal,
                        RH_SoilSiteStability = SSSVxWRatingFinal,
                        RH_CommentsBI = CommentBI,
                        RH_CommentsHF = CommentHF,
                        RH_CommentsSS = CommentSSS#,
                        # Observer,
                        # Recorder
  ) %>%

    # Add the indicators
    dplyr::left_join(rangeland_health_indicators, by = "RecKey")

  ## last drop
  IIRH <- IIRH %>% dplyr::select(
    -c(DateLoadedInDb)
  )

  return(IIRH)
}

#' @export gather_rangeland_health_lmf
#' @rdname IIRH
gather_rangeland_health_lmf <- function(dsn = NULL,
                                        file_type = NULL,
                                        RANGEHEALTH = NULL) {

  if(!is.null(RANGEHEALTH)){
    IIRH <- RANGEHEALTH
  } else if(!is.null(dsn)){


    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing RHSUMMARY or the filepath to a text file containing RHSUMMARY")
    }

    # if file type is NULL, define it by checking the extension of dsn
    if(is.null(file_type)){
      extension <- substr(dsn, nchar(dsn)-2, nchar(dsn))
      if(extension == "csv") {
        file_type <- "csv"
      } else if(extension == "gdb") {
        file_type <- "gdb"
      } else {
        file_type <- "txt"
      }
    }

    # Read in the data as .txt or .gdb
    IIRH <- switch(file_type,
                   "gdb" = {
                     suppressWarnings(sf::st_read(dsn,
                                                  layer = "RANGEHEALTH",
                                                  stringsAsFactors = FALSE, quiet = T
                     ))
                   },
                   "txt" = {
                     read.table(paste(dsn, "rangehealth.txt", sep = ""),
                                stringsAsFactors = FALSE,
                                header = FALSE,
                                sep = "|",
                                strip.white = TRUE
                     )
                   },
                   "csv" = {
                     read.csv(dsn)
                   }
    )

    # if it is in a text file, there are no field names assigned.
    if (file_type == "txt") {
      IIRH <- name_variables_nri(
        data = IIRH,
        table_name = "RHSUMMARY"
      )
    }
  } else {
    stop("Provide RANGEHEALTH or a path to a geodatabase containing that table")
  }

  # Clean up the field names so they are human readable and match TerrAdat names
  IIRH_clean <- IIRH %>%
    dplyr::select(PrimaryKey, DBKey,
                  RH_Rills = "RILLS",
                  RH_WaterFlowPatterns = "WATER_FLOW_PATTERNS",
                  RH_PedestalsTerracettes = "PEDESTALS_TERRACETTES",
                  RH_BareGround = "BARE_GROUND",
                  RH_Gullies = "GULLIES",
                  RH_WindScouredAreas = "WIND_SCOURED_AREAS",
                  RH_LitterMovement = "LITTER_MOVEMENT",
                  RH_SoilSurfResisErosion = "SOIL_SURF_RESIS_EROSION",
                  RH_SoilSurfLossDeg = "SOIL_SURFACE_LOSS_DEG",
                  RH_PlantCommunityComp = "INFILTRATION_RUNOFF",
                  RH_Compaction = "COMPACTION_LAYER",
                  RH_FuncSructGroup = "FUNC_STRUCT_GROUPS",
                  RH_DeadDyingPlantParts = "PLANT_MORTALITY_DEC",
                  RH_LitterAmount = "LITTER_AMOUNT",
                  RH_AnnualProd = "ANNUAL_PRODUCTION",
                  RH_InvasivePlants = "INVASIVE_PLANTS",
                  RH_ReprodCapabilityPeren = "REPROD_CAPABILITY_PEREN",
                  RH_SoilSiteStability = "SOILSITE_STABILITY",
                  RH_BioticIntegrity = "BIOTIC_INTEGRITY",
                  RH_HydrologicFunction = "HYDROLOGIC_FUNCTION",
    )

  return(IIRH_clean)
}

#' export gather_rangeland_health_survey123
#' rdname IIRH
# gather_rangeland_health_survey123 <- function(PlotObservation_0 = NULL) {
#
#   # Clean up the Indicators Table
#   rangeland_health_indicators <- PlotObservation_0 %>%
#     dplyr::select(PrimaryKey = PlotKey,
#                   FormDate,
#                   RH_Rills = Rills,
#                   RH_Gullies = Gullies,
#                   RH_PedestalsTerracettes = Pedestals,
#                   RH_WindScouredAreas = Deposition)
#     dplyr::mutate(
#       indicator = Seq %>%
#         as.character() %>%
#         # Rename Seq from a number to an Indicator name
#         stringr::str_replace_all(c(
#           "\\b1\\b" = "RH_Rills",
#           "\\b2\\b" = "RH_WaterFlowPatterns",
#           "\\b3\\b" = "RH_PedestalsTerracettes",
#           "\\b4\\b" = "RH_BareGround",
#           "\\b5\\b" = "RH_Gullies",
#           "\\b6\\b" = "RH_WindScouredAreas", #
#           "\\b7\\b" = "RH_LitterMovement", #
#           "\\b8\\b" = "RH_SoilSurfResisErosion", #
#           "\\b9\\b" = "RH_SoilSurfLossDeg", #
#           "\\b10\\b" = "RH_PlantCommunityComp", #
#           "\\b11\\b" = "RH_Compaction", #
#           "\\b12\\b" = "RH_FuncSructGroup", #
#           "\\b13\\b" = "RH_DeadDyingPlantParts", #
#           "\\b14\\b" = "RH_LitterAmount", #
#           "\\b15\\b" = "RH_AnnualProd", #
#           "\\b16\\b" = "RH_InvasivePlants", #
#           "\\b17\\b" = "RH_ReprodCapabilityPeren"
#         )),
#       Rating = Rating %>%
#         as.character() %>%
#         stringr::str_replace_all(c(
#           "1" = "NS",
#           "2" = "SM",
#           "3" = "M",
#           "4" = "ME",
#           "5" = "ET",
#           "0" = NA
#         ))
#     ) %>%
#     subset(!is.na(Rating)) %>%
#     dplyr::select(RecKey, indicator, Rating) %>%
#     dplyr::distinct() %>%
#     tidyr::spread(key = indicator, value = Rating)
#
#   # Attributes and then joined to Indicators
#   IIRH <- dplyr::select(IIRH_header, DBKey, PrimaryKey, RecKey, DateLoadedInDb,
#                         RH_HydrologicFunction = HFVxWRatingFinal,
#                         RH_BioticIntegrity = BIVxWRatingFinal,
#                         RH_SoilSiteStability = SSSVxWRatingFinal,
#                         RH_CommentsBI = CommentBI,
#                         RH_CommentsHF = CommentHF,
#                         RH_CommentsSS = CommentSSS#,
#                         # Observer,
#                         # Recorder
#   ) %>%
#
#     # Add the indicators
#     dplyr::left_join(rangeland_health_indicators, by = "RecKey")
#
#   ## last drop
#   IIRH <- IIRH %>% dplyr::select(
#     -c(DateLoadedInDb)
#   )
#
#   return(IIRH)
# }




#' @export gather_rangeland_health
#' @rdname IIRH
#'
gather_rangeland_health <- function(dsn = NULL,
                                    source,
                                    file_type = NULL,
                                    tblQualHeader = NULL,
                                    tblQualDetail = NULL,
                                    RANGEHEALTH = NULL) {


  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    IIRH <- gather_rangeland_health_terradat(dsn = dsn,
                                             tblQualDetail = tblQualDetail,
                                             tblQualHeader = tblQualHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    IIRH <- gather_rangeland_health_lmf(dsn = dsn,
                                        file_type = file_type,
                                        RANGEHEALTH = RANGEHEALTH)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # IIRH$source <- toupper(source)
  if(nrow(IIRH) > 0) IIRH$source <- source

  if("sf" %in% class(IIRH)) IIRH <- sf::st_drop_geometry(IIRH)

  if (any(class(IIRH) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(IIRH)[do.call(rbind, vapply(IIRH,
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    IIRH <- dplyr::mutate_at(IIRH, dplyr::vars(change_vars),
                            dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  IIRH <- IIRH %>%
    dplyr::select(PrimaryKey, DBKey, tidyselect::everything())

  return(IIRH)
}
