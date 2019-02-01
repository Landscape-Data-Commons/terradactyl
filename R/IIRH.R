### Interpreting Indicators of Rangeland Health ###
#' @title Rangeland Health
#' @param dsn String. Data source name to the geodatabase or text file
#' containing  interpreting indicators of rangeland health data.
#' @param file.type String. Specifies whether the dsn is a text file \code{"txt"}
#' or a geodatabase \code{"gdb"}
#' @param source String. Specifies the original data source layer.
#' \code{"LMF", "AIM", "TERRADAT", "NRI"} are all valid.
#'

#' @export gather_rangeland_health_terradat
#' @rdname IIRH
gather_rangeland_health_terradat <- function(dsn) {
  # check file
  if (!file.exists(dsn)) {
    stop("dsn must be a valid filepath")
  }

  # Read in tblQualHeader
  IIRH_header <- sf::st_read(dsn, layer = "tblQualHeader")

  # Read in tblQualDetail
  IIRH_detail <- sf::st_read(dsn, layer = "tblQualDetail")

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
    RH_CommentsSS = CommentSSS
  ) %>%

    # Add the indicators
    dplyr::left_join(rangeland_health_indicators)

  return(IIRH)
}


#' @export gather_rangeland_health_lmf
#' @rdname IIRH
gather_rangeland_health_lmf <- function(dsn, file.type = "gdb") {
  if (!file.exists(dsn)) {
    stop("dsn must be a valid filepath to a geodatabase containing  RHSUMMARY")
  }

  # Read in the data as .txt or .gdb
  IIRH <- switch(file.type,
    "gdb" = {
      suppressWarnings(sf::st_read(dsn,
        layer = "RANGEHEALTH",
        stringsAsFactors = FALSE
      ))
    },
    "txt" = {
      read.table(paste(dsn, "rangehealth.txt", sep = ""),
        stringsAsFactors = FALSE,
        header = FALSE,
        sep = "|",
        strip.white = TRUE
      )
    }
  )

  # if it is in a text file, there are no field names assigned.
  if (file.type == "txt") {

    IIRH <- name_variables_nri(data = IIRH,
                                     table_name = "RHSUMMARY")

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
      RH_HydrologicFunction = "HYDROLOGIC_FUNCTION"
    )

  return(IIRH_clean)
}

#' @export gather_rangeland_health
#' @rdname IIRH
#'
gather_rangeland_health <- function(dsn, source, file.type = "gdb") {

  # Check for a valid layer type
  try(if (!toupper(layer) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
  } )

  # Based on the layer type, use the appropriate gather function
  IIRH <- switch(toupper(source),
    "AIM" = gather_rangeland_health_terradat(dsn = dsn),
    "TERRADAT" = gather_rangeland_health_terradat(dsn = dsn),
    "DIMA" = gather_rangeland_health_terradat(dsn = dsn),
    "LMF" = gather_rangeland_health_lmf(
      dsn = dsn,
      file.type = file.type
    ),
    "NRI" = gather_rangeland_health_lmf(
      dsn = dsn,
      file.type = file.type
    )
  )

  return(IIRH)
}
