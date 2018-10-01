### Rangeland Health###
#' @title Rangeland Health
#' @param dsn
#'

#' @export gather.rangeland.health
#' @rdname IIRH
gather.rangeland.health <- function(dsn) {
  IIRH.header <- sf::st_read(dsn, layer = "tblQualHeader")
  IIRH.detail <- sf::st_read(dsn, layer = "tblQualDetail")


  # Clean up the Indicators Table
  rangeland.health.indicators <- IIRH.detail %>%
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
          "\\b13\\b" = "RH_PlantMortalityDec", #
          "\\b14\\b" = "RH_LitterAmount", #
          "\\b15\\b" = "RH_AnnualProd", #
          "\\b16\\b" = "RH_InvasivePlants", #
          "\\b17\\b" = "RH_ReprodCapbailityPeren"
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
  IIRH <- dplyr::select(IIRH.header, DBKey, PrimaryKey, RecKey, DateLoadedInDb,
    RH_HydrologicFunction = HFVxWRatingFinal,
    RH_BioticIntegrity = BIVxWRatingFinal,
    RH_SoilSiteStability = SSSVxWRatingFinal,
    RH_CommentsBI = CommentBI,
    RH_CommentsHF = CommentHF,
    RH_CommentsSS = CommentSSS
  ) %>%

    # Add the indicators
    dplyr::left_join(rangeland.health.indicators)

  return(IIRH)
}
