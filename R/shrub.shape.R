#' Sagebrush Shape
#' @description Describe the shape characteristics of sagebrush on the plot,
#' including number of sagebrush hits in each shape type and the predominant shape.
#' @param lpi.tall
#'

#' @export sagebrush.shape
#' @rdname sagebrush.shape
sagebrush.shape <- function(lpi.tall) {
  shrub.shape <- lpi.tall %>%
    # Get the Sagebrush hits
    subset(GRSG_Group %in% "Sagebrush" & !is.na(ShrubShape)) %>%
    # condense to the unique lpi hits
    dplyr::select(PrimaryKey, RecKey, PointLoc, ShrubShape, chckbox) %>%
    dplyr::distinct()

  # Summarize by all ShrubShape observations
  shrub.shape.all <- shrub.shape %>%
    # Count the number of its for each ShrubShape type (C or M)
    dplyr::count(PrimaryKey, ShrubShape) %>%
    # Determine which ShrubShape is predominant on the plot (e.g., the max occurrences)
    dplyr::mutate(SagebrushShape_All_Predominant = dplyr::group_by(., PrimaryKey) %>%
      summarise(SagebrushShape_All_Predominant = ShrubShape[which.max(n)]) %>%
      dplyr::select(SagebrushShape_All_Predominant) %>%
      unlist()) %>%
    tidyr::spread(key = ShrubShape, value = n)

  names(shrub.shape.all) <- names(shrub.shape.all) %>%
    stringr::str_replace_all(c(
      "\\bC\\b" = "SagebrushShape_All_Column_Count",
      "\\bS\\b" = "SagebrushShape_All_Spread_Count"
    ))


  # Summarize by all Live ShrubShape observations
  shrub.shape.live <- shrub.shape %>%
    subset(chckbox == 0) %>% # indicates live hits
    # Count the number of its for each ShrubShape type (C or M)
    dplyr::count(PrimaryKey, ShrubShape) %>%
    # Determine which ShrubShape is predominant on the plot (e.g., the max occurrences)
    dplyr::mutate(SagebrushShape_Live_Predominant = dplyr::group_by(., PrimaryKey) %>%
      summarise(SagebrushShape_Live_Predominant = ShrubShape[which.max(n)]) %>%
      dplyr::select(SagebrushShape_Live_Predominant) %>%
      unlist()) %>%
    tidyr::spread(key = ShrubShape, value = n)

  names(shrub.shape.live) <- names(shrub.shape.live) %>%
    stringr::str_replace_all(c(
      "\\bC\\b" = "SagebrushShape_Live_Column_Count",
      "\\bS\\b" = "SagebrushShape_All_Spread_Count"
    ))


  # Join all and live shrub shapes
  sagebrush.shape <- dplyr::full_join(shrub.shape.all, shrub.shape.live)

  return(sagebrush.shape)
}
