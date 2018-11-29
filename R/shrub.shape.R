#' Sagebrush Shape
#' @description Describe the shape characteristics of sagebrush on the plot,including number of sagebrush hits in each shape type and the predominant shape_
#' @param lpi.tall
#' @return Data frame of count and predominants of different sagebrush shapes

#' @export sagebrush_shape_base
#' @rdname sagebrush_shape
#'
sagebrush_shape_base <- function(lpi.tall) {
  shrub_shape <- lpi.tall %>%
    # Get the Sagebrush hits
    subset(SG_Group %in% "Sagebrush" & !is.na(ShrubShape)) %>%
    # condense to the unique lpi hits
    dplyr::select(dplyr::one_of(c("PrimaryKey",
                                  "PointNbr",
                                  "ShrubShape",
                                  "chckbox"))) %>%
    dplyr::distinct()

  # Summarize by all ShrubShape observations
  shrub_shape_predominant <- shrub_shape %>%
    # Count the number of its for each ShrubShape type (C or M)
    dplyr::count(PrimaryKey, ShrubShape) %>%
    # Determine which ShrubShape is predominant on the plot
    # (e_g_, the max occurrences)
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarise(SagebrushShape_All_Predominant = ShrubShape[which.max(n)])

  # Count the number of instances in C or S
  shrub_shape_count <- shrub_shape %>% dplyr::count(PrimaryKey, ShrubShape)

  #Rename for indicator tables
  if (nrow(shrub_shape_count) == 0) {
    sagebrush_shape_all <- shrub_shape_predominant %>%
      dplyr::mutate(SagebrushShape_All_Column_Count  = NA,
                    SagebrushShape_All_Spread_Count = NA)
  }

  #Format columnar
  if ("C" %in% lpi.tall$ShrubShape) {
    # Spread
    shrub_shape_column <- shrub_shape_count %>%
      #Filter by C
      dplyr::filter(ShrubShape == "C") %>%

      # Rename
      dplyr::rename("SagebrushShape_All_Column_Count" = n)

    sagebrush_shape_all <- dplyr::full_join(shrub_shape_predominant,
                                            shrub_shape_column, by = "PrimaryKey")
  }
  # Format spreading
  if ("S" %in% lpi.tall$ShrubShape) {
    # Spread
    shrub_shape_spread <- shrub_shape_count %>%
      #Filter by C
      dplyr::filter(ShrubShape == "S") %>%

      # Rename
      dplyr::rename("SagebrushShape_All_Spread_Count" = n)

    # Join to rest of indicators
    if ("C" %in% lpi.tall$ShrubShape) {
      sagebrush_shape_all <- sagebrush_shape_all %>%
        dplyr::full_join(shrub_shape_spread, by = "PrimaryKey")
    } else {
      sagebrush_shape_all <- shrub_shape_predominant %>%
        dplyr::full_join(shrub_shape_spread, by = "PrimaryKey")
    }

  }

  # Clean up fields
  sagebrush_shape_all <- sagebrush_shape_all %>%
    dplyr::select(-dplyr::matches("ShrubShape|^n$"))


return(sagebrush_shape_all)

}


#' @export sagebrush_shape
#' @rdname sagebrush_shape

sagebrush_shape <- function(lpi.tall, live = TRUE) {
  shape_all <- sagebrush_shape_base(lpi.tall = lpi.tall)

  if (live){
    shape_live <- sagebrush_shape_base(lpi.tall = subset(lpi.tall, chckbox ==0))

    #rename the fields with "All" to "Live"

    colnames(shape_live) <- colnames(shape_live) %>% gsub(pattern = "All",
                                                          replacement = "Live")

    #Join with shape_all
    shape_all <- dplyr::full_join(shape_all, shape_live, by = "PrimaryKey")
  }

  return(shape_all)

}


