#' Sagebrush Shape
#' @description Describe the shape characteristics of sagebrush on the plot,
#' including number of sagebrush hits in each shape type and
#' the predominant shape_
#' @param lpi_tall Dataframe. Gathered LPI dataframe in a tall format.
#' @param live Logical. Indicate if you want to distinguish live vs dead shrub
#'  shapes.Default is \code{TRUE}
#' @return Data frame of count and predominants of different sagebrush shapes


#' @export sagebrush_shape_base
#' @rdname sagebrush_shape
#'
sagebrush_shape_base <- function(lpi_tall) {
  shrub_shape <- lpi_tall %>%
    # Get the Sagebrush hits
    subset(SG_Group %in% "Sagebrush" & !is.na(ShrubShape)) %>%
    # condense to the unique lpi hits
    dplyr::select(dplyr::one_of(c(
      "PrimaryKey",
      "PointNbr",
      "ShrubShape",
      "chckbox"
    ))) %>%
    dplyr::distinct()

  # Summarize by all ShrubShape observations
  shrub_shape_predominant <- shrub_shape %>%
    # Count the number of its for each ShrubShape type (C or M)
    dplyr::count(PrimaryKey, ShrubShape) %>%
    # pivot wider so each shape type is a column
    tidyr::pivot_wider(names_from = ShrubShape,
                       values_from = n,
                       values_fill = list(n =0)
                        ) %>%
    dplyr::mutate(C = if ("C" %in% names(.)){C}else{0},
                  S = if ("S" %in% names(.)){S}else{0}) %>%
    # Determine which ShrubShape is predominant on the plot
    # (e_g_, the max occurrences)
    dplyr::mutate(SagebrushShape_All_Predominant = dplyr::case_when(C > S ~ "C",
                                                                    C < S ~ "S",
                                                                    C == S ~ "CS")) %>%
    # Rename fields
    dplyr::select(SagebrushShape_All_Column_Count = C,
                  SagebrushShape_All_Spread_Count = S,
                  SagebrushShape_All_Predominant,
                  PrimaryKey)



  # Rename for indicator tables
  if (nrow(shrub_shape_predominant) == 0) {
    sagebrush_shape_all <- shrub_shape_predominant %>%
      dplyr::mutate(
        SagebrushShape_All_Column_Count = NA,
        SagebrushShape_All_Spread_Count = NA
      )
  }

  # # Format columnar
  # if ("C" %in% lpi_tall$ShrubShape) {
  #   # Spread
  #   shrub_shape_column <- shrub_shape_count %>%
  #     # Filter by C
  #     dplyr::filter(ShrubShape == "C") %>%
  #
  #     # Rename
  #     dplyr::rename("SagebrushShape_All_ColumnCount" = n)
  #
  #   sagebrush_shape_all <- dplyr::full_join(shrub_shape_predominant,
  #     shrub_shape_column,
  #     by = "PrimaryKey"
  #   )
  # }
  # # Format spreading
  # if ("S" %in% lpi_tall$ShrubShape) {
  #   # Spread
  #   shrub_shape_spread <- shrub_shape_count %>%
  #     # Filter by C
  #     dplyr::filter(ShrubShape == "S") %>%
  #
  #     # Rename
  #     dplyr::rename("SagebrushShape_All_SpreadCount" = n)
  #
  #   # Join to rest of indicators
  #   if ("C" %in% lpi_tall$ShrubShape) {
  #     sagebrush_shape_all <- sagebrush_shape_all %>%
  #       dplyr::full_join(shrub_shape_spread, by = "PrimaryKey")
  #   } else {
  #     sagebrush_shape_all <- shrub_shape_predominant %>%
  #       dplyr::full_join(shrub_shape_spread, by = "PrimaryKey")
  #   }
  # }

  # # Clean up fields
  # sagebrush_shape_all <- sagebrush_shape_all %>%
  #   dplyr::select(-dplyr::matches("ShrubShape|^n$"))


  return(shrub_shape_predominant)
}


#' @export sagebrush_shape
#' @rdname sagebrush_shape

sagebrush_shape <- function(lpi_tall, live = TRUE) {
  shape_all <- sagebrush_shape_base(lpi_tall = lpi_tall)

  if (live) {
    shape_live <- sagebrush_shape_base(lpi_tall = subset(lpi_tall,
                                                         chckbox == 0))

    # rename the fields with "All" to "Live"

    colnames(shape_live) <- colnames(shape_live) %>% gsub(
      pattern = "All",
      replacement = "Live"
    )

    # Join with shape_all
    shape_all <- dplyr::full_join(shape_all, shape_live, by = "PrimaryKey")
  }

  return(shape_all)
}
