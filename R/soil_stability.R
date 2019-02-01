#' Soil Stability Indicator Calculations
#' @param soil_stability_tall Dataframe Gathered soil stability data
#' @param all Logical. When \code{TRUE} Calculate soil stability for all samples. Defaults to \code{TRUE}
#' @param cover Logical. When \code{TRUE}, calculates soil stability for samples covered byperennial vegetation. Defaults to \code{TRUE}
#' @param uncovered. Logical. When \code{TRUE}, calculates soil stability for samples not covered by perennial vegetation. Defaults to \code{TRUE}
#' @param all_cover_types Logical. When \code{TRUE}, calculates soil stability for each indidual cover type. Defaults to \code{FALSE}
#' @param tall Logical. Indicates if output is tall/long or wide. Defaults to \code{TRUE}
#' @return Dataframe of calculated soil stability values by plot.

#' @export soil_stability
#' @rdname soil_stability
soil_stability <- function(soil_stability_tall,
                           all = TRUE,
                           cover = TRUE,
                           uncovered = TRUE,
                           all_cover_types = FALSE,
                           tall = FALSE) {
  soil_stability_rating <- list()

  # Remove NA Rating values
  soil_stability_tall <- subset(soil_stability_tall, !is.na(Rating))

  # Calculate a mean rating for all cover types
  if (all == TRUE) {
    soil_stability_rating[["all"]] <- soil_stability_tall %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(rating = mean(Rating)) %>%
      dplyr::mutate(Veg = "all") %>%
      as.data.frame()
  }
  # Calculate mean rating for all covered soil samples
  if (cover == TRUE) {
    soil_stability_rating[["covered"]] <- soil_stability_tall %>%
      subset(Veg != "NC") %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(rating = mean(Rating)) %>%
      dplyr::mutate(Veg = "covered") %>%
      as.data.frame()
  }
  # Calculate mean rating for all uncovered soil samples
  if (uncovered == TRUE) {
    soil_stability_rating[["unconvered"]] <- soil_stability_tall %>%
      subset(Veg == "NC") %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(rating = mean(Rating)) %>%
      dplyr::mutate(Veg = "uncovered") %>%
      as.data.frame()
  }
  # Calculate mean rating for all cover types individually
  if (all_cover_types == TRUE) {
    soil_stability_rating[["all_cover_types"]] <- soil_stability_tall %>%
      dplyr::group_by(PrimaryKey, Veg) %>%
      dplyr::summarize(rating = mean(Rating)) %>%
      as.data.frame()
  }

  # merge all soil stability rating calculations
  soil_stability_rating_all <- do.call("rbind", soil_stability_rating)

  # if tall=FALSE spread into a wide format
  if (!tall) {
    soil_stability_rating_all <- soil_stability_rating_all %>%
      tidyr::spread(key = Veg, value = rating)
  }

  return(soil_stability_rating_all)
}
