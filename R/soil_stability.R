# mode <- function(x){
#   counts <- table(x)
#   counts <- counts[order(counts,
#                          decreasing = TRUE)]
#   as.numeric(names(counts)[1])
# }

#' Soil Stability Indicator Calculations
#' @param soil_stability_tall Dataframe Gathered soil stability data
#' @param all Logical. When \code{TRUE}, an indicator representing mean soil stability for all samples regardless of cover type will be calculated. If \code{tall = TRUE} these indicator values will be associated with \code{"all"} in the Veg variable. If \code{tall = FALSE} they will be found in the variable called SoilStability_All. Defaults to \code{TRUE}.
#' @param cover Logical. When \code{TRUE}, an indicator representing mean soil stability for all samples associated with perennial plant cover (defined as records which DO NOT have \code{"NC"} in the Veg variable) will be calculated. If \code{tall = TRUE} these indicator values will be associated with \code{"covered"} in the Veg variable. If \code{tall = FALSE} they will be found in the variable called SoilStability_Protected. Defaults to \code{TRUE}.
#' @param uncovered. Logical. When \code{TRUE}, an indicator representing mean soil stability for all samples not associated with perennial plant cover (defined as records which have \code{"NC"} in the Veg variable) will be calculated. If \code{tall = TRUE} these indicator values will be associated with \code{"uncovered"} in the Veg variable. If \code{tall = FALSE} they will be found in the variable called SoilStability_Unprotected. Defaults to \code{TRUE}.
#' @param all_cover_types Logical. When \code{TRUE}, indicators will be calculated for mean soil stability per cover type found in the Veg variable, e.g., \code{"SH"} or \code{"NC"}. If \code{tall = TRUE} these indicator values will be associated with corresponding code in the Veg variable. If \code{tall = FALSE} they will be found in the variable named for the associated code in Veg, e.g. SH or NC. Defaults to \code{FALSE}.
#' @param exclude_na_cover Logical. When \code{TRUE}, all records with \code{NA} in the Veg variable will be removed prior to calculations. Defaults to \code{TRUE}.
#' @param tall Logical. Indicates if output will be tall/long or wide. Defaults to \code{TRUE}.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{1} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @return Dataframe of calculated soil stability values by plot and cover type.

#' @export soil_stability
#' @rdname soil_stability
soil_stability <- function(soil_stability_tall,
                           all = TRUE,
                           cover = TRUE,
                           uncovered = TRUE,
                           all_cover_types = FALSE,
                           exclude_na_cover = TRUE,
                           tall = FALSE,
                           digits = 6,
                           verbose = FALSE) {
  #### Sanitization of inputs ##################################################
  current_indicator_type_vector <- c(all, cover, uncovered, all_cover_types)

  if (!any(current_indicator_type_vector)) {
    stop("At least one of the following arguments must be TRUE: all, cover, uncovered, all_cover_types.")
  }

  required_variables <- list(all = c("PrimaryKey",
                                     "Rating"),
                             cover = c("PrimaryKey",
                                       "Rating",
                                       "Veg"),
                             uncovered = c("PrimaryKey",
                                           "Rating",
                                           "Veg"),
                             all_cover_types = c("PrimaryKey",
                                                 "Rating",
                                                 "Veg"))

  current_required_variables <- unique(unlist(required_variables[current_indicator_type_vector]))

  if (!all(current_required_variables %in% names(soil_stability_tall))) {
    stop(paste0("The following variables are required but not present in soil_stability_tall: ",
               paste(setdiff(x = current_required_variables,
                             y = names(soil_stability_tall))),
               collapse = ", "))
  }

  # Remove records with NA Veg values if requested.
  if (exclude_na_cover) {
    soil_stability_tall <- dplyr::filter(.data = soil_stability_tall,
                                         !is.na(Veg))
  }

  # Ensure that Rating is a numeric variable and remove records with NA in the
  # Rating variable.
  soil_stability_tall <- dplyr::mutate(.data = soil_stability_tall,
                                       Rating = as.numeric(Rating))
  if (any(is.na(soil_stability_tall$Rating))) {
    warning(paste0("There are ", sum(is.na(soil_stability_tall[["Rating"]])), " records with NA or other non-numeric values in the Rating variable. ",
                   "These are being removed from the data before calculating the requested indicators."))
    soil_stability_tall <- dplyr::filter(.data = soil_stability_tall,
                                         !is.na(Rating))
  }

  for (current_required_variable in current_required_variables) {
    if (any(is.na(soil_stability_tall[[current_required_variable]]))) {
      warning(paste0("There are ", sum(is.na(soil_stability_tall[[current_required_variable]])), " records with NA as the value in the variable ", current_required_variable, ". ",
                     "These will potentially result in errors which may include these records incorrectly being excluded from or included in calculations."))
    }
  }


  #### Calculations ############################################################
  # A list to store our outputs from the various calculations.
  soil_stability_indicator_list <- list()

  # Calculate summarizing by the PrimaryKey but without differentiating between
  # cover types.
  if (all) {
    if (any(is.na(soil_stability_tall$Veg))) {
      warning(paste0("There are ", sum(is.na(soil_stability_tall$Veg)), " records with NA as the Veg value. These will be included in calculating soil stability. Please use the argument exclude_na_cover = TRUE to avoid including them if appropriate."))
    }

    soil_stability_indicator_list[["all"]] <- dplyr::summarize(.data = soil_stability_tall,
                                                               .by = c("PrimaryKey"),
                                                               # rating_mode = mode(Rating),
                                                               # rating_median = median(Rating),
                                                               rating = mean(Rating)) |>
      dplyr::mutate(.data = _,
                    Veg = "all")
  }

  # Calculate summarizing by the PrimaryKey but only for records which had a
  # vegetative cover type recorded, i.e., not "NC".
  # Records which have an NA value in the Veg variable could still be included.
  # The function removes NA values by default, so this shouldn't be an issue
  # unless the user has specifically made it one.
  if (cover) {
    if (any(is.na(soil_stability_tall$Veg))) {
      warning(paste0("There are ", sum(is.na(soil_stability_tall$Veg)), " records with NA as the Veg value. These will be included in calculating soil stability under cover. Please use the argument exclude_na_cover = TRUE to avoid including them if appropriate."))
    }

    soil_stability_indicator_list[["covered"]] <- dplyr::filter(.data = soil_stability_tall,
                                                                !(Veg %in% c("NC"))) |>
      dplyr::summarize(.data = _,
                       .by = c("PrimaryKey"),
                       # rating_mode = mode(Rating),
                       # rating_median = median(Rating),
                       rating = mean(Rating)) |>
      dplyr::mutate(.data = _,
                    Veg = "covered")
  }

  # Calculate summarizing by the PrimaryKey but only for records which have "NC"
  # (no cover) recorded in the Veg variable.
  # Records which have an NA value in the Veg variable will NOT be included.
  if (uncovered) {
    soil_stability_indicator_list[["uncovered"]] <- dplyr::filter(.data = soil_stability_tall,
                                                                  Veg %in% c("NC")) |>
      dplyr::summarize(.data = _,
                       .by = c("PrimaryKey"),
                       # rating_mode = mode(Rating),
                       # rating_median = median(Rating),
                       rating = mean(Rating)) |>
      dplyr::mutate(.data = _,
                    Veg = "uncovered")
  }

  # Calculate mean rating for all cover types individually
  if (all_cover_types) {
    soil_stability_indicator_list[["all_cover_types"]] <- dplyr::filter(.data = soil_stability_tall,
                                                                        Veg %in% c("NC")) |>
      dplyr::summarize(.data = _,
                       .by = c("PrimaryKey",
                               "Veg"),
                       # rating_mode = mode(Rating),
                       # rating_median = median(Rating),
                       rating = mean(Rating))
  }

  # Bind all the results produced into a single data frame.
  soil_stability_rating_all <- dplyr::bind_rows(soil_stability_indicator_list) |>
    dplyr::mutate(.data = _,
                  rating = round(x = rating,
                                 digits = digits))

  # If tall is FALSE, then we'll pivot the current data frame into a wide format
  if (!tall) {
    # I'd rather do the renaming in the tall version of the table, but I also
    # don't want to break legacy code that expects the output veg variable to
    # have the values produced above, so we'll make the changes here because
    # a wide table has always gotten its variables renamed in the past versions
    # of this function anyway.
    soil_stability_rating_all <- dplyr::mutate(.data = soil_stability_rating_all,
                                               Veg = dplyr::case_when(Veg == "all" ~ "SoilStability_All",
                                                                      Veg == "covered" ~ "SoilStability_Protected",
                                                                      Veg == "uncovered" ~ "SoilStability_Unprotected",
                                                                      .default = Veg)) |>
      tidyr::pivot_wider(data = _,
                         names_from = Veg,
                         values_from = rating)

  }

  return(as.data.frame(soil_stability_rating_all))
}
