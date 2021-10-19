#' RAP data model
#' Build data inputs for the Rangeland Analysis Platform.
#' @param lpi_species Dataframe in environment. An LPI tall table generated from \code{gather_lpi()} and \code{species_join()}.
#' @param header Dataframe in environment. Header table generated from \code{gather_header()}.
#' @examples
#' RAP_inputs <- terradactyl::data_model_RAP(lpi_species,
#'                                           header)
#'
data_model_RAP <- function(lpi_species,
                           header) {

  #gather co-variates
  RAP_header <- header %>%
    dplyr::select_if(names(header) %in% c("PrimaryKey",
                                          "Latitude_NAD83",
                                          "Longitude_NAD83",
                                          "LocationType",
                                          "DateVisited"))

  # calculate cover variables
  cover <- core_cover_indicators(lpi_species %>%
                                   subset(PrimaryKey %in% header$PrimaryKey))

  # join header and core cover
  RAP_data <- dplyr::left_join(RAP_header,
                               cover)


}
