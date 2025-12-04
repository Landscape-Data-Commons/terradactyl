
#' Load demo data for terradactyl examples
#' @description The package terradactyl contains example data sets that can be
#'   used for testing or to check formats. This function loads objects directly
#'   into the environment and does not work with an assignment operator like
#'   \code{<-}.
#'
#'   The example data include indicators (the output from
#'   \code{build_indicators()}) and the long/tall-format data produced by the
#'   various gather functions, e.g. \code{gather_lpi()}.
#'
#'
#' @param type Character string or vector of character strings. This determines
#'   which data set(s) to load into the environment. Valid values are
#'   \code{"lpi"}, \code{"gap"}, \code{"height"}, \code{"generic_species"},
#'   \code{"lpi_species"}, \code{"rangeland_health"},
#'   \code{"species_inventory"}, \code{"soil_stability"}, \code{"species_list"},
#'   \code{"generic_species"}, and \code{"header"}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#'
#' @returns Does not return anything to be assigned. Loads data frames directly into
#'   the workspace. All loaded data frames are named with the suffix "_sample".
#' @export
#'
#' @examples
#' # Loading example Terrestrial AIM indicators produced by build_indicators()
#' demo_data(type = "indicators")
#'
#' # Loading example long/tall-format line-point intercept data produced by
#' # gather_lpi()
#' demo_data(type = "lpi")
#'
#' # Loading example gap data produced by gather_gap()
#' demo_data(type = "gap")
demo_data <- function(type,
                      verbose = FALSE){

  file_lookup <- c(indicators = "indicators_sample.rda",
                   species_list = "species_list_sample.rda",
                   gap = "tall_gap_sample.rda",
                   header = "tall_header_sample.rda",
                   height = "tall_height_sample.rda",
                   lpi = "tall_lpi_sample.rda",
                   lpi_species = "tall_lpi_species_sample.rda",
                   rangeland_health = "tall_rangeland_health_sample.rda",
                   soil_stability = "tall_soil_stability_sample.rda",
                   species_inventory = "tall_species_inventory_sample.rda")

  if (!is.character(type)) {
    stop(paste0("type must be a character string or vector of character strings. Valid values include: '",
                paste(names(file_lookup),
                      collapse = "', '"),
                "'"))
  }

  bad_types <- setdiff(x = type,
                       y = names(file_lookup))
  if (length(bad_types) > 0) {
    stop(paste0("The following type values are unrecognized: '",
                paste(bad_types,
                      collapse = "', '"),
                "'"))
  }

  for (current_type in type) {
    load(file = file.path("data",
                          file_lookup[current_type]),
         # So that this loads into the global environment
         envir = .GlobalEnv,
         verbose = verbose)
  }

}

# #' Terradactyl indicator data
# #' @name indicators_sample
# #' @docType data
# #' @keywords data indicator
# #' @description Sample of indicator data produced by the build_indicators function, using the sample tall tables included in this package.
# NULL
#
# #' Tall gap data
# #' @name tall_gap_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall gap data produced by the gather_gap function.
# NULL
#
# #' Tall header data
# #' @name tall_header_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall header data produced by the gather_header function.
# NULL
#
# #' Tall vegetation height data
# #' @name tall_height_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall gap data produced by the gather_height function.
# NULL
#
# #' Tall line-point intercept data
# #' @name tall_lpi_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall gap data produced by the gather_lpi function.
# NULL
#
# #' Tall rangeland health data
# #' @name tall_rangeland_health_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall rangeland health data produced by the gather_rangeland_health function.
# NULL
#
# #' Tall soil stability data
# #' @name tall_soil_stability_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall soil stability data produced by the gather_soil_stability function.
# NULL
#
# #' Tall species inventory data
# #' @name tall_species_inventory_sample
# #' @docType data
# #' @keywords data tall
# #' @description Sample of tall species inventory data produced by the gather_species_inventory function.
# NULL
