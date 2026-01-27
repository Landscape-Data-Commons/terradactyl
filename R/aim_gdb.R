# Build indicators feature class
#' Calculate the full set of standard Terrestrial AIM Database indicators
#' @description
#' A wrapper function for the *_calc() family of functions that produce the default TerrADat indicators.
#'
#' @param header Data frame or character string. The data to be provided as the argument \code{header} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param dsn Character string. The filepath to the geodatabase containing data. Passed to indicator calculation functions that require the argument \code{dsn}.
#' @param species_file Data frame or character string. The data to be provided as the argument \code{species_file} to any indicator calculation functions that require it. If this is a character string, it must point to the CSV or GDB file containing the data. This should almost always be to a geodatabase containing tblNationalPlants and tblStateSpecies.
#' @param species_code_var Character string. The name of the variable in the species characteristics that contain the species codes. Defaults to \code{"SpeciesCode"}.
#' @param lpi_tall Data frame or character string. The data to be provided as the argument \code{lpi_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param gap_tall Data frame or character string. The data to be provided as the argument \code{gap_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param height_tall Data frame or character string. The data to be provided as the argument \code{height_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param spp_inventory_tall Data frame or character string. The data to be provided as the argument \code{spp_inventory_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param soil_stability_tall Data frame or character string. The data to be provided as the argument \code{soil_stability_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param ... Optional are filtering statements. These will be passed to \code{dplyr::filter()} to applied to \code{header} to restrict the calculations.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @export
#' @returns A data frame with all standard TerrADat indicators in a format matching TerrADat.
#'
build_terradat_indicators <- function(header,
                                      dsn,
                                      species_file,
                                      species_code_var = "SpeciesCode",
                                      lpi_tall = NULL,
                                      gap_tall = NULL,
                                      height_tall = NULL,
                                      spp_inventory_tall = NULL,
                                      soil_stability_tall = NULL,
                                      digits = 6,
                                      ...,
                                      verbose = FALSE) {
  #### Setup ###################################################################
  # Assign filter expressions
  filter_exprs <- rlang::quos(...)

  #### Reading #################################################################
  inputs_list <- list(header = header,
                      lpi_tall = lpi_tall,
                      gap_tall = gap_tall,
                      height_tall = height_tall,
                      spp_inventory_tall = spp_inventory_tall,
                      soil_stability_tall = soil_stability_tall)

  for (current_input_type in names(inputs_list)) {
    if (verbose) {
      message(paste0("Currently working with ",
                     current_input_type,
                     "."))
    }

    if (is.null(inputs_list[[current_input_type]])) {
      message(paste("No data provided for", current_input_type, "so indicators derived from those will not be calculated."))
    } else {
      current_data <- read_whatever(input = inputs_list[[current_input_type]],
                                    accept_failure = FALSE,
                                    verbose = verbose)
    }

    if (current_input_type == "header") {
      if (!is.data.frame(current_data)) {
        stop("Something is wrong with the current header information provided.")
      } else if (nrow(current_data) < 1) {
        stop("The header information contains no records.")
      }

      current_data <- dplyr::filter(.data = current_data,
                                    !!!filter_exprs)
      if (nrow(current_data) < 1) {
        stop("The header information contains no records after applying the filtering expressions.")
      }

    } else {
      if (is.null(current_data)) {
        current_data <- NULL
      } else if (nrow(current_data) < 1) {
        message(paste("No records found in the data provided for", current_input_type, "so indicators derived from those will not be calculated."))
        current_data <- NULL
      } else {
        if (verbose) {
          message("Restricting data to records with PrimaryKey values found in the provided headers")
        }
        current_data <- dplyr::filter(.data = current_data,
                                      PrimaryKey %in% inputs_list[["header"]]$PrimaryKey)
      }
      if (nrow(current_data) < 1) {
        message(paste("No records found in the data provided for", current_input_type, "after restricting by PrimaryKey so indicators derived from those will not be calculated."))
        current_data <- NULL
      }

    }

    inputs_list[[current_input_type]] <- current_data
  }

  #### Calculating indicators ##################################################
  indicators_list <- list()
  ##### LPI --------------------------------------------------------------------
  if (!is.null(inputs_list[["lpi_tall"]])) {
    if (verbose) {
      message("Calculating LPI indicators")
    }
    indicators_list[["lpi"]] <- lpi_calc(lpi_tall = inputs_list[["lpi_tall"]],
                                         header = inputs_list[["header"]],
                                         species_file = species_file,
                                         species_code_var = species_code_var,
                                         digits = digits,
                                         verbose = verbose)
  } else {
    if (verbose) {
      message("No LPI data provided. Skipping LPI-derived indicators")
    }
  }

  ##### Gap --------------------------------------------------------------------
  if (!is.null(inputs_list[["gap_tall"]])) {
    if (verbose) {
      message("Calculating gap indicators")
    }
    indicators_list[["gap"]] <- gap_calc(gap_tall = inputs_list[["gap_tall"]],
                                         header = inputs_list[["header"]],
                                         digits = digits,
                                         verbose = verbose)
  } else {
    if (verbose) {
      message("No gap data provided. Skipping gap indicators")
    }
  }

  ##### Height -----------------------------------------------------------------
  if (!is.null(inputs_list[["height_tall"]])) {
    if (verbose) {
      message("Calculating height indicators")
    }
    indicators_list[["height"]] <- height_calc(height_tall = inputs_list[["height_tall"]],
                                               header = inputs_list[["header"]],
                                               source = "AIM",
                                               species_file = species_file,
                                               digits = digits,
                                               verbose = verbose)
  } else {
    if (verbose) {
      message("No height data provided. Skipping height indicators")
    }
  }

  ##### Species Inventory ------------------------------------------------------
  if (!is.null(inputs_list[["spp_inventory_tall"]])) {
    if (verbose) {
      message("Calculating species inventory indicators")
    }
    indicators_list[["species"]] <- spp_inventory_calc(spp_inventory_tall = inputs_list[["spp_inventory_tall"]],
                                                       header = inputs_list[["header"]],
                                                       species_file = species_file,
                                                       source = "AIM",
                                                       # digits = digits,
                                                       verbose = verbose)
  } else {
    if (verbose) {
      message("No species inventory data provided. Skipping inventory-derived indicators")
    }
  }

  ##### Soil Stability ---------------------------------------------------------
  if (!is.null(inputs_list[["soil_stability_tall"]])) {
    if (verbose) {
      message("Calculating soil stability indicators")
    }
    indicators_list[["soil_stability"]] <- soil_stability_calc(soil_stability_tall = inputs_list[["soil_stability_tall"]],
                                                               digits = digits,
                                                               verbose = verbose)
  } else {
    if (verbose) {
      message("No soil stability data provided. Skipping soil stability indicators")
    }
  }

  #### Output ##################################################################
  # Reduce the list to a data frame
  output <- purrr::reduce(.f = dplyr::left_join,
                          .x = indicators_list)

  output
}

# Build LMF Indicators
#' Calculate the full set of standard Terrestrial AIM Database indicators from LMF data
#' @description
#' A wrapper function for the *_calc() family of functions that produce the default TerrADat indicators.
#'
#' @param header Data frame or character string. The data to be provided as the argument \code{header} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param dsn Character string. The filepath to the geodatabase containing data. Passed to indicator calculation functions that require the argument \code{dsn}.
#' @param species_file Data frame or character string. The data to be provided as the argument \code{species_file} to any indicator calculation functions that require it. If this is a character string, it must point to the CSV or GDB file containing the data. This should almost always be to a geodatabase containing tblNationalPlants and tblStateSpecies.
#' @param lpi_tall Data frame or character string. The data to be provided as the argument \code{lpi_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param gap_tall Data frame or character string. The data to be provided as the argument \code{gap_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param height_tall Data frame or character string. The data to be provided as the argument \code{height_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param spp_inventory_tall Data frame or character string. The data to be provided as the argument \code{spp_inventory_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param soil_stability_tall Data frame or character string. The data to be provided as the argument \code{soil_stability_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param ... Optional are filtering statements. These will be passed to \code{dplyr::filter()} to applied to \code{header} to restrict the calculations.
#' @param generic_species_file Optional character string. Must specify the full path to a CSV containing generic species information. If this is \code{NULL}. Defaults to \code{NULL}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @export
#' @returns A data frame with all standard TerrADat indicators in a format matching TerrADat.
#'

build_lmf_indicators <- function(header,
                                 dsn,
                                 species_file,
                                 lpi_tall,
                                 gap_tall,
                                 height_tall,
                                 spp_inventory_tall,
                                 soil_stability_tall,
                                 digits = 6,
                                 ...,
                                 generic_species_file = NULL,
                                 verbose = FALSE) {




  # Assign filter expressions
  filter_exprs <- rlang::quos(...)

  #### Reading #################################################################
  inputs_list <- list(header = header,
                      lpi_tall = lpi_tall,
                      gap_tall = gap_tall,
                      height_tall = height_tall,
                      spp_inventory_tall = spp_inventory_tall,
                      soil_stability_tall = soil_stability_tall)

  for (current_input_type in names(inputs_list)) {
    if (verbose) {
      message(paste0("Currently working with ",
                     current_input_type,
                     "."))
    }

    if (is.null(inputs_list[[current_input_type]])) {
      message(paste("No data provided for", current_input_type, "so indicators derived from those will not be calculated."))
    } else {
      current_data <- read_whatever(input = inputs_list[[current_input_type]],
                                    accept_failure = FALSE,
                                    verbose = verbose)
    }

    if (current_input_type == "header") {
      if (!is.data.frame(current_data)) {
        stop("Something is wrong with the current header information provided.")
      } else if (nrow(current_data) < 1) {
        stop("The header information contains no records.")
      }

      current_data <- dplyr::filter(.data = current_data,
                                    !!!filter_exprs)
      if (nrow(current_data) < 1) {
        stop("The header information contains no records after applying the filtering expressions.")
      }

    } else {
      if (is.null(current_data)) {
        current_data <- NULL
      } else if (nrow(current_data) < 1) {
        message(paste("No records found in the data provided for", current_input_type, "so indicators derived from those will not be calculated."))
        current_data <- NULL
      } else {
        if (verbose) {
          message("Restricting data to records with PrimaryKey values found in the provided headers")
        }
        current_data <- dplyr::filter(.data = current_data,
                                      PrimaryKey %in% inputs_list[["header"]]$PrimaryKey)
      }
      if (nrow(current_data) < 1) {
        message(paste("No records found in the data provided for", current_input_type, "after restricting by PrimaryKey so indicators derived from those will not be calculated."))
        current_data <- NULL
      }

    }

    inputs_list[[current_input_type]] <- current_data
  }
  # Read header in
  header <- readRDS(header) |>
    # Filter using the filtering expression specified by user
    # dplyr::filter(.data = _,
    #               !!!filter_exprs) |>
    dplyr::filter(.data = _,
                  source %in% c("LMF", "NRI"))

  # Check header for data
  if(nrow(header) == 0){
    stop("No records present in provided header.")
  }

  # Join all indicator calculations together
  indicators <- list(header,
                     # LPI
                     lpi_calc(lpi_tall = lpi_tall,
                              header = header,
                              species_file = species_file,
                              generic_species_file = generic_species_file,
                              digits = digits),
                     # Gap
                     gap_calc(gap_tall = gap_tall,
                              header = header,
                              digits = digits),
                     #  # Height
                     height_calc(height_tall = height_tall,
                                 header = header,
                                 source = source,
                                 species_file = species_file,
                                 generic_species_file = generic_species_file,
                                 digits = digits),
                     # Species Inventory
                     spp_inventory_calc(spp_inventory_tall = spp_inventory_tall,
                                        header = header,
                                        species_file = species_file,
                                        source = source,
                                        generic_species_file = generic_species_file),
                     # Soil Stability
                     soil_stability_calc(soil_stability_tall = soil_stability_tall),
                     digits = digits)

  purrr::reduce(.f = dplyr::left_join,
                .x = indicators)
}

# Build Indicators
#' Calculate the full set of standard Terrestrial AIM Database indicators
#' @description
#' A wrapper function for the *_calc() family of functions that produce the default TerrADat indicators.
#'
#' @param header Data frame or character string. The data to be provided as the argument \code{header} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param source Character string. The expected input data format. Must be one of \code{"terradat"}, \code{"aim"}, \code{"lmf"}, or \code{"nri"}. Case insensitive.
#' @param dsn Character string. The filepath to the geodatabase containing data. Passed to indicator calculation functions that require the argument \code{dsn}.
#' @param species_file Data frame or character string. The data to be provided as the argument \code{species_file} to any indicator calculation functions that require it. If this is a character string, it must point to the CSV or GDB file containing the data. This should almost always be to a geodatabase containing tblNationalPlants and tblStateSpecies.
#' @param lpi_tall Data frame or character string. The data to be provided as the argument \code{lpi_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param gap_tall Data frame or character string. The data to be provided as the argument \code{gap_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param height_tall Data frame or character string. The data to be provided as the argument \code{height_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param spp_inventory_tall Data frame or character string. The data to be provided as the argument \code{spp_inventory_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param soil_stability_tall Data frame or character string. The data to be provided as the argument \code{soil_stability_tall} to any indicator calculation functions that require it. If this is a character string, it must point to the .Rdata file containing the data.
#' @param ... Optional are filtering statements. These will be passed to \code{dplyr::filter()} to applied to \code{header} to restrict the calculations.
#' @param generic_species_file Optional character string. Must specify the full path to a CSV containing generic species information. If this is \code{NULL}. Defaults to \code{NULL}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @export
#' @returns A data frame with all standard TerrADat indicators in a format matching TerrADat.
#'
build_indicators <- function(header, source,
                             dsn = NULL, lpi_tall,
                             species_file,
                             gap_tall,
                             height_tall,
                             spp_inventory_tall,
                             soil_stability_tall, ...,
                             generic_species_file = NULL,
                             digits = 6,
                             verbose = FALSE) {
  all_indicators <- switch(toupper(source),
                           "TERRADAT" = {
                             build_terradat_indicators(
                               dsn = dsn,
                               species_file = species_file,
                               digits = digits,
                               generic_species_file = generic_species_file)
                           },
                           "AIM" = build_terradat_indicators(
                             header = header,
                             dsn = dsn,
                             source = source,
                             lpi_tall = lpi_tall,
                             gap_tall = gap_tall,
                             height_tall = height_tall,
                             spp_inventory_tall = spp_inventory_tall,
                             soil_stability_tall = soil_stability_tall,
                             species_file = species_file,
                             digits = digits,
                             ...,
                             generic_species_file = generic_species_file
                           ),
                           "LMF" = build_lmf_indicators(
                             header = header,
                             dsn = dsn,
                             source = source,
                             lpi_tall = lpi_tall,
                             gap_tall = gap_tall,
                             height_tall = height_tall,
                             spp_inventory_tall = spp_inventory_tall,
                             soil_stability_tall = soil_stability_tall,
                             species_file = species_file,
                             digits = digits,
                             ...,
                             generic_species_file = generic_species_file
                           ),
                           "NRI" = build_lmf_indicators(
                             header = header,
                             dsn = dsn,
                             source = source,
                             lpi_tall = lpi_tall,
                             gap_tall = gap_tall,
                             height_tall = height_tall,
                             spp_inventory_tall = spp_inventory_tall,
                             soil_stability_tall = soil_stability_tall,
                             species_file = species_file,
                             digits = digits,
                             ...,
                             generic_species_file = generic_species_file
                           )
  )

  # If target feature class is a gdb compare indicator field names with the
  # names for a the target feature class
  if (!is.null(dsn)) {
    if (tools::file_ext(dsn) == "gdb") {
      if (verbose) {
        message("Reading column names from dsn. Missing columns will be added to output.")
      }
      available_layers <- sf::st_layers(dsn = dsn)[["name"]]

      layer_source <- dplyr::if_else(condition = source == "LMF",
                                     true = "LMF",
                                     false = "Terradat")

      source_layer <- available_layers[stringr::str_detect(string = available_layers,
                                                           pattern = paste0(layer_source,
                                                                            "(_)?_I_Indicators$"))]

      if (length(source_layer) < 1) {
        source_layer <- NULL
      } else if (length(source_layer) > 1) {
        source_layer <- source_layer[1]
        warning(paste0("Using first discovered applicable layer, ",
                       source_layer, "."))

      }
      # These are used for data management within a geodatabase and we're going to
      # drop them.
      internal_gdb_vars <- c("GlobalID",
                             "created_user",
                             "created_date",
                             "last_edited_user",
                             "last_edited_date",
                             "DateLoadedInDb",
                             "DateLoadedinDB",
                             "rid",
                             "DBKey",
                             "DataErrorChecking",
                             "DataEntry",
                             "DateModified",
                             "FormType",
                             "ViewOBJECTID",
                             "Shape")
      feature_class_field_names <- sf::st_read(dsn = dsn,
                                               layer = source_layer) |>
        names() |>
        setdiff(x = _,
                internal_gdb_vars)

      expected_indicator_variables <- feature_class_field_names[stringr::str_detect(string = feature_class_field_names,
                                                                                    pattern = "(Cover)|(^[FA]H)|(SoilStability)|(^Hgt)|(^Num)|(^SagebrushShape)")]
      #
      indicator_field_names <- data.frame(
        name = names(all_indicators),
        calculated = "yes"
      )

      missing_names <- data.frame(
        name = feature_class_field_names,
        feature.class = "yes"
      ) |>
        # Join feature class field names to indicator field names
        dplyr::full_join(indicator_field_names) |>

        # get the field names where there is not corollary in calculated
        subset(is.na(calculated), select = "name") |>
        dplyr::mutate(value = NA) |>
        # make into a data frame
        tidyr::spread(key = name, value = value)

      # Add a row for each PrimaryKey inall_indicators
      missing_names[nrow(all_indicators), ] <- NA
      # For some indicators, the null value is 0 (to indicate the method was completed,
      # but no data in that group were collected)
      # Skip this if the method was not provided
      if(!is.null(lpi_tall)){
        missing_names[, grepl(names(missing_names), pattern = "^FH|^AH")] <- 0
      }

      if(!is.null(spp_inventory_tall)){
        missing_names[, grepl(names(missing_names), pattern = "^Num")] <- 0
      }

      # Merge back to indicator data to create a feature class for export
      final_feature_class <- dplyr::bind_cols(all_indicators, missing_names)
      return(final_feature_class)

      if(!is.null(spp_inventory_tall)){
        missing_names[, grepl(names(missing_names), pattern = "^Num")] <- 0
      } else {
        return(all_indicators)
      }
    } else {
      return(all_indicators)
    }
  } else {
    return(all_indicators)
  }
}



# Calculate the LPI indicators
#' Calculate the standard Terrestrial AIM Database (TerrADat) Line-Point Intercept indicators
#' @description
#' This function calculates the full set of LPI-derived indicators that are standard for TerrADat.
#' These indicators are dependent on the species characteristics used by Terrestrial AIM and stored in the tblNationalPlants and tblStateSpecies tables in TerrADat. Attempting to use this function with any other format of species characteristic data will almost certainly fail.
#'
#' For any other LPI-derived indicators, use the underlying functions \code{pct_cover()} and \code{mean_height()}.
#'
#'
#' @param header Data frame or character string. The metadata for the plots involved in the calculations, this must contain the variable PrimaryKey and any of SpeciesState, State, and County. If a character string, this must point to a CSV file containing the data.
#' @param lpi_tall  Data frame or character string. The long/tall-format LPI data for the plots involved in the calculations. The format must match the output from \code{gather_lpi()}. If a character string, this must point to a CSV file containing the data.
#' @param species_file Data frame or character string. The species characteristics information. If this is a character string for the filepath to a geodatabase, that geodatabase must contain both the tblNationalPlants and tblStateSpecies tables. Otherwise, this must either be the output from \code{species_read_aim()} or be a character string pointing to a CSV file containing the output from \code{species_read_aim()}.
#' @param species_code_var Character string. The name of the variable in the species characteristics that contains the species codes. Defaults to \code{"SpeciesCode"}.
#' @param generic_species_file Optional character string. Must specify the full path to a CSV containing generic species information. If this is \code{NULL}. Defaults to \code{NULL}.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{2} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#'
#' @returns A data frame matching the format of LPI indicators in TerrADat.
#' @export
lpi_calc <- function(header = NULL,
                     lpi_tall = NULL,
                     species_file,
                     species_code_var = "SpeciesCode",
                     generic_species_file = NULL,
                     digits = 6,
                     verbose = FALSE) {

  if (!is.character(species_code_var)) {
    stop("species_code_var must be a single character string specifying the name of the variable in the species_file that contains the species codes.")
  } else if (length(species_code_var) > 1) {
    stop("species_code_var must be a single character string specifying the name of the variable in the species_file that contains the species codes.")
  }

  #### Handling header and raw data ############################################
  if ("character" %in% class(header)) {
    if (toupper(tools::file_ext(header)) == "RDATA") {
      header <- readRDS(header)
    } else {
      stop("When header is a character string it must be the path to a .Rdata file containing header data.")
    }
  } else if ("data.frame" %in% class(header)) {
    header <- header
  }
  if ("character" %in% class(lpi_tall)) {
    if (toupper(tools::file_ext(lpi_tall)) == "RDATA") {
      lpi_tall <- readRDS(file = lpi_tall)
    } else {
      stop("When lpi_tall is a character string it must be the path to a .Rdata file containing tall LPI data.")
    }
  } else if ("data.frame" %in% class(lpi_tall)) {
    lpi_tall <- lpi_tall
  }

  lpi_tall_header <- dplyr::left_join(x = dplyr::select(.data = header,
                                                        tidyselect::any_of(c("PrimaryKey",
                                                                             "SpeciesState",
                                                                             "State",
                                                                             "County"))),
                                      y = lpi_tall,
                                      relationship = "one-to-many",
                                      by = "PrimaryKey")

  #### Defaults and setup ######################################################
  ##### Expected output indicators ---------------------------------------------
  # Valid indicator names we're looking for
  # We'll use this to:
  # 1) Drop any unintended indicators
  # 2) Populate missing indicators with 0 (e.g., when there are no invasive
  #    shrubs and therefore no invasive shrub indicators calculated)
  # 3) Reorder the output of indicators in the output to meet expectations
  expected_indicator_names <- c("TotalFoliarCover",
                                "BareSoilCover",
                                "AH_ForbCover",
                                "AH_PerenForbCover",
                                "AH_AnnForbCover",
                                "AH_PreferredForbCover",
                                "AH_GrassCover",
                                "AH_GraminoidCover",
                                "AH_PerenGrassCover",
                                "AH_PerenGraminoidCover",
                                "AH_C3PerenGrassCover",
                                "AH_C4PerenGrassCover",
                                "AH_AnnGrassCover",
                                "AH_AnnGraminoidCover",
                                "AH_TallPerenGrassCover",
                                "AH_ShortPerenGrassCover",
                                "AH_PerenForbGraminoidCover",
                                "AH_AnnForbGraminoidCover",
                                "AH_ShrubCover",
                                "AH_ShrubSucculentCover",
                                "AH_SagebrushCover",
                                "AH_SagebrushCover_Live",
                                "AH_NonSagebrushShrubCover",
                                "AH_TotalLitterCover",
                                "AH_WoodyLitterCover",
                                "AH_HerbLitterCover",
                                "AH_DuffCover",
                                "AH_VagrLichenCover",
                                "AH_LichenCover",
                                "AH_MossCover",
                                "AH_CyanobacteriaCover",
                                "AH_RockCover",
                                "AH_EmbLitterCover",
                                "AH_WaterCover",
                                "AH_InvasiveCover",
                                "AH_InvasivePerenForbCover",
                                "AH_InvasiveAnnForbCover",
                                "AH_InvasivePerenGrassCover",
                                "AH_InvasiveAnnGrassCover",
                                "AH_InvasivePerenForbGrassCover",
                                "AH_InvasiveAnnForbGrassCover",
                                "AH_InvasiveShrubCover",
                                "AH_InvasiveSubShrubCover",
                                "AH_InvasiveSucculentCover",
                                "AH_InvasiveTreeCover",
                                "AH_NativeCover",
                                "AH_NonNativeCover",
                                "AH_NoxiousCover",
                                "AH_PJCover",
                                "AH_ConiferCover",
                                "AH_BasalCover",
                                "AH_BasalPerenGrassCover",
                                "AH_BiocrustCover",
                                "FH_TotalLitterCover",
                                "FH_WoodyLitterCover",
                                "FH_HerbLitterCover",
                                "FH_DuffCover",
                                "FH_VagrLichenCover",
                                "FH_LichenCover",
                                "FH_MossCover",
                                "FH_CyanobacteriaCover",
                                "FH_RockCover",
                                "FH_EmbLitterCover",
                                "FH_WaterCover",

                                # NEEDS TO BE WRITTEN FOR
                                "FH_DepSoilCover",

                                "FH_ForbCover",
                                "FH_PerenForbCover",
                                "FH_AnnForbCover",
                                "FH_GraminoidCover",
                                "FH_AnnGraminoidCover",
                                "FH_PerenGraminoidCover",
                                "FH_PerenForbGraminoidCover",
                                "FH_ShrubCover",
                                "FH_SagebrushCover",
                                "FH_NonSagebrushShrubCover",
                                "FH_TreeCover",

                                "SagebrushShape_Live_ColumnCount",
                                "SagebrushShape_Live_SpreadCount",
                                "SagebrushShape_Live_Predominant")

  ##### Indicator renaming lookup ----------------------------------------------
  # The indicators that have nonstandard names. This'll let us rename them with
  # the help of stringr::str_replace_all() later.
  nonstandard_indicator_lookup <- c("^FH_BareSoilCover$" = "BareSoilCover",
                                    "^AH_SagebrushLiveCover$" = "AH_SagebrushCover_Live",
                                    "^AH_BasalPlantCover$" = "AH_BasalCover")

  ##### Grouping variable lists -----------------------------------------------
  # These are the groupings of variables we'll use to calculate the indicators,
  # organized by which hit (first, any, or basal).
  # Note that a number of these variables will be defined below under:
  # Joining species info > Sanitization/harmonization
  fh_variable_groupings <- list(c("Duration", "GrowthHabitSub"),
                                c("Duration", "ForbGraminoid"),
                                c("GrowthHabitSub"),
                                c("SG_Group"),
                                c("Noxious", "Duration", "GrowthHabitSub"),
                                c("between_plant"),
                                c("Litter"),
                                c("Lichen"),
                                c("TotalLitter"),
                                c("Moss"))
  ah_variable_groupings <- list(c("GrowthHabit"),
                                c("GrowthHabitSub"),
                                c("Duration", "GrowthHabit"),
                                c("Duration", "GrowthHabitSub"),
                                c("Duration", "ForbGraminoid"),
                                c("ShrubSucculent"),
                                c("Noxious"),
                                c("Litter"),
                                c("TotalLitter"),
                                c("SG_Group"),
                                c("SG_Group", "Live"),
                                c("Grass"),
                                c("Duration", "Grass"),
                                c("C3", "Duration", "Grass"),
                                c("C4", "Duration", "Grass"),
                                c("Native"),
                                c("Invasive"),
                                c("Invasive", "Duration", "GrowthHabitSub"),
                                c("Invasive", "Duration", "ShrubSucculent"),
                                c("Invasive", "Duration", "Grass"),
                                c("Invasive", "Duration", "ForbGrass"),
                                c("Conifer"),
                                c("PJ"),
                                c("Moss"),
                                c("Rock"),
                                c("Biocrust"),
                                c("Lichen"))
  basal_variable_groupings <- list(c("Duration", "Grass"),
                                   c("Plant"))

  ##### Definitions for new variables ------------------------------------------
  # The following objects are values that we'll use to create new variables for
  # use in defining indicators.

  ###### Litter code categories ------------------------------------------------
  litter_codes <- list("HerbLitter" = c("HL", "L", "DN", "ER", "AM"),
                       "WoodyLitter" = c("WL"),
                       # "NonVegLitter" = c("HT", "NL", "AL"),
                       "EmbLitter" = c("EL"))

  ###### Rock codes ------------------------------------------------------------
  rock_codes <- c("R", "GR", "CB", "ST", "BY",
                  # These are LMF codes
                  "RF", "BR")

  ###### Between-plant codes ---------------------------------------------------
  # These are for grouping values for between-plant indicators
  # NOTE: IF YOU ADD A NEW CATEGORY DON'T FORGET TO INCLUDE IT IN THE MUTATE()
  # UNDER SANITIZATION/HARMONIZATION BELOW (searching for between_plant_codes
  # will turn it up)
  between_plant_codes <- list("WoodyLitter" = litter_codes[["WoodyLitter"]],
                              "HerbLitter" =  litter_codes[["HerbLitter"]],
                              # "NonVegLitter" = litter_codes[["NonVegLitter"]],
                              "EmbLitter" = litter_codes[["EmbLitter"]],
                              "DepSoil" = c("DS"),
                              "Duff" = c("D"),
                              "Lichen" = c("LC"),
                              "VagrLichen" = c("VL"),
                              "Moss" = c("M"),
                              "Cyanobacteria" = c("CY"),
                              "Water" = c("W", "WA"),
                              "Rock" = c(rock_codes),
                              "BareSoil" = c("AG", "CM", "LM", "FG", "PC", "S"))

  ###### Pinyon-juniper species codes ------------------------------------------
  pj_identifiers <- c("JUCA7",
                      "SACA29",
                      "JUCAS2",
                      "JUCAU",
                      "JUOCU",
                      "JUUT",
                      "SAUT3",
                      "JUCE2",
                      "JUAR3",
                      "JUCOA3",
                      "JUCOA2",
                      "JUDE2",
                      "JUNDEPD",
                      "JUNDEPS",
                      "JUDES",
                      "JUDES2",
                      "JUER",
                      "JUPIE",
                      "JUCO11",
                      "JUCOC2",
                      "JUERC",
                      "JUFL",
                      "JUNFLAF",
                      "SAFL16",
                      "JUGR7",
                      "JUNKNI",
                      "JUKN",
                      "JUMOK",
                      "JUME6",
                      "JUME7",
                      "JUUTM",
                      "JUMOG",
                      "JUNCFMON",
                      "JUNIP",
                      "JUNMEXM",
                      "JUMO",
                      "JUMOM",
                      "JUNOCCM",
                      "SAMO8",
                      "JUDEP",
                      "JUNDEPP2",
                      "JUNPAC",
                      "JUNPAC2",
                      "JUCAO",
                      "JUOS",
                      "JUNTETO",
                      "SAOS",
                      "JUOC",
                      "JUOCO",
                      "SAOC9",
                      "JUOCA2",
                      "JUOCA",
                      "JUNGYM",
                      "JUOCG",
                      "JUMOP",
                      "JUPI",
                      "JUSC2",
                      "JUVIS2",
                      "JUVIS4",
                      "SASC5",
                      "JUSCC2",
                      "JUSCP",
                      "PICA16",
                      "PIMOC2",
                      "PIMOC",
                      "PICA3",
                      "PIREC",
                      "PICE",
                      "PINCEMC",
                      "PINCEMB",
                      "PICEB",
                      "PICER",
                      "PINCULR",
                      "PIRE5",
                      "PICUD",
                      "PIDI3",
                      "CAREDU",
                      "PICEE",
                      "PIED",
                      "PIEDE",
                      "PINMONE",
                      "PINCALF",
                      "PIEDF",
                      "PINFAL",
                      "PIMOF2",
                      "PINMONF",
                      "PIMOF",
                      "APIFLE",
                      "PINCEMF",
                      "PIFL2",
                      "PIFLA",
                      "PIFLA2",
                      "PIFLC",
                      "PIFLC2",
                      "PINCEMJ",
                      "PINCULJ",
                      "PIJO",
                      "PIJU",
                      "PINQUAJ",
                      "CARMON2",
                      "PINCEMM",
                      "PINEDUM",
                      "PIMO",
                      "PIMOM2",
                      "PINCEMP2",
                      "PINPAR",
                      "PINCEMQ",
                      "PIQU")

  ###### Conifer families ------------------------------------------------------
  conifer_identifiers <- c("Cupressaceae",
                           "Pinaceae",
                           "Taxaceae")

  ###### Lichen codes ----------------------------------------------------------
  lichen_identifiers <- c(Lichen = "LC",
                          Cyanobacteria = "CY",
                          VagrLichen = "VL")

  ###### Biocrust codes --------------------------------------------------------
  biocrust_identifiers <- c("CY",
                            "LC",
                            "M")

  ###### Moss definitions ------------------------------------------------------
  # For moss cover, we need to identify species that use irregular unknown codes
  # and species that were keyed out in addition to the traditional "where does
  # 'M' occur as a surface code"
  # This will find codes like "MOSS", "M123", "MOS123", and "MOSS123"
  unknown_moss_regex <- "^(M(OS{1,2})?\\d+)|(MOSS)$"
  # In tblNationalPlants there's a variable called HigherTaxon that we can use
  # to identify which species codes are technically mosses. This is helpful
  # mostly for Alaska where they ID mosses to species, but anywhere we don't do
  # it runs the risk of underestimating the amount of moss cover if there are
  # any recorded in the canopy.
  moss_identifiers <- "Moss"

  #### Joining species info ----------------------------------------------------
  # If generic_species_file is not provided, assume it is the same as species_file
  if (is.null(generic_species_file)) {
    if (verbose) {
      message("No generic_species_file provided, using species_file in its place.")
    }
    generic_species_file <- species_file
  }

  if (verbose) {
    message("Checking species_file and reading in as necessary.")
  }

  if (is.character(species_file)) {
    current_species_file_extension <- tools::file_ext(species_file)

    if (nchar(current_species_file_extension) == 0) {
      stop("When species_file is a character string, it must be a filepath to either a CSV or a GDB (geodatabase).")
    } else if (current_species_file_extension %in% c("CSV", "csv")) {
      if (!file.exists(species_file)) {
        stop(paste0("The provided species_file value, ", species_file, ", points to a file that does not exist."))
      }
      species_list <- read.csv(file = species_file,
                               stringsAsFactors = FALSE)
    } else if (current_species_file_extension %in% c("GDB", "gdb")) {
      species_list <- species_read_aim(dsn = species_file,
                                       verbose = verbose)
    }
  } else if (is.data.frame(species_file)) {
    species_list <- species_file
  } else {
    stop("species_file must either be a filepath to a CSV or a GDB file or a data frame.")
  }

  if (verbose) {
    message("Attempting to join the species list to the LPI data.")
  }

  lpi_species <- species_join(data = sf::st_drop_geometry(lpi_tall_header),
                              data_code = "code",
                              species_file = species_list,
                              # This isn't hardcoded to accommodate other, non-
                              # AIM species lists.
                              species_code = species_code_var,
                              species_growth_habit_code = "GrowthHabitSub",
                              species_duration = "Duration",
                              # These won't all be present in every list, but
                              # that shouldn't be a problem because they're only
                              # used with an any_of().
                              species_property_vars = c("GrowthHabit",
                                                        "GrowthHabitSub",
                                                        "Duration",
                                                        "Family",
                                                        "SG_Group",
                                                        "HigherTaxon",
                                                        "Nonnative",
                                                        "Invasive",
                                                        "Noxious",
                                                        "SpecialStatus",
                                                        "Photosynthesis",
                                                        "PJ",
                                                        "CurrentPLANTSCode"),
                              growth_habit_file = "",
                              growth_habit_code = "Code",
                              # This FALSE should prevent us from having to
                              # worry about generic_species_file because that's
                              # only used to overwrite generic species info.
                              overwrite_generic_species = FALSE,
                              generic_species_file = generic_species_file,
                              update_species_codes = FALSE,
                              by_species_key = FALSE,
                              check_species = FALSE,
                              verbose = verbose)


  ##### Sanitization/harmonization #############################################
  # One big mutate() to do all this lifting.
  # We're harmonizing multiple variants (e.g., non-woody, nonwoody, etc. all
  # being changed to NonWoody) and adding some additional variables that we can
  # use for indicator calcs
  if (verbose) {
    message("Harmonizing species characteristics with AIM indicator needs.")
  }

  # Let's check for the required variables for all these.
  # If any are missing, we can warn the user that those variables will be
  # created but populated with NA and so no indicators that involve them will
  # be calculated.
  expected_variables <- c("GrowthHabit",
                          "GrowthHabitSub",
                          "Duration",
                          "Family",
                          "HigherTaxon",
                          "Nonnative",
                          "Invasive",
                          "Noxious",
                          "SpecialStatus",
                          "Photosynthesis",
                          "PJ",
                          "CurrentPLANTSCode")
  missing_expected_variables <- setdiff(x = c("GrowthHabit",
                                              "GrowthHabitSub",
                                              "Duration",
                                              "Family",
                                              "HigherTaxon",
                                              "Nonnative",
                                              "Invasive",
                                              "Noxious",
                                              "SpecialStatus",
                                              "Photosynthesis",
                                              "PJ",
                                              "CurrentPLANTSCode"),
                                        names(lpi_species))

  if (length(missing_expected_variables) > 0) {
    warning(paste0("The provided species information does not contain all expected variables required for the standard set of indicators. Indicators which depend on those variables will not be calculated. The variables in question are: ",
                   paste(missing_expected_variables,
                         collapse = ", ")))
    # This makes a new data frame without any data in it consisting of only the
    # missing variables and a number of rows equal to the number of lpi_species
    # records then binds them together.
    lpi_species <- matrix(nrow = nrow(lpi_species),
                          ncol = length(missing_expected_variables)) |>
      as.data.frame() |>
      setNames(object = _,
               nm = missing_expected_variables) |>
      dplyr::bind_cols(lpi_species,
                       .x = _)
  }


  lpi_species <- dplyr::mutate(.data = lpi_species,
                               ###### Duration ---------------------------------
                               # Update the Duration values so that we don't
                               # need to do special renaming of indicators.
                               # This also lumps biennials in with annuals.
                               Duration = dplyr::case_when(grepl(x = Duration,
                                                                 pattern = "perennial",
                                                                 ignore.case = TRUE) ~ "Peren",
                                                           grepl(x = Duration,
                                                                 pattern = "(annual)|(biennial)",
                                                                 ignore.case = TRUE) ~ "Ann",
                                                           is.na(Duration) ~ "duration_irrelevant",
                                                           .default = Duration),

                               ###### GrowthHabit ------------------------------
                               # Updates to the GrowthHabit variable to harmonize
                               # values with expectations, including adding a
                               # new value for nonvasculars which shifts those
                               # qualifying species out of the general nonwoody
                               # calculations
                               GrowthHabit = dplyr::case_when(grepl(x = GrowthHabit,
                                                                    pattern = "^non-?woody$",
                                                                    ignore.case = TRUE) ~ "NonWoody",
                                                              grepl(x = GrowthHabitSub,
                                                                    pattern = "^non-?vascular$",
                                                                    ignore.case = TRUE) ~ "Nonvascular",
                                                              # This removes sedges from consideration???
                                                              # Maybe an artifact of trying to avoid spitting
                                                              # out unused indicators
                                                              GrowthHabitSub == "Sedge" ~ "growthhabit_irrelevant",
                                                              # For first-hit calculations
                                                              is.na(GrowthHabit) ~ "growthhabit_irrelevant",
                                                              .default = GrowthHabit),

                               ###### GrowthHabitSub ---------------------------
                               # Updates to GrowthHabitSub, mostly harmonizing
                               # variations on naming conventions
                               GrowthHabitSub = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                       pattern = "forb",
                                                                       ignore.case = TRUE) ~ "Forb",
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^sub-?shrub$",
                                                                       ignore.case = TRUE) ~ "SubShrub",
                                                                 # Not sure why we're removing non-vasculars??
                                                                 # Maybe an artifact of trying to avoid spitting
                                                                 # out unused indicators. Blame Alaska.
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^non-?vascular$",
                                                                       ignore.case = TRUE) ~ "growthhabitsub_irrelevant",
                                                                 # Anyway, doing the exact same to moss
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^moss$",
                                                                       ignore.case = TRUE) ~ "growthhabitsub_irrelevant",
                                                                 # And to lichen
                                                                 grepl(x = GrowthHabitSub,
                                                                       pattern = "^lichen$",
                                                                       ignore.case = TRUE) ~ "growthhabitsub_irrelevant",
                                                                 # For first-hit calculations
                                                                 is.na(GrowthHabit) ~ "growthhabitsub_irrelevant",
                                                                 .default = GrowthHabitSub),
                               ###### Plant ------------------------------------
                               # This is for calculating basal cover by plants,
                               # total foliar cover, and making sure plant
                               # properties aren't assigned to non-plant records
                               # in variables that are made/modified below in
                               # this mutate() call.
                               #
                               # The value of this variable will be "Plant" only
                               # if ALL of the following criteria are true:
                               #   1) The growth habit was NOT set to
                               #     "growthhabit_irrelevant".
                               #   2) The growth habit is NOT NA because only
                               #     codes with assigned growth habits should
                               #     count.
                               #   3) The number of characters in code is >=3
                               #     which as of January 2026 is still a
                               #     difference between valid species codes and
                               #     all other types of code values.
                               #
                               # If a record fails any of those criteria, the
                               # value in this variable will be NA.
                               Plant = dplyr::case_when(!(GrowthHabit %in% c("growthhabit_irrelevant",
                                                                             NA)) &nchar(code) >= 3 ~ "Plant",
                                                        .default = NA),

                               ###### chckbox ----------------------------------
                               # The chckbox variable is a numeric representation
                               # of a logical value, but 0 is for a "dead" record
                               # and 1 is for a "live" record, so let's actually
                               # make that easy on ourselves
                               Live = dplyr::case_when(chckbox %in% c(0, "0") ~ "Live",
                                                       # chckbox %in% c("1") ~ "Dead",
                                                       .default = NA),

                               ###### ShrubSucculent ---------------------------
                               # Add a variable for shrubs and succulents so we
                               # can easily calculate indicators for just them
                               ShrubSucculent = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                       pattern = "shrub|succulent",
                                                                       ignore.case = TRUE) ~ "ShrubSucculent",
                                                                 .default = NA),

                               ###### Litter -----------------------------------
                               # For the litter cover
                               # The "irrelevant" values are so that we can get
                               # first hit calculated without inflating things
                               Litter = dplyr::case_when(code %in% litter_codes[["HerbLitter"]] ~ "HerbLitter",
                                                         code %in% litter_codes[["WoodyLitter"]] ~ "WoodyLitter",
                                                         .default = "litter_irrelevant"),

                               ###### TotalLitter ------------------------------
                               TotalLitter = dplyr::case_when(code %in% unlist(litter_codes) ~ "TotalLitter",
                                                              .default = "total_litter_irrelevant"),

                               ###### C3 (photosynthesis) ----------------------
                               # Make separate photosynthesis columns because at
                               # least one species is classified as both
                               C3 = dplyr::case_when(grepl(x = Photosynthesis,
                                                           pattern = "C3") ~ "C3",
                                                     .default = NA),

                               ###### C4 (photosynthesis) ----------------------
                               C4 = dplyr::case_when(grepl(x = Photosynthesis,
                                                           pattern = "C4") ~ "C4",
                                                     .default = NA),

                               ###### Grass ------------------------------------
                               # For all the grass-specific indicators
                               Grass = dplyr::case_when(Family %in% c("Poaceae") ~ "Grass",
                                                        .default = NA),

                               ###### SG_Group (sage-grouse) -------------------
                               # This is to turn the SG_Group codes into values
                               # that match the expected indicator names for
                               # our convenience.
                               SG_Group = stringr::str_replace_all(string = SG_Group,
                                                                   pattern = "StaturePerennialGrass",
                                                                   replacement = "PerenGrass"),
                               # This makes sure that the value in SG_Group is
                               # only the string associated with the group for
                               # the species code in the relevant state.
                               # Records where there's not a group value for the
                               # associated state (or "US") will get NA instead.
                               SG_Group = stringr::str_extract(string = SG_Group,
                                                               pattern = paste0("(?<=((US)|(", SpeciesState, ")):)[A-z]+")),
                               # This makes sure that we've assigned any shrubs
                               # that didn't get a sage-grouse group are
                               # assigned to "NonSagebrushShrub"
                               SG_Group = dplyr::case_when(is.na(SG_Group) & GrowthHabitSub == "Shrub" ~ "NonSagebrushShrub",
                                                           # So that first-hit calcs work as intended.
                                                           is.na(SG_Group) & GrowthHabitSub != "Shrub" ~ "Irrelevant",
                                                           .default = SG_Group),

                               ###### ForbGraminoid ----------------------------
                               # For combined forb and graminoid cover
                               ForbGraminoid = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                      pattern = "(^((graminoid)|(grass))$)|forb",
                                                                      ignore.case = TRUE) ~ "ForbGraminoid",
                                                                .default = NA),

                               ###### ForbGrass --------------------------------
                               # For combined forb and grass cover
                               ForbGrass = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                  pattern = "forb",
                                                                  ignore.case = TRUE) | Family %in% "Poaceae" ~ "ForbGrass",
                                                            .default = NA),

                               ###### Biocrust ---------------------------------
                               # For biocrust cover
                               Biocrust = dplyr::case_when(code %in% biocrust_identifiers ~ "Biocrust",
                                                           .default = NA),

                               ###### Lichen -----------------------------------
                               Lichen = dplyr::case_when(code %in% lichen_identifiers["Lichen"] ~ "Lichen",
                                                         code %in% lichen_identifiers["VagrLichen"] ~ "VagrLichen",
                                                         code %in% lichen_identifiers["Cyanobacteria"] ~ "Cyanobacteria",
                                                         .default = "lichen_irrelevant"),

                               ###### PJ ---------------------------------------
                               # For pinyon-juniper cover
                               PJ = dplyr::case_when(code %in% pj_identifiers ~ "PJ",
                                                     .default = NA),

                               ###### Conifer ----------------------------------
                               # For conifer cover
                               Conifer = dplyr::case_when(Family %in% conifer_identifiers ~ "Conifer",
                                                          .default = NA),

                               ###### Moss -------------------------------------
                               # For moss cover
                               Moss = dplyr::case_when(HigherTaxon %in% moss_identifiers |
                                                         stringr::str_detect(string = code,
                                                                             pattern = unknown_moss_regex) |
                                                         code %in% c("M") ~ "Moss",
                                                       # We're not keeping an indicator calculated for
                                                       # "Nonmoss" but we do need that info for first hits to work
                                                       .default = "moss_irrelevant"),

                               ###### Invasive ---------------------------------
                               # This is just to make the Invasive values match
                               # the desired indicator names
                               Invasive = stringr::str_to_title(string = Invasive),

                               ###### Native -----------------------------------
                               # This is for the native and non-native cover
                               # It assumes that everything flagged as EXOTIC or
                               # ABSENT should be considered NonNative and that
                               # everything else is Native
                               Native = dplyr::case_when(Nonnative %in% c("NATIVE", NA) & !is.na(Plant) ~ "Native",
                                                         !(Nonnative %in% c("NATIVE", NA)) & !is.na(Plant) ~ "NonNative",
                                                         .default = NA),

                               ###### Noxious ----------------------------------
                               # For noxious cover. This previously assumed that
                               # anything flagged as YES is noxious and nothing
                               # else is. This is now disabled because noxious
                               # status is being handled more appropriately and
                               # through a different format. I'm leaving this
                               # for posterity for the moment though.
                               # Noxious = dplyr::case_when(Noxious %in% c("YES") ~ "Noxious",
                               #                            .default = NA),
                               #
                               # Noxious is now encoded as a character string
                               # with localities separated by |s. We need to
                               # check for the relevant locality based on the
                               # State variable NOT the AdminState because these
                               # determinations are made based on the physical
                               # location of the sampling within the legal
                               # boundaries of states, not which state is
                               # administering the lands (which is sometimes
                               # different).
                               # The regex checks to see if the beginning of
                               # the string or the characters immediately
                               # following a | are "US", the code from the State
                               # variable, or the code from the State variable
                               # and the value from the County variable
                               # separated by a :, e.g., "OR:Jefferson".
                               # The single-letter designations for type of
                               # noxiousness are not taken into account, e.g.,
                               # "OR:A" and "OR:B" will be treated identically.
                               # County-level designations may eventually be
                               # removed, but for now they're still in there and
                               # this regex will work regardless.
                               Noxious = dplyr::case_when(stringr::str_detect(string = Noxious,
                                                                              pattern = paste0("(^|\\|)((", SpeciesState, ")|(US))")) ~ "Noxious",
                                                          .default = "noxious_irrelevant"),

                               ###### Rock -------------------------------------
                               Rock = dplyr::case_when(code %in% rock_codes ~ "Rock",
                                                       .default = NA),

                               ###### between_plant ----------------------------
                               between_plant = dplyr::case_when(code %in% between_plant_codes[["Woodylitter"]] ~ "WoodyLitter",
                                                                code %in% between_plant_codes[["HerbLitter"]] ~ "HerbLitter",
                                                                # code %in% between_plant_codes[["NonVegLitter"]] ~ "NonVegLitter",
                                                                code %in% between_plant_codes[["EmbLitter"]] ~ "EmbLitter",
                                                                code %in% between_plant_codes[["DepSoil"]] ~ "DepSoil",
                                                                code %in% between_plant_codes[["Duff"]] ~ "Duff",
                                                                code %in% between_plant_codes[["Lichen"]] ~ "Lichen",
                                                                HigherTaxon %in% moss_identifiers |
                                                                  stringr::str_detect(string = code,
                                                                                      pattern = unknown_moss_regex) |
                                                                  code %in% between_plant_codes[["Moss"]] ~ "Moss",
                                                                code %in% between_plant_codes[["Cyanobacteria"]] ~ "Cyanobacteria",
                                                                code %in% between_plant_codes[["Water"]] ~ "Water",
                                                                code %in% between_plant_codes[["Rock"]] ~ "Rock",
                                                                code %in% between_plant_codes[["VagrLichen"]] ~ "VagrLichen",
                                                                code %in% between_plant_codes[["BareSoil"]] ~ "BareSoil",
                                                                .default = "between_plant_irrelevant"),

                               ###### AdditionalRemoteSensing ------------------
                               # Special indicators for remote sensing use
                               AdditionalRemoteSensing = dplyr::case_when(code %in% c("DS") ~ "DepSoil",
                                                                          .default = "remote_sensing_irrelevant")
  )

  #### Calculations ############################################################
  ##### Total foliar cover #####################################################
  if (verbose) {
    message("Calculating total foliar cover.")
  }
  total_foliar <- pct_cover_total_foliar(lpi_tall = lpi_species,
                                         tall = TRUE,
                                         by_line = FALSE,
                                         digits = digits)

  ##### All other cover ########################################################
  variable_groups <- list("first" = fh_variable_groupings,
                          "any" = ah_variable_groupings,
                          "basal" = basal_variable_groupings)

  # This is going to look gnarly, but automates stuff so we don't have to do the
  # capitalization corrections by hand
  unique_grouping_vars <- unique(c(unlist(fh_variable_groupings),
                                   unlist(ah_variable_groupings),
                                   unlist(basal_variable_groupings)))
  capitalization_lookup_list <- lapply(X = unique_grouping_vars,
                                       data = lpi_species,
                                       FUN = function(X, data){
                                         # message(paste(X,
                                         #               collapse = ", "))
                                         current_values <- unique(data[[X]])
                                         current_values <- current_values[!is.na(current_values)]
                                         if (length(current_values) > 0) {
                                           setNames(object = current_values,
                                                    nm = paste0("^",
                                                                toupper(current_values),
                                                                "$"))
                                         } else {
                                           NULL
                                         }
                                       })
  names(capitalization_lookup_list) <- unique_grouping_vars

  # This calculates the indicators.
  # The first level is iterating over the list variable_groups, working through
  # the hit types and the second level is working through all the groupings
  # within the hit type.
  cover_indicators_list <- lapply(X = names(variable_groups),
                                  # X = "first",
                                  variable_groups = variable_groups,
                                  data = lpi_species,
                                  capitalization_lookup_list = capitalization_lookup_list,
                                  verbose = verbose,
                                  FUN = function(X, variable_groups, data, capitalization_lookup_list, verbose){
                                    current_hit <- X
                                    message(paste("Calculating", current_hit, "hit indicators."))

                                    current_variable_groupings <- variable_groups[[current_hit]]
                                    # For the current hit type ("first", "any",
                                    # "basal"), calculate indicators for each
                                    # required variable grouping
                                    current_results_list <- lapply(X = seq(length(current_variable_groupings)),
                                                                   data = data,
                                                                   hit = current_hit,
                                                                   current_variable_groupings = current_variable_groupings,
                                                                   capitalization_lookup_list = capitalization_lookup_list,
                                                                   verbose = verbose,
                                                                   FUN = function(X, data, hit, current_variable_groupings, capitalization_lookup_list, verbose){
                                                                     current_grouping_vars <- current_variable_groupings[[X]]
                                                                     if (verbose) {
                                                                       message(paste("Calculating", hit, "hit indicators grouped by the variable(s):",
                                                                                     paste(current_grouping_vars,
                                                                                           collapse = ", "),
                                                                                     paste0("(Grouping ", X, " of ", length(current_variable_groupings), ")")))
                                                                     }

                                                                     current_results_raw <- pct_cover(lpi_tall = data,
                                                                                                      tall = TRUE,
                                                                                                      by_line = FALSE,
                                                                                                      hit = hit,
                                                                                                      indicator_variables = current_grouping_vars,
                                                                                                      verbose = verbose,
                                                                                                      digits = digits)

                                                                     # Sometimes there are no data that had non-NA
                                                                     # values in the variables of interest, so
                                                                     # we have to be prepared for that.
                                                                     if (is.null(current_results_raw)) {
                                                                       if (verbose) {
                                                                         message("No qualifying data for the requested indicator(s). Returning NULL.")
                                                                       }
                                                                       return(NULL)
                                                                     }

                                                                     if (verbose) {
                                                                       message("Adjusting indicator names.")
                                                                     }

                                                                     # Now we rename the indicators.
                                                                     # We'll split them into their component parts
                                                                     # and then use the appropriate lookup vector
                                                                     # for each part to correct the capitalization.
                                                                     # There are more efficient ways to do this,
                                                                     # but this is extensible, standardized, and
                                                                     # basically hands-off for us when we update
                                                                     # indicators.
                                                                     current_results <- tidyr::separate_wider_delim(data = current_results_raw,
                                                                                                                    cols = indicator,
                                                                                                                    # Of course this doesn't use
                                                                                                                    # actual regex despite that
                                                                                                                    # being the tidyverse standard
                                                                                                                    delim = ".",
                                                                                                                    names = current_grouping_vars)


                                                                     # A for loop might actually be fastest (and
                                                                     # is certainly easiest), so that's the
                                                                     # solution for now.
                                                                     # I attempted to use mutate() with {{}} and
                                                                     # := but it wasn't evaluating the
                                                                     # str_replace_all() correctly because I couldn't
                                                                     # convince it to retrieve the relevant vector
                                                                     # with {{}} or dplyr::vars() for use as the
                                                                     # string argument.
                                                                     for (current_variable in current_grouping_vars) {
                                                                       current_results[[current_variable]] <- stringr::str_replace_all(string = current_results[[current_variable]],
                                                                                                                                       pattern = capitalization_lookup_list[[current_variable]])
                                                                     }

                                                                     # Having now made the variables with the
                                                                     # corrected components, we can recombine them
                                                                     current_results <- tidyr::unite(data = current_results,
                                                                                                     col = indicator,
                                                                                                     dplyr::all_of(current_grouping_vars),
                                                                                                     sep = "")

                                                                     # And add the hit prefix and "Cover" to the
                                                                     # indicator names
                                                                     current_prefix <- switch(EXPR = hit,
                                                                                              "first" = "FH_",
                                                                                              "any" = "AH_",
                                                                                              "basal" = "AH_Basal")
                                                                     current_results <- dplyr::mutate(.data = current_results,
                                                                                                      indicator = paste0(current_prefix,
                                                                                                                         indicator,
                                                                                                                         "Cover")) |>
                                                                       # And correct for the special case indicators
                                                                       dplyr::mutate(.data = _,
                                                                                     indicator = stringr::str_replace_all(string = indicator,
                                                                                                                          pattern = nonstandard_indicator_lookup))
                                                                     # We'll keep only the bare minimum here.
                                                                     dplyr::select(.data = current_results,
                                                                                   PrimaryKey,
                                                                                   indicator,
                                                                                   percent) |>
                                                                       # Get only the indicators we want to actually keep. Doing this saves us
                                                                       # from wasting memory storing unnecessary indicators even temporarily
                                                                       # and spares us the horror of storing them even less efficiently in
                                                                       # a wide format after this loop.
                                                                       dplyr::filter(.data = _,
                                                                                     indicator %in% expected_indicator_names)
                                                                   })

                                    # Bind all those results together
                                    dplyr::bind_rows(current_results_list)
                                  })

  # It's possible to accidentally calculate the same indicator more than once,
  # e.g. in Alaska where you might find "Moss" in the variable GrowthHabitSub
  # and so get a FH_MossCover when calculating both from GrowthHabitSub *AND*
  # SpecialConsiderationCode
  cover_indicators <- dplyr::bind_rows(cover_indicators_list) |>
    dplyr::distinct()

  #### Combine all LPI based cover indicators ##################################
  if (verbose) {
    message("Combining all cover indicators and converting to a wide format.")
  }
  lpi_indicators <- dplyr::bind_rows(cover_indicators,
                                     total_foliar) |>
    # Remove duplicates (which I guess is possible)
    dplyr::distinct(.data = _) |>
    # Spread to a wide format.
    tidyr::pivot_wider(data = _,
                       names_from = indicator,
                       values_from = percent,
                       values_fill = 0)

  ##### Sagebrush shape indicators ---------------------------------------------
  # If there are qualifying data, add sagebrush shape!
  if (any(!is.na(lpi_species$ShrubShape))) {
    if (verbose) {
      message("Calculating sagebrush shape indicators and joining to output.")
    }
    # Sagebrush shape is only recorded for live sagebrush according to the
    # protocol, so we're going to calculate with live = FALSE and rename it to
    # reflect the living status
    sagebrush_shape_calc <- sagebrush_shape(lpi_tall = lpi_species,
                                            live = FALSE) |>
      dplyr::rename_with(.data = _,
                         .fn = ~ stringr::str_replace(string = .x,
                                                      pattern = "_All_",
                                                      replacement = "_Live_"),
                         .cols = tidyselect::contains(match = "_All_"))
    lpi_indicators <- dplyr::left_join(x = lpi_indicators,
                                       y = sagebrush_shape_calc,
                                       relationship = "one-to-one",
                                       by = "PrimaryKey")
  } else {
    if (verbose) {
      message("No qualifying data were found in ShrubShape. Skipping sagebrush shape indicators.")
    }
  }

  #### Final munging ###########################################################
  # Keep only the indicators we want
  output <- dplyr::select(lpi_indicators,
                          PrimaryKey,
                          dplyr::any_of(expected_indicator_names))

  # We need to make sure that all indicator variables are numeric because
  # they're all cover values. Any NA values should be assumed to be 0s.
  # Also, it's totally inappropriate to return indicator values with 6+ decimal
  # places so we're rounding to a single decimal place.
  output <- dplyr::mutate(.data = output,
                          dplyr::across(.cols = -tidyselect::any_of(x = c("PrimaryKey",
                                                                          "SagebrushShape_Live_Predominant")),
                                        .fns = ~ as.numeric(.x) |>
                                          tidyr::replace_na(data = _,
                                                            replace = 0) |>
                                          round(x = _,
                                                digits = digits)))

  # Add in variables for indicators we want but which had no qualifying data and
  # therefore should have a value of 0 for all plots.
  # setdiff() is rad and I wish I'd known about it years ago.
  # We'll make sure to set ONLY the numeric indicators' NAs to 0.
  # The character indicators get NAs.
  character_value_indicators <- NULL
  output_missing_numeric_indicators <- setdiff(x = expected_indicator_names,
                                               y = c(names(output),
                                                     character_value_indicators))
  output[output_missing_numeric_indicators] <- 0
  output_missing_character_indicators <- setdiff(x = character_value_indicators,
                                                 y = names(output))
  output[output_missing_character_indicators] <- NA

  if (length(c(output_missing_numeric_indicators, output_missing_character_indicators)) > 0) {
    warning(paste("The following indicators had no qualifying data and have been populated with 0 or NA as appropriate. This is not unexpected with rare situations and is even likely with smaller data sets. The indicators in question are:",
                  paste(c(output_missing_numeric_indicators, output_missing_character_indicators),
                        collapse = ", ")))
  }

  # This will reorder the variables to be as expected!
  output <- dplyr::select(.data = output,
                          dplyr::all_of(c("PrimaryKey",
                                          expected_indicator_names)))

  output
}



# Calculate the Gap indicators for AIM
#' Calculate the standard Terrestrial AIM Database (TerrADat) Canopy Gap indicators
#' @description
#' This function calculates the full set of gap-derived indicators that are standard for TerrADat.
#'
#' For any other gap indicators, use the underlying function \code{gap_cover()}.
#'
#'
#' @param header Optional data frame or character string. The metadata for the plots involved in the calculations, this is used to filter or subset the data being used for the calculations and must contain this must contain the variable PrimaryKey. If a character string, this must point to a CSV file containing the data. If \code{NULL} then no filtering will occur. Defaults to \code{NULL}.
#' @param gap_tall  Data frame or character string. The long/tall-format gap data for the plots involved in the calculations. The format must match the output from \code{gather_gap()}. If a character string, this must point to a CSV file containing the data.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{1} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#'
#' @returns A data frame matching the format of gap indicators in TerrADat.
#' @export
gap_calc <- function(header = NULL,
                     gap_tall,
                     digits = 6,
                     verbose = FALSE) {
  if ("character" %in% class(header)) {
    if (toupper(tools::file_ext(header)) == "RDATA") {
      header <- readRDS(file = header)
    } else {
      stop("When header is a character string it must be the path to a .rds file containing tall LPI data.")
    }
  } else if ("data.frame" %in% class(header)) {
    header <- header
  } else if (!is.null(header)) {
    stop("header must be a data frame or a filepath to an .Rdata file that contains the header data frame.")
  }


  if (verbose) {
    message("Reading gap data")
  }
  if ("character" %in% class(gap_tall)) {
    if (toupper(tools::file_ext(gap_tall)) == "RDATA") {
      gap_tall <- readRDS(file = gap_tall)
    } else {
      stop("When gap_tall is a character string it must be the path to a .rds file containing tall LPI data.")
    }
  } else if ("data.frame" %in% class(gap_tall)) {
    gap_tall <- gap_tall
  } else {
    stop("gap_tall must be a data frame or a filepath to an .Rdata file that contains the gap_tall data frame.")
  }

  if (!is.null(header)) {
    if ("PrimaryKey" %in% names(header)) {
      pks <- unique(header$PrimaryKey)
    } else {
      warning("The variable 'PrimaryKey' does not appear in the provided header data. As a result, gap_tall will not be filtered prior to calculations. If you intend to restrict gap_tall to records based on PrimaryKey values, please provide that information in header or filter gap_tall prior to calling this function.")
      pks <- NULL
    }
  } else {
    pks <- NULL
  }

  if (length(pks) > 0) {
    gap_tall <- dplyr::filter(.data = gap_tall,
                              PrimaryKey %in% pks)
  }


  # Calculate indicators and rename them.
  gap_values <- gap_cover(gap_tall = gap_tall,
                          digits = digits,
                          tall = FALSE)$percent |>
    dplyr::select(.data = _,
                  tidyselect::all_of(x = c("PrimaryKey",
                                           GapCover_25_50 = "25-50",
                                           GapCover_51_100 = "51-100",
                                           GapCover_101_200 = "101-200",
                                           GapCover_200_plus = "201-Inf")))

  gap_values
}


# Calculate the Height indicators for AIM
#' Calculate the standard Terrestrial AIM Database (TerrADat) height indicators
#' @description
#' This function calculates the full set of height-derived indicators that are standard for TerrADat.
#'
#' For any other gap indicators, use the underlying function \code{gap_cover()}.
#'
#'
#' @param header Optional data frame or character string. The metadata for the plots involved in the calculations, this is used to filter or subset the data being used for the calculations and must contain this must contain the variable PrimaryKey. If a character string, this must point to a CSV file containing the data. If \code{NULL} then no filtering will occur. Defaults to \code{NULL}.
#' @param gap_tall  Data frame or character string. The long/tall-format gap data for the plots involved in the calculations. The format must match the output from \code{gather_gap()}. If a character string, this must point to a CSV file containing the data.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{2} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @param species_file Data frame or character string. The species characteristics information. If this is a character string for the filepath to a geodatabase, that geodatabase must contain both the tblNationalPlants and tblStateSpecies tables. Otherwise, this must either be the output from \code{species_read_aim()} or be a character string pointing to a CSV file containing the output from \code{species_read_aim()}.
#' @param species_code_var Character string. The name of the variable in the species characteristics that contains the species codes. Defaults to \code{"SpeciesCode"}.
#' @param source Character string. If \code{"terradat"} or \code{"aim"} (case insensitive) then live and "dead" heights will be calculated. Defaults to \code{NULL}.
#' @param generic_species_file Optional character string. Must specify the full path to a CSV containing generic species information. If this is \code{NULL}. Defaults to \code{NULL}.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{1} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#'
#' @returns A data frame matching the format of height indicators in TerrADat.
#' @export
height_calc <- function(header,
                        height_tall,
                        species_file = species_file,
                        species_code_var = "SpeciesCode",
                        source = NULL,
                        generic_species_file = NULL,
                        digits = 6,
                        verbose = FALSE) {
  if (verbose) {
    message("Beginning height calculations")
  }

  if ("character" %in% class(header)) {
    if (toupper(tools::file_ext(header)) == "RDATA") {
      header <- readRDS(header)
    } else {
      stop("When header is a character string it must be the path to a .Rdata file containing header data.")
    }
  }
  if ("character" %in% class(height_tall)) {
    if (toupper(tools::file_ext(height_tall)) == "RDATA") {
      height_tall <- readRDS(file = height_tall)
    } else {
      stop("When height_tall is a character string it must be the path to a .Rdata file containing tall LPI data.")
    }
  } else if ("data.frame" %in% class(height_tall)) {
    height_tall <- height_tall
  }

  height_tall_header <- dplyr::left_join(x = dplyr::select(.data = header,
                                                           # Swapped from all_of()
                                                           # because county was
                                                           # missing from some
                                                           # data.
                                                           tidyselect::any_of(c("PrimaryKey",
                                                                                "SpeciesState",
                                                                                "State",
                                                                                "County"))),
                                         y = height_tall,
                                         relationship = "one-to-many",
                                         by = "PrimaryKey")

  #### Joining species info ----------------------------------------------------
  # If generic_species_file is not provided, assume it is the same as species_file
  if (is.null(generic_species_file)) {
    if (verbose) {
      message("No generic_species_file provided, using species_file in its place.")
    }
    generic_species_file <- species_file
  }

  if (verbose) {
    message("Checking species_file and reading in as necessary.")
  }

  if (is.character(species_file)) {
    current_species_file_extension <- tools::file_ext(species_file)

    if (nchar(current_species_file_extension) == 0) {
      stop("When species_file is a character string, it must be a filepath to either a CSV or a GDB (geodatabase).")
    } else if (current_species_file_extension %in% c("CSV", "csv")) {
      if (!file.exists(species_file)) {
        stop(paste0("The provided species_file value, ", species_file, ", points to a file that does not exist."))
      }
      species_list <- read.csv(file = species_file,
                               stringsAsFactors = FALSE)
    } else if (current_species_file_extension %in% c("GDB", "gdb")) {
      species_list <- species_read_aim(dsn = species_file,
                                       verbose = verbose)
    }
  } else if (is.data.frame(species_file)) {
    species_list <- species_file
  } else {
    stop("species_file must either be a filepath to a CSV or a GDB file or a data frame.")
  }

  if (verbose) {
    message("Attempting to join the species list to the height data.")
  }

  height_species <- species_join(data = sf::st_drop_geometry(height_tall_header),
                                 data_code = "Species",
                                 species_file = species_list,
                                 # This isn't hardcoded to accommodate other, non-
                                 # AIM species lists.
                                 species_code = species_code_var,
                                 species_growth_habit_code = "GrowthHabitSub",
                                 species_duration = "Duration",
                                 # These won't all be present in every list, but
                                 # that shouldn't be a problem because they're only
                                 # used with an any_of().
                                 species_property_vars = c("GrowthHabit",
                                                           "GrowthHabitSub",
                                                           "Duration",
                                                           "Family",
                                                           "SG_Group",
                                                           "HigherTaxon",
                                                           "Nonnative",
                                                           "Invasive",
                                                           "Noxious",
                                                           "SpecialStatus",
                                                           "Photosynthesis",
                                                           "PJ",
                                                           "CurrentPLANTSCode"),
                                 growth_habit_file = "",
                                 growth_habit_code = "Code",
                                 # This FALSE should prevent us from having to
                                 # worry about generic_species_file because that's
                                 # only used to overwrite generic species info.
                                 overwrite_generic_species = FALSE,
                                 generic_species_file = generic_species_file,
                                 update_species_codes = FALSE,
                                 by_species_key = FALSE,
                                 check_species = FALSE,
                                 verbose = verbose)

  #### Cleanup! ################################################################
  # These are so we can assign a new variable called "pgpf" indicating which
  # records contain heights for perennial grasses or perennial forbs.
  pgpf_growthhabitsubs <- c("Forb/herb", "Forb", "Graminoid", "Grass")

  # Clean up variables and then keep only valid records, e.g., records where a
  # species was recorded with the a GrowthHabit_measured value that matches the
  # GrowthHabit value added in the species join OR the species was NA but there
  # was a recorded height value.
  height_species <- dplyr::mutate(.data = height_species,
                                  Duration = dplyr::case_when(stringr::str_detect(string = Duration,
                                                                                  pattern = "[Pp]eren") ~ "Peren",
                                                              .default = Duration),
                                  # Correct the Non-Woody to NonWoody
                                  GrowthHabit = dplyr::case_when(stringr::str_detect(string = GrowthHabit,
                                                                                     pattern = "^Non(-)?[Ww]oody$") ~ "NonWoody",
                                                                 .default = GrowthHabit),
                                  GrowthHabitSub = dplyr::case_when(stringr::str_detect(string = GrowthHabitSub,
                                                                                        pattern = "[Ff]orb") ~ "Forb",
                                                                    stringr::str_detect(string = GrowthHabitSub,
                                                                                        pattern = "[Gg]rass") ~ "Graminoid",
                                                                    .default = GrowthHabitSub),
                                  # Add a variable indicating if a record is tied to a perennial forb or grass
                                  pgpf = dplyr::case_when(Duration == "Peren" &
                                                            GrowthHabitSub %in% pgpf_growthhabitsubs ~ "PerenForbGraminoid",
                                                          .default = NA),
                                  # Get these values tuned up so we get the
                                  # expected indicator names.
                                  type = stringr::str_to_title(string = type),
                                  # Make sure that Noxious actually reflects the
                                  # assigned status for the state.
                                  Noxious = dplyr::case_when(stringr::str_detect(string = Noxious,
                                                                                 pattern = paste0("(^|\\|)((", SpeciesState, ")|(US))")) ~ "Nox",
                                                             .default = "NonNox"),
                                  # This makes sure that the value in SG_Group is
                                  # only the string associated with the group for
                                  # the species code in the relevant state.
                                  # Records where there's not a group value for the
                                  # associated state (or "US") will get NA instead.
                                  SG_Group = stringr::str_remove_all(string = SG_Group,
                                                                     pattern = "Stature") |>
                                    stringr::str_replace_all(string = _,
                                                             pattern = "Perennial",
                                                             replacement = "Peren") |>
                                    stringr::str_extract(string = _,
                                                         pattern = paste0("(?<=((US)|(", SpeciesState, ")):)[A-z]+")),
                                  # This makes sure that we've assigned any shrubs
                                  # that didn't get a sage-grouse group are
                                  # assigned to "NonSagebrushShrub"
                                  SG_Group = dplyr::case_when(is.na(SG_Group) & GrowthHabitSub == "Shrub" ~ "NonSagebrushShrub",
                                                              .default = SG_Group)
  ) |>
    # This is the bit that keeps records where the species was NA but there was
    # a height recorded OR the GrowthHabit as recorded matches the species info
    # added by species_join().
    dplyr::filter(.data = _,
                  is.na(Species) & !is.na(Height) |
                    GrowthHabit_measured == GrowthHabit)

  # Because we'll calculate "Hgt_Sagebrush_Live_Avg" if we ought to.
  if (toupper(source) %in% c("TERRADAT", "AIM")) {
    height_species <- dplyr::mutate(.data = height_species,
                                    Chkbox = dplyr::case_when(Chkbox %in% c(0, "0") ~ "_Live",
                                                              .default = as.character(Chkbox)))
  }

  #### Height calculations #####################################################
  # These are the output variables we anticipate getting back (and want)
  expected_indicator_variables <- c("Hgt_Woody_Avg",
                                    "Hgt_Herbaceous_Avg",
                                    "Hgt_Forb_Avg",
                                    "Hgt_PerenForb_Avg",
                                    "Hgt_Graminoid_Avg",
                                    "Hgt_PerenGraminoid_Avg",
                                    "Hgt_TallPerenGrass_Avg",
                                    "Hgt_ShortPerenGrass_Avg",
                                    "Hgt_PerenForbGraminoid_Avg",
                                    "Hgt_Shrub_Avg",
                                    "Hgt_NonSagebrushShrub_Avg",
                                    "Hgt_Sagebrush_Avg")

  # Let's do this with a lapply()!
  # We'll need the definitions here for which variables should be used as
  # indicator_variables for each pass.
  indicator_variables_list <- list(c("type"),
                                   c("GrowthHabitSub"),
                                   c("Duration",
                                     "GrowthHabitSub"),
                                   c("pgpf"),
                                   c("Noxious",
                                     "Duration",
                                     "GrowthHabitSub"),
                                   c("SG_Group"))

  # For TerrADat only
  if (source %in% c("TerrADat", "AIM")) {
    expected_indicator_variables <- c(expected_indicator_variables,
                                      "Hgt_Sagebrush_Live_Avg")
    indicator_variables_list[[length(indicator_variables_list) + 1]] <- c("SG_Group",
                                                                          "Chkbox")
  }

  height_values_list <- lapply(X = indicator_variables_list,
                               height_species = height_species,
                               verbose = verbose,
                               digits = digits,
                               FUN = function(X, height_species, verbose, digits){
                                 if (verbose) {
                                   message(paste(X,
                                                 collapse = ", "))
                                 }

                                 mean_height(height_tall = height_species,
                                             method = "mean",
                                             tall = TRUE,
                                             indicator_variables = X,
                                             digits = digits,
                                             verbose = verbose) |>
                                   dplyr::mutate(.data = _,
                                                 indicator = stringr::str_remove_all(string = indicator,
                                                                                     pattern = "\\.") |>
                                                   paste0("Hgt_",
                                                          . = _,
                                                          "_Avg"))
                               })

  output <- dplyr::bind_rows(height_values_list) |>
    dplyr::filter(.data = _,
                  indicator %in% expected_indicator_variables) |>
    tidyr::pivot_wider(data = _,
                       names_from = indicator,
                       values_from = mean_height,
                       values_fill = NA)

  missing_indicators <- setdiff(x = expected_indicator_variables,
                                y = names(output))
  if (length(missing_indicators) > 0) {
    warning(paste0("One or more expected indicators did not have qualifying data and will be returned with NA values. This is not unexpected, especially for sage-grouse vegetation indicators. The following indicators were not calculated: ",
                   paste(missing_indicators,
                         collapse = ", ")))
    output[, missing_indicators] <- NA
  }

  output
}



# Calculate species inventory
#' Calculate the standard Terrestrial AIM Database (TerrADat) species indicators
#' @description
#' This function calculates the full set of species-specific indicators that are standard for TerrADat.
#'
#' This depends on the species characteristics used being those found in tblNationalPlants.
#'
#' @param header Data frame or character string. The metadata for the plots involved in the calculations, this is used to add the SpeciesState variable by joining with the PrimaryKey variable. If a character string, this must point to a CSV file containing the data.
#' @param spp_inventory_tall  Data frame or character string. The long/tall-format species inventory data for the plots involved in the calculations. The format must match the output from \code{gather_species_inventory()}. If a character string, this must point to a CSV file containing the data.
#' @param species_file Data frame or character string. The species characteristics information. If this is a character string for the filepath to a geodatabase, that geodatabase must contain both the tblNationalPlants and tblStateSpecies tables. Otherwise, this must either be the output from \code{species_read_aim()} or be a character string pointing to a CSV file containing the output from \code{species_read_aim()}.
#' @param species_code_var Character string. The name of the variable in the species characteristics that contains the species codes. Defaults to \code{"SpeciesCode"}.
#' @param source Character string. If \code{"terradat"} or \code{"aim"} (case insensitive) then live and "dead" heights will be calculated. Defaults to \code{NULL}.
#' @param generic_species_file Optional character string. Must specify the full path to a CSV containing generic species information. If this is \code{NULL}. Defaults to \code{NULL}.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{1} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#'
#' @returns A data frame matching the format of height indicators in TerrADat.
#' @export
spp_inventory_calc <- function(header,
                               spp_inventory_tall,
                               species_file,
                               source,
                               species_code_var = "SpeciesCode",
                               generic_species_file = NULL,
                               verbose = FALSE) {
  if ("character" %in% class(header)) {
    if (toupper(tools::file_ext(header)) == "RDATA") {
      header <- readRDS(file = header)
    } else {
      stop("When header is a character string it must be the path to a .Rdata file containing header data.")
    }
  }
  if ("character" %in% class(spp_inventory_tall)) {
    if (toupper(tools::file_ext(spp_inventory_tall)) == "RDATA") {
      data <- readRDS(file = spp_inventory_tall)
    } else {
      stop("When spp_inventory_tall is a character string it must be the path to a .rds file containing tall LPI data.")
    }
  } else if ("data.frame" %in% class(spp_inventory_tall)) {
    data <- spp_inventory_tall
  }

  data <- dplyr::left_join(x = dplyr::select(.data = header,
                                             PrimaryKey,
                                             SpeciesState),
                           y = data,
                           by = "PrimaryKey")

  #### Joining species info ----------------------------------------------------
  # If generic_species_file is not provided, assume it is the same as species_file
  if (is.null(generic_species_file)) {
    if (verbose) {
      message("No generic_species_file provided, using species_file in its place.")
    }
    generic_species_file <- species_file
  }

  if (verbose) {
    message("Checking species_file and reading in as necessary.")
  }

  if (is.character(species_file)) {
    current_species_file_extension <- tools::file_ext(species_file)

    if (nchar(current_species_file_extension) == 0) {
      stop("When species_file is a character string, it must be a filepath to either a CSV or a GDB (geodatabase).")
    } else if (current_species_file_extension %in% c("CSV", "csv")) {
      if (!file.exists(species_file)) {
        stop(paste0("The provided species_file value, ", species_file, ", points to a file that does not exist."))
      }
      species_list <- read.csv(file = species_file,
                               stringsAsFactors = FALSE)
    } else if (current_species_file_extension %in% c("GDB", "gdb")) {
      species_list <- species_read_aim(dsn = species_file,
                                       verbose = verbose)
    }
  } else if (is.data.frame(species_file)) {
    species_list <- species_file
  } else {
    stop("species_file must either be a filepath to a CSV or a GDB file or a data frame.")
  }

  if (verbose) {
    message("Attempting to join the species list to the height data.")
  }

  data <- species_join(data = sf::st_drop_geometry(data),
                       data_code = "Species",
                       species_file = species_list,
                       # This isn't hardcoded to accommodate other, non-
                       # AIM species lists.
                       species_code = species_code_var,
                       species_growth_habit_code = "GrowthHabitSub",
                       species_duration = "Duration",
                       # These won't all be present in every list, but
                       # that shouldn't be a problem because they're only
                       # used with an any_of().
                       species_property_vars = c("GrowthHabit",
                                                 "GrowthHabitSub",
                                                 "Duration",
                                                 "Family",
                                                 "SG_Group",
                                                 "HigherTaxon",
                                                 "Nonnative",
                                                 "Invasive",
                                                 "Noxious",
                                                 "SpecialStatus",
                                                 "Photosynthesis",
                                                 "PJ",
                                                 "CurrentPLANTSCode"),
                       growth_habit_file = "",
                       growth_habit_code = "Code",
                       # This FALSE should prevent us from having to
                       # worry about generic_species_file because that's
                       # only used to overwrite generic species info.
                       overwrite_generic_species = FALSE,
                       generic_species_file = generic_species_file,
                       update_species_codes = FALSE,
                       by_species_key = FALSE,
                       check_species = FALSE,
                       verbose = verbose)

  # Cleanup to get things in order for the indicators
  data <- dplyr::mutate(.data = data,
                        Total = "Total",
                        ###### Invasive ---------------------------------
                        # This is just to make the Invasive values match
                        # the desired indicator names
                        Invasive = stringr::str_to_title(string = Invasive),

                        ###### Native -----------------------------------
                        # This is for the native and non-native cover
                        # It assumes that everything flagged as EXOTIC or
                        # ABSENT should be considered NonNative and that
                        # everything else is Native
                        Native = dplyr::case_when(Nonnative %in% c("NATIVE", NA) ~ "Native",
                                                  .default = "Nonnative"),

                        ###### Noxious ----------------------------------
                        # For noxious cover. This assumes that anything
                        # flagged as YES is noxious and nothing else is.
                        # NOTE: This is now disabled because noxious
                        # status is being handled more appropriately and
                        # through a different format. I'm leaving this
                        # for posterity for the moment though.
                        # Noxious = dplyr::case_when(Noxious %in% c("YES") ~ "Noxious",
                        #                            .default = NA),
                        # Noxious is now encoded as a character string
                        # with localities separated by |s. We need to
                        # check for the relevant locality based on the
                        # State variable NOT the AdminState because these
                        # determinations are made based on the physical
                        # location of the sampling within the legal
                        # boundaries of states, not which state is
                        # administering the lands (which is sometimes
                        # different).
                        # The regex checks to see if the beginning of
                        # the string or the characters immediately
                        # following a | are "US", the code from the State
                        # variable, or the code from the State variable
                        # and the value from the County variable
                        # separated by a :, e.g., "OR:Jefferson".
                        # The single-letter designations for type of
                        # noxiousness are not taken into account, e.g.,
                        # "OR:A" and "OR:B" will be treated identically.
                        # County-level designations may eventually be
                        # removed, but for now they're still in there and
                        # this regex will work regardless.
                        Noxious = dplyr::case_when(stringr::str_detect(string = Noxious,
                                                                       pattern = paste0("(^|\\|)((", SpeciesState, ")|(US))")) ~ "Noxious",
                                                   .default = "noxious_irrelevant"),)

  #### Calculating #############################################################
  # These are the output variables we anticipate getting back (and want)
  expected_indicator_variables <- c("NumSpp_Total",
                                    "NumSpp_Native",
                                    "NumSpp_Nonnative",
                                    "NumSpp_Invasive",
                                    "NumSpp_Noxious",
                                    "NumSpp_PreferredForb")

  indicator_variables_list <- list(c("Total"),
                                   c("Native"),
                                   c("Invasive"),
                                   c("Noxious"),
                                   c("SG_Group"))
  output_list <- lapply(X = indicator_variables_list,
                        data = data,
                        verbose = verbose,
                        FUN = function(X, data, verbose){
                          species_count(species_inventory_tall = data,
                                        indicator_variables = X,
                                        verbose = verbose) |>
                            dplyr::mutate(.data = _,
                                          indicator = paste0("NumSpp_",
                                                             indicator))
                        })

  output <- dplyr::bind_rows(output_list) |>
    dplyr::filter(.data = _,
                  indicator %in% expected_indicator_variables) |>
    tidyr::pivot_wider(data = _,
                       names_from = indicator,
                       values_from = n,
                       values_fill = 0)

  missing_indicators <- setdiff(x = expected_indicator_variables,
                                y = names(output))
  if (length(missing_indicators) > 0) {
    warning(paste0("One or more expected indicators did not have qualifying data and will be returned with 0 values. This is not unexpected, especially for sage-grouse vegetation indicators. The following indicators were not calculated: ",
                   paste(missing_indicators,
                         collapse = ", ")))
    output[, missing_indicators] <- 0
  }

  output
}


# Calculate soil stability values
#' Calculate the standard Terrestrial AIM Database (TerrADat) soil stability indicators
#' @description
#' This function calculates the full set of soil stability-derived indicators that are standard for TerrADat.
#'
#'
#' @param soil_stability_tall  Data frame or character string. The long/tall-format gap data for the plots involved in the calculations. The format must match the output from \code{gather_soil_stability()}. If a character string, this must point to a CSV file containing the data.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{2} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @param species_file Data frame or character string. The species characteristics information. If this is a character string for the filepath to a geodatabase, that geodatabase must contain both the tblNationalPlants and tblStateSpecies tables. Otherwise, this must either be the output from \code{species_read_aim()} or be a character string pointing to a CSV file containing the output from \code{species_read_aim()}.
#' @param species_code_var Character string. The name of the variable in the species characteristics that contains the species codes. Defaults to \code{"SpeciesCode"}.
#' @param source Character string. If \code{"terradat"} or \code{"aim"} (case insensitive) then live and "dead" heights will be calculated. Defaults to \code{NULL}.
#' @param generic_species_file Optional character string. Must specify the full path to a CSV containing generic species information. If this is \code{NULL}. Defaults to \code{NULL}.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{1} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#'
#' @returns A data frame matching the format of height indicators in TerrADat.
#' @export
soil_stability_calc <- function(soil_stability_tall,
                                digits = 6,
                                verbose = FALSE) {
  if ("character" %in% class(soil_stability_tall)) {
    if (toupper(tools::file_ext(soil_stability_tall)) == "RDATA") {
      data <- readRDS(file = soil_stability_tall)
    } else {
      stop("When soil_stability_tall is a character string it must be the path to a .rds file containing tall LPI data.")
    }
  } else if ("data.frame" %in% class(soil_stability_tall)) {
    data <- soil_stability_tall
  }

  # Drop the NA values
  data <- dplyr::filter(.data = data,
                        !is.na(Rating))

  indicators <- soil_stability(data,
                               all = TRUE,
                               cover = TRUE,
                               uncovered = TRUE,
                               all_cover_types = FALSE,
                               tall = FALSE,
                               digits = digits)
  indicators
}
