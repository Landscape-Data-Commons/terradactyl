#### GATHER ALL WRAPPER ########################################################
#' Gather data for plot metadata, line-point intercept, gap, vegetation height, soil stability, species inventory, and rangeland health.
#'
#' @description Given wide format AIM/LMF data, produce long/tall-format versions.
#'
#' This uses the family of gather functions in terradactyl. For additional
#' documentation regarding those, please see \code{\link[terradactyl:gather_header]{gather_header()}},
#' \code{\link[terradactyl:gather_lpi]{gather_lpi()}}, \code{\link[terradactyl:gather_height]{gather_height()}},
#' \code{\link[terradactyl:gather_gap]{gather_gap()}}, \code{\link[terradactyl:gather_soil_stability]{gather_soil_stability()}},
#' \code{\link[terradactyl:gather_species_inventory]{gather_species_inventory()}},
#' \code{\link[terradactyl:gather_rangeland_health]{gather_rangeland_health()}}
#'
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param source Character string. This specifies the expected data format(s)
#'   and determines which gather function will be used. It must be
#'   one or more of of \code{"AIM"}, \code{"TERRADAT"}, \code{"LMF"}, or \code{"DIMA"}.
#'   This is case-insensitive.
#' @param data_list Named list of data frames or character strings. These may be
#' data frames containing the data that will be gathered, character strings
#' specifying the filepath to CSV or Rdata files containing the data, or character
#' strings specifying the name of the corresponding feature class in a geodatabase
#' specified by the argument \code{dsn}. The names in this list must match the
#' argument names expected by the gather functions, e.g.
#' \code{"PINTERCEPT"} or \code{"tblPlots"}. See the documentation for those
#' functions for additional details. If \code{NULL} this will be ignored.
#' Defaults to \code{NULL}.
#' @param data_types Vector of character strings. These are the data types to
#' gather into long/tall-format. Valid values are \code{"header"}, \code{"lpi"}, \code{"height"}, \code{"gap"}, \code{"soilstability"}, \code{"species"}, and \code{"rangelandhealth"}.
#' This is case-insensitive. Defaults to \code{c("header", "lpi", "height", "gap", "soilstability", "species", "rangelandhealth")}.
#' @param output_filepath Optional character string. The filepath to a folder to
#' save all outputs to as RDS or CSV files. This folder must already exist. If
#' this is not \code{NULL} then the function will write the output and will NOT
#' return any outputs in the R environment. If this is \code{NULL} then no
#' data will be written to file and the function will return a list of data frames,
#' one for each data type. Defaults to \code{NULL}.
#' @param output_filetype Optional vector of character strings. This determines
#' what file(s) type to write the outputs as in \code{output_filepath}. Valid file
#' values are \code{"CSV"} and \code{"RDS"}, case-insensitive. Only considered if
#' \code{output_filepath} is not \code{NULL}. Defaults to \code{c("csv", "rds")}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @returns If \code{output_filepath} is \code{NULL}, a list of tall data frames containing reformatted input data. Otherwise, no outputs in the R environment but files written into \code{output_filepath}.
#' @examples
#' # To process all AIM and LMF data in a geodatabase and save them in the
#' # folder called "output" in the working directory as both CSV and RDS files.
#' gather_all(dsn = "Path/To/AIM-LMF_Geodatabase.gdb",
#'            output_filepath = "output")
#'
#' # To process line-point intercept and gap data from AIM in a geodatabase and
#' # store them in the environment as a list of data frames with diagnostic
#' # messages active.
#' long_data_list <- gather_all(dsn = "Path/To/AIM-LMF_Geodatabase.gdb",
#'                              source = "aim",
#'                              data_types = c("lpi",
#'                                             "gap"),
#'                              verbose = TRUE)
#'
#' # To process wide-format LMF height and species inventory data that are
#' # already in a data frame in the environment.
#' long_data_list <- gather_all(source = "lmf",
#'                              data_types = c("height",
#'                                             "speciesinventory"),
#'                              data_list = list(PASTUREHEIGHTS = pastureheights_data_frame,
#'                                               PLANTCENSUS = plantcensus_data_frame))
#' @export

gather_all <- function(dsn = NULL,
                       source,
                       data_list = NULL,
                       data_types = c("header",
                                      "lpi",
                                      "height",
                                      "gap",
                                      "soilstability",
                                      "species",
                                      "rangelandhealth"),
                       output_filepath = NULL,
                       output_filetype = c("csv", "rds"),
                       # skip_missing = FALSE,
                       verbose = FALSE) {
  #### Sanitization ------------------------------------------------------------
  ##### Source -----------------------------------------------------------------
  source <- tolower(source) |>
    unique()

  valid_source_values <- c("aim",
                           "terradat",
                           "dima",
                           "lmf")

  # This collapses synonyms to avoid reprocessing data multiple times.
  if (sum(c("aim",
            "terradat",
            "dima") %in% source) > 1) {
    message('Because the values "aim", "terradat", and "dima" are all handled the same way and more than one has been provided, they will be replaced with "aim" to avoid duplicating work.')

    source <- c(setdiff(x = source,
                        y = c("aim",
                              "terradat",
                              "dima")),
                "aim") |>
      unique()
  }

  recognized_sources <- intersect(x = source,
                                  y = valid_source_values)
  if (length(recognized_sources) < 1) {
    stop(paste0('source must be a vector of one or more of the following character strings: "',
                paste(valid_source_values,
                      collapse = '", "'),
                '"'))
  }

    ##### Check data_types -------------------------------------------------------
  valid_data_types <- c("header" = "header",
                        "LPI" = "lpi",
                        "height" = "height",
                        "gap" = "gap",
                        "soil stability" = "soilstability",
                        "species inventory" = "species",
                        "rangeland health" = "rangelandhealth")

  requested_data_types <- intersect(x = valid_data_types,
                                    y = tolower(data_types))

  # This bit brings the names through which we use for messages later.
  requested_data_types <- valid_data_types[valid_data_types %in% requested_data_types]

  if (length(requested_data_types) < 1) {
    stop(paste0('None of the requested data types were recognized. The argument data_types must be a vector of one or more of the following character strings: "',
                paste(valid_data_types,
                      collapse = '", "')),
         '"')
  }

  if (verbose) {
    message(paste0("The following data types will be read in: ",
                   paste(requested_data_types,
                         collapse = ", ")))
  }

  ##### Check data_list --------------------------------------------------------
  method_argument_names <- list("aim" = list("header" = c("tblPlots",
                                                          "date_tables"),
                                             "lpi" = c("tblLPIHeader",
                                                       "tblLPIDetail"),
                                             "height" = c("tblLPIHeader",
                                                          "tblLPIDetail"),
                                             "gap" = c("tblGapHeader",
                                                       "tblGapDetail"),
                                             "soilstability" = c("tblSoilStabHeader",
                                                                 "tblSoilStabDetail"),
                                             "species" = c("tblSpecRichHeader",
                                                           "tblSpecRichDetail"),
                                             "rangelandhealth" = c("tblQualHeader",
                                                                   "tblQualDetail")),
                                "lmf" = list( "header" = c("POINT",
                                                           "POINTCOORDINATES",
                                                           "GPS",
                                                           "ESFSG"),
                                              "lpi" = "PINTERCEPT",
                                              "height" = "PASTUREHEIGHTS",
                                              "gap" = "GINTERCEPT",
                                              "soilstability" = "SOILDISAG",
                                              "species" = "PLANTCENSUS",
                                              "rangeland health" = "RANGEHEALTH"))

  if (verbose) {
    message("Checking data_list for superficial validity. Additional checks will take place later.")
  }

  bad_data_list_values <- lapply(X = recognized_sources,
                                 method_argument_names = method_argument_names,
                                 requested_data_types = requested_data_types,
                                 data_list = data_list,
                                 verbose = verbose,
                                 FUN = function(X, method_argument_names, requested_data_types, data_list, verbose){
                                   if (any(c("aim", "terradat", "dima") %in% X)) {
                                     current_method_argument_names <- method_argument_names[["aim"]]
                                   } else {
                                     current_method_argument_names <- method_argument_names[[X]]
                                   }

                                   arguments_to_check <- current_method_argument_names[requested_data_types] |>
                                     unlist()

                                   valid_classes <- sapply(X = arguments_to_check,
                                                           data_list = data_list,
                                                           FUN = function(X, data_list){
                                                             is.null(data_list[[X]]) | is.data.frame(data_list[[X]]) | is.character(data_list[[X]])
                                                           })

                                   arguments_to_check[!valid_classes]
                                 }) |>
    unlist()

  if (length(bad_data_list_values) > 0) {
    stop(paste0("The following in data_list are not a valid class (either data frame or character string): ",
                paste(bad_data_list_values,
                      collapse = ", ")))
  }


  anticipated_data_list_names <- lapply(X = recognized_sources,
                                        method_argument_names = method_argument_names,
                                        requested_data_types = requested_data_types,
                                        FUN = function(X, method_argument_names, requested_data_types){
                                          method_argument_names[[X]][requested_data_types]
                                        }) |>
    unlist()
  extraneous_data_list_names <- setdiff(x = names(data_list),
                                        anticipated_data_list_names)

  if (length(extraneous_data_list_names) > 0) {
    warning(paste0("The following were included in data_list but will not be used because they are not relevant to the data types being worked with: ",
                   paste(extraneous_data_list_names,
                         collapse = ", ")))
  }


  ##### Check output_filepath --------------------------------------------------
  if (!is.null(output_filepath) & !is.character(output_filepath)) {
    stop("output_filepath must be a character string specifying the filepath to an existing folder.")
  }

  if (is.character(output_filepath)) {
    if (length(output_filepath) > 1) {
      stop("output_filepath must be a single character string specifying the filepath to an existing folder.")
    }
    if (nchar(tools::file_ext(output_filepath)) > 0) {
      stop(paste0("output_filepath must be a single character string specifying the filepath to an existing folder but it currently has the file extension ",
                  tools::file_ext(output_filepath)))
    }
    if (!file.exists(output_filepath)) {
      stop(paste0("The specified output_filepath, ", output_filepath, ", does not exist."))
    }
    if (verbose) {
      message(paste0("Gathered data will be written to ", output_filepath))
    }
  } else if (is.null(output_filepath) & verbose) {
    message(paste0("Gathered data will be returned as a list of data frames."))
  }

  ##### Check output_filetype --------------------------------------------------
  if (!is.null(output_filetype)) {
    output_filetype <- tolower(output_filetype) |>
      unique()

    valid_filetypes <- c("csv",
                         "rds")

    recognized_filetypes <- intersect(x = output_filetype,
                                      y = valid_filetypes)

    if (length(recognized_filetypes) < 1) {
      stop(paste0('output_filetype must be a vector of one or more of the following character strings: "',
                  paste(valid_filetypes,
                        collapse = '", "'),
                  '"'))
    }
  }


  #### Reading, gathering, and writing -----------------------------------------
  # For each of the requested data types, run the relevant gather function.
  # If output_filepath is not NULL, the results will be written out to that
  # location. Otherwise, the data are stored in an output list.

  # If more than one recognized source is specified, the outputs will be written
  # separately but the output list will have them combined by data type.

  # TODO: If skip_missing is TRUE, the tryCatch() should let NULLs be returned
  # (but not written anywhere).

  output <- list()

  for (current_source in recognized_sources) {
    for (current_data_type in requested_data_types) {
      if (verbose) {
        message(paste0("Attempting to gather ", names(requested_data_types[requested_data_types %in% current_data_type]), " data."))
      }

      # Note that the arguments are being pulled from data_list by name because
      # that will return NULL for any that weren't provided and that plays
      # nicely with the internal functions read_whatever() and
      # read_with_fallback() that the gather functions use.
      current_output <- switch(EXPR = current_data_type,
                               "header" = {
                                 gather_header(dsn = dsn,
                                               source = current_source,
                                               tblPlots = data_list[["tblPlots"]],
                                               POINT = data_list[["POINT"]],
                                               POINTCOORDINATES = data_list[["POINTCOORDINATES"]],
                                               GPS = data_list[["GPS"]],
                                               ESFSG = data_list[["ESFSG"]],
                                               date_tables = data_list[["date_tables"]],
                                               verbose = verbose)
                               },
                               "lpi" = {
                                 gather_lpi(dsn = dsn,
                                            source = current_source,
                                            tblLPIHeader = data_list[["tblLPIHeader"]],
                                            tblLPIDetail = data_list[["tblLPIDetail"]],
                                            PINTERCEPT = data_list[["PINTERCEPT"]],
                                            verbose = verbose)
                               },
                               "height" = {
                                 gather_height(dsn = dsn,
                                               source = current_source,
                                               tblLPIHeader = data_list[["tblLPIHeader"]],
                                               tblLPIDetail = data_list[["tblLPIDetail"]],
                                               PASTUREHEIGHTS = data_list[["PASTUREHEIGHTS"]],
                                               verbose = verbose)
                               },
                               "gap" = {
                                 gather_gap(dsn = dsn,
                                            source = current_source,
                                            tblGapHeader = data_list[["tblGapHeader"]],
                                            tblGapDetail = data_list[["tblGapDetail"]],
                                            GINTERCEPT = data_list[["GINTERCEPT"]],
                                            verbose = verbose)
                               },
                               "soilstability" = {
                                 gather_soil_stability(dsn = dsn,
                                                       source = current_source,
                                                       tblSoilStabHeader = data_list[["tblSoilStabHeader"]],
                                                       tblSoilStabDetail = data_list[["tblSoilStabDetail"]],
                                                       SOILDISAG = data_list[["SOILDISAG"]],
                                                       verbose = verbose)
                               },
                               "species" = {
                                 gather_species_inventory(dsn = dsn,
                                                          source = current_source,
                                                          tblSpecRichHeader = data_list[["tblSpecRichHeader"]],
                                                          tblSpecRichDetail = data_list[["tblSpecRichDetail"]],
                                                          PLANTCENSUS = data_list[["PLANTCENSUS"]],
                                                          verbose = verbose)
                               },
                               "rangelandhealth" = {
                                 gather_rangeland_health(dsn = dsn,
                                                         source = current_source,
                                                         tblQualHeader = data_list[["tblQualHeader"]],
                                                         tblQualDetail = data_list[["tblQualDetail"]],
                                                         RANGEHEALTH = data_list[["RANGEHEALTH"]],
                                                         verbose = verbose)
                               })

      if (!is.null(output_filepath)) {
        for (filetype in recognized_filetypes) {
          current_output_filename <- paste0(current_data_type,
                                            "_",
                                            current_source,
                                            ".",
                                            filetype)
          if (verbose) {
            message(paste0("Writing ",
                           current_output_filename))
          }
          switch(EXPR = filetype,
                 "rds" = {
                   saveRDS(object = current_output,
                           file = file.path(output_filepath,
                                            current_output_filename))
                 },
                 "csv" = {
                   write.csv(x = current_output,
                             file = file.path(output_filepath,
                                              current_output_filename),
                             row.names = FALSE)
                 })
        }
      } else {
        # If not being written out, bind it to whatever has already been read in
        # because they should have a source variable anyway.
        output[[current_data_type]] <- dplyr::bind_rows(output[[current_data_type]],
                                                        current_output)
      }

      if (verbose) {
        message("Cleaning up and freeing memory.")
      }

      # Remove the data from the working environment to save memory!
      rm(current_output)

      # And do some memory cleanup just to be safe. The function gc() always
      # returns a matrix but one that isn't useful for us to expose to the user,
      # so the invisible() hides that from appearing in the console.
      gc() |>
        invisible()
    }
  }


  if (is.null(output_filepath)) {
    return(output)
  }
}


#### HEADERS ###################################################################
#' Gather AIM plot-level header data
#' @description This reads in metadata from AIM sampling used as headers for
#' various methods and returns it as a tall/long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}. Dates are
#' added from one or more other tables associated with the specific data
#' collection methods. The expected format is that used in the Terrestrial AIM
#' Database (TerrADat).
#'
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param tblPlots Data frame or character string. If a data frame, must contain the variables PrimaryKey, SpeciesState, PlotID, PlotKey, EcolSite, Latitude, Longitude, State, Elevation, CountyName, EstablishDate, DateLoadedInDb, and SamplingApproach. If a character string, must either correspond to the filepath to a CSV or RDATA file containing a table with those variables or the name of the feature class in the geodatabase provided as \code{dsn} with those variables. If \code{NULL}, the function will attempt to find a feature class called tblPlots (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param date_tables Optional (contingent on other arguments) character vector
#'   or list of data frames. This specifies the tables to extract date
#'   information from. If \code{dsn} is not \code{NULL} AND the geodatabase
#'   contains at least one feature class from the set tblLPIHeader,
#'   tblGapHeader, and tblSpecRichHeader, this argument is optional and can be
#'   left as \code{NULL}. Otherwise, if \code{dsn} is not \code{NULL} and the
#'   desired tables to use are named anything else, this must be a vector of
#'   character strings specifying the names of the relevant feature classes. If
#'   \code{tblPlots} is being used, then this must be a list of data frames
#'   containing the relevant data. Defaults to \code{NULL}.
#' @param ... Additional optional filtering expressions passed to
#'   \code{\link[dplyr:filter]{dplyr::filter()}} to restrict the processing and output to only a
#'   subset of input data.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @returns A long-format data frame of header data.
#' @examples
#' # Using a geodatabase which already contains the tables tblPLots and at least
#' # one automatically-recognized table to pull dates from.
#' gather_header_terradat(dsn = "data_folder/aim_data.gdb")
#'
#' # Using a data frame containing the contents of a tblPlots.
#' # In this example, no path to a geodatabase is specified with dsn, so a list
#' # containing at least one date table must also be provided.
#' gather_header_terradat(tblPlots = tblPlots_data_frame,
#'                        date_tables = list(tblLPIHeader,
#'                        tblGapHeader))
#'
#' # Using a geodatabase which already contains the tables tblPLots in
#' # conjunction with a filtering expression to limit the output to records
#' # where the State variable contained either of the values "CO" and "NM".
#' gather_header_terradat(dsn = "data_folder/aim_data.gdb",
#'                        State %in% c("CO", "NM"))
#'
#' @export
gather_header_terradat <- function(dsn = NULL,
                                   tblPlots = NULL,
                                   date_tables = NULL,
                                   ...,
                                   verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         # For once these variables need to carry through!
                         # "DateLoadedInDb",
                         # "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")

  # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)


  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblPlots,
                               default_name = "tblPlots",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose)

  # Restrict only to records which match the filtering criteria.
  header <- dplyr::filter(.data = header,
                          !!!filter_exprs)

  # In the case that there's a DateLoadedInDB variable, make sure those values
  # are in a variable called DateLoadedInDb instead.
  header <- dplyr::select(.data = header,
                          tidyselect::everything(),
                          tidyselect::any_of(x = c(DateLoadedInDb = "DateLoadedInDB"))) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of("DateLoadedInDB"))

  # These are variables which need to exist. If they don't, we'll just populate
  # them with NA.
  potentially_missing_vars <- c("Design",
                                "DesignFlag",
                                "Purpose",
                                "PurposeFlag",
                                "ProjectName")
  # Perversely, this is easiest with a loop.
  for (var_name in setdiff(x = potentially_missing_vars,
                           y = names(header))) {
    header[[var_name]] <- NA
  }

  # Narrow it down to the variables we actually need and rename them as
  # necessary. If you're using tidyselect functions, you can use a named vector
  # to rename as you select.
  header <- dplyr::select(.data = header,
                          tidyselect::all_of(c("PrimaryKey",
                                               "SpeciesState",
                                               "PlotID",
                                               "PlotKey",
                                               EcologicalSiteId = "EcolSite",
                                               Latitude_NAD83 = "Latitude",
                                               Longitude_NAD83 = "Longitude",
                                               "State",
                                               "Elevation",
                                               County = "CountyName",
                                               DateEstablished = "EstablishDate",
                                               "DateLoadedInDb",
                                               "SamplingApproach",
                                               "DesignFlag",
                                               "Purpose",
                                               "PurposeFlag",
                                               "DesignName"))) |>
    # We only want to keep records with PrimaryKey values.
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  # We need to grab dates from the other tables.
  # If date_tables was provided, we'll work with that. Otherwise, we'll snag
  # them from the GDB pointed to by dsn.
  if(!is.null(dsn) & is.null(date_tables)){
    available_layers <- sf::st_layers(dsn = dsn)$name

    desired_date_tables <- c("tblLPIHeader",
                             "tblGapHeader",
                             "tblSpecRichHeader")

    date_tables <- lapply(X = desired_date_tables,
                          dsn = dsn,
                          verbose = verbose,
                          FUN = function(X, dsn, verbose){
                            if (verbose) {
                              message(paste("Attempting to reading dates from",
                                            X))
                            }
                            current_date_table <- read_whatever(input = dsn,
                                                                layer = X,
                                                                regex = TRUE,
                                                                best_guess = TRUE,
                                                                accept_failure = TRUE,
                                                                verbose = verbose)

                            if (!is.null(current_date_table)) {
                              current_date_table <- sf::st_drop_geometry(x = current_date_table) |>
                                # This'll happen again in a bit, but just to
                                # save on memory we'll pare it down here.
                                dplyr::select(.data = _,
                                              tidyselect::all_of(x = c("PrimaryKey")),
                                              tidyselect::any_of(c(Date = "FormDate",
                                                                   Date = "CollectDate"))) |>
                                dplyr::distinct()
                            }
                            current_date_table
                          })
  }

  if(is.null(date_tables)){
    stop("date_tables must be provided if dsn is not. Provide a list of tables containing the variables FormDate or collectDate.")
  }

  if(class(date_tables) != "list"){
    stop("date_tables must be a list of minimum length 1. If you intend to provide a single data frame as date_tables, wrap it in list() like this: list(whatever_data_frame)")
  }

  # Take all the tables and return a record for each PrimaryKey with the first
  # valid date found in the provided data.
  dates <- lapply(X = date_tables,
                  FUN = function(X){
                    dplyr::select(.data = X,
                                  PrimaryKey,
                                  tidyselect::any_of(c("Date",
                                                       Date = "FormDate",
                                                       Date = "CollectDate"))) |>
                      dplyr::distinct()
                  }) |>
    dplyr::bind_rows() |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(c("PrimaryKey")),
                     DateVisited = dplyr::first(x = na.omit(Date),
                                                order_by = na.omit(Date))) |>
    dplyr::distinct()

  # Return the header with the date added.
  dplyr::left_join(x = header,
                   y = dates,
                   relationship = "one-to-one",
                   by = "PrimaryKey") |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()
}

#' Gather LMF plot-level header data
#' @description This reads in metadata from AIM sampling used as headers for
#' various methods and returns it as a long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}. The required
#' feature classes in the geodatabase specified with \code{dsn} are POINT,
#' POINTCOORDINATES, GPS, and ESFSG. The expected formats for
#' the input data are those used in the Landscape Monitoring Framework.
#'
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param POINT Data frame or character string. If a data frame, must contain the variables PrimaryKey, SpeciesState, COUNTY, and STATE. If a character string, must either correspond to the filepath to a CSV or RDATA file containing a table with those variables or the name of the feature class in the geodatabase provided as \code{dsn} with those variables. If \code{NULL}, the function will attempt to find a feature class called POINT (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param POINTCOORDINATES Data frame or character string. If a data frame, must contain the variables PrimaryKey, LocationType, REPORT_LATITUDE, and REPORT_LONGITUDE. If a character string, must either correspond to the filepath to a CSV or RDATA file containing a table with those variables or the name of the feature class in the geodatabase provided as \code{dsn} with those variables. If \code{NULL}, the function will attempt to find a feature class called POINTCOORDINATES (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param GPS Data frame or character string. If a data frame, must contain the variables PrimaryKey, CAPDATE, and ELEVATION. If a character string, must either correspond to the filepath to a CSV or RDATA file containing a table with those variables or the name of the feature class in the geodatabase provided as \code{dsn} with those variables. If \code{NULL}, the function will attempt to find a feature class called GPS (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param ESFSG Data frame or character string. If a data frame, must contain the variables COVERAGE, END_MARK, START_MARK, ESFSG_MLRA, ESFSG_SITE, and ESFSG_STATE (the variable ESFSG_PREFIX is optional). If a character string, must either correspond to the filepath to a CSV or RDATA file containing a table with those variables or the name of the feature class in the geodatabase provided as \code{dsn} with those variables. If \code{NULL}, the function will attempt to find a feature class called ESFSG (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param ... Additional optional filtering expressions passed to
#'   \code{\link[dplyr:filter]{dplyr::filter()}} to restrict the processing and output to only a
#'   subset of input data.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @returns A long-format data frame of header data.
#' @examples
#' # Basic use assuming that all expected feature classes appear in lmf_data.gdb
#' gather_header_lmf(dsn = "data_path/lmf_data.gdb")
#'
#' # Using a filtering expression to restrict the processing and output to only
#' # records where the SpeciesState variable in POINT contained either of the
#' # values "CO" and "NM".
#' gather_header_lmf(dsn = "data_path/lmf_data.gdb",
#'                   SpeciesState %in% c("CO", "NM"))
#'
#' @export
gather_header_lmf <- function(dsn = NULL,
                              POINT = NULL,
                              POINTCOORDINATES = NULL,
                              GPS = NULL,
                              ESFSG = NULL,
                              ...,
                              verbose = FALSE) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  #### Reading #################################################################
  # This is the lookup table of state and county names with LMF numeric codes
  lmf_locale_lookup <- read.csv(file = file.path("data", "lmf_locale_lookup.csv"),
                                stringsAsFactors = FALSE)

  # The basic info about sampling locations
  point <- read_with_fallback(dsn = dsn,
                              tbl = POINT,
                              default_name = "POINT",
                              regex = TRUE,
                              best_guess = TRUE,
                              accept_failure = FALSE,
                              verbose = verbose) |>
    # Filter using the filtering expression specified by user
    dplyr::filter(.data = _,
                  !!!filter_exprs) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(c("PrimaryKey",
                                       "SpeciesState",
                                       "COUNTY",
                                       "STATE")))

  # The coordinates for the sampling locations
  pointcoordinates <- read_with_fallback(dsn = dsn,
                                         tbl = POINTCOORDINATES,
                                         default_name = "POINTCOORDINATES",
                                         regex = TRUE,
                                         best_guess = TRUE,
                                         accept_failure = FALSE,
                                         verbose = verbose) |>
    sf::st_drop_geometry() |>
    dplyr::select(.data = _,
                  PrimaryKey,
                  Latitude_NAD83 = REPORT_LATITUDE,
                  Longitude_NAD83 = REPORT_LONGITUDE,
                  LocationType)

  # The best source for elevation and sampling dates
  gps <- read_with_fallback(dsn = dsn,
                            tbl = GPS,
                            default_name = "GPS",
                            regex = TRUE,
                            best_guess = TRUE,
                            accept_failure = FALSE,
                            verbose = verbose) |>
    dplyr::select(.data = _,
                  PrimaryKey,
                  # The GPS capture date is the best approximation of the
                  # sampling date.
                  DateVisited = CAPDATE,
                  Elevation = ELEVATION) |>
    # Convert elevation to meters
    dplyr::mutate(.data = _,
                  Elevation = Elevation * 0.3048)

  # Ecological site assignments
  esfsg <- read_with_fallback(dsn = dsn,
                              tbl = ESFSG,
                              default_name = "ESFSG",
                              regex = TRUE,
                              best_guess = TRUE,
                              accept_failure = FALSE,
                              verbose = verbose)

  # Add in ESFSG_PREFIX column to old data in order to keep up with LMF schema
  # changes.
  # # The prefix is the "F" or "R" typically found at the beginning of an ecosite
  # ID.
  if(!"ESFSG_PREFIX" %in% colnames(esfsg)) {
    esfsg$ESFSG_PREFIX <- ""
  }

  #### Combining ###############################################################
  # This creates then iterates on output.

  # County and State are referred to by number codes, let's use the names
  output <- dplyr::left_join(x = point,
                             y = lmf_locale_lookup,
                             relationship = "many-to-one",
                             by = c("COUNTY",
                                    "STATE")) |>
    # Pare down to needed fields
    dplyr::select(.data = _,
                  tidyselect::all_of(c("PrimaryKey",
                                       "SpeciesState",
                                       "County",
                                       "State"))) |>
    dplyr::mutate(.data = _,
                  PlotKey = PrimaryKey) |>
    dplyr::distinct() |>
    # Add in the spatial info from POINTCOORDINATES
    dplyr::left_join(x = _,
                     y = pointcoordinates,
                     relationship = "one-to-one",
                     by = "PrimaryKey") |>
    # Add the date and elevation from GPS
    dplyr::left_join(x = _,
                     y = gps,
                     relationship = "one-to-one",
                     by = "PrimaryKey")

  # This isn't part of the pipe chain above just because it's complicated and
  # maintenance will be easier if it's separate
  output <- dplyr::left_join(x = output,
                             y = esfsg,
                             by = "PrimaryKey") |>
    # If the ESD coverage !=all, figure what portion of the plot the dominant
    # ESD is on the plot by taking END_MARK - START_MARK and dividing by the
    # line length.
    dplyr::mutate(.data = _,
                  ESD_coverage = dplyr::if_else(condition = COVERAGE == "all",
                                                true = as.integer(300),
                                                false = (END_MARK - START_MARK)),
                  # LMF schema is being updated to include F/R prefix under the
                  # column ESFSG_PREFIX replace NA's with "" in ESFSG_PREFIX
                  ESFSG_PREFIX = tidyr::replace_na(data = ESFSG_PREFIX,
                                                   replace = ""),
                  EcologicalSiteId = trimws(paste0(ESFSG_PREFIX,
                                                   ESFSG_MLRA,
                                                   ESFSG_SITE,
                                                   ESFSG_STATE)),
                  MLRA = gsub(x = ESFSG_MLRA,
                              pattern = "^$",
                              replacement = NA)) |>
    # Add up the coverage on each plot and get the percent coverage
    dplyr::group_by() |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(c("PrimaryKey",
                                                "EcologicalSiteId")),
                     PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) |>
    # Arrange by ESD_coverage and find the dominant ecological site
    dplyr::group_by(.data = _,
                    PrimaryKey) |>
    dplyr::arrange(.data = _,
                   dplyr::desc(PercentCoveredByEcoSite),
                   .by_group = TRUE) |>
    dplyr::filter(.data = _,
                  dplyr::row_number() == 1) |>
    # Join to the previously-created output to build the final header
    dplyr::left_join(x = output,
                     y = _,
                     relationship = "one-to-one",
                     by = "PrimaryKey") |>
    dplyr::mutate(.data = _,
                  PlotID = PrimaryKey)

  output

  # point <- sf::read_sf(dsn = dsn,
  #                      layer = "POINT",
  #                      quiet = TRUE) |>
  #   sf::st_drop_geometry(x = _) |>
  #   # Filter using the filtering expression specified by user
  #   dplyr::filter(.data = _,
  #                 !!!filter_exprs) |>
  #   dplyr::select(.data = _,
  #                 tidyselect::all_of(c("PrimaryKey",
  #                                      "SpeciesState",
  #                                      "COUNTY",
  #                                      "STATE")))
  #
  #
  #
  #
  #
  #
  # # Get the field coordinates
  # point_coordinate <- sf::st_read(dsn = dsn,
  #                                 layer = "POINTCOORDINATES",
  #                                 stringsAsFactors = FALSE,
  #                                 quiet = TRUE) |>
  #   sf::st_drop_geometry() |>
  #   dplyr::select(.data = _,
  #                 PrimaryKey,
  #                 Latitude_NAD83 = REPORT_LATITUDE,
  #                 Longitude_NAD83 = REPORT_LONGITUDE,
  #                 LocationType) |>
  #   dplyr::left_join(x = point,
  #                    y = _,
  #                    relationship = "one-to-one",
  #                    by = "PrimaryKey")
  #
  # # Add elevation data
  # point_elevation <- sf::read_sf(dsn = dsn,
  #                                layer = "GPS",
  #                                quiet = TRUE) |>
  #   dplyr::select(.data = _,
  #                 PrimaryKey,
  #                 # The GPS capture date is the best approximation of the
  #                 # sampling date.
  #                 DateVisited = CAPDATE,
  #                 Elevation = ELEVATION) |>
  #   dplyr::left_join(x = point_coordinate,
  #                    y = _,
  #                    relationship = "one-to-one",
  #                    by = "PrimaryKey") |>
  #   # Convert elevation to meters
  #   dplyr::mutate(.data = _,
  #                 Elevation = Elevation * 0.3048)
  #
  # # Add Ecological Site Id
  # point_ESD_raw <- sf::st_read(dsn = dsn,
  #                              layer = "ESFSG",
  #                              stringsAsFactors = FALSE,
  #                              quiet = TRUE)
  #
  # # Add in ESFSG_PREFIX column to old data in order to keep up with LMF schema
  # # changes.
  #
  # if(!"ESFSG_PREFIX" %in% colnames(point_ESD_raw)) {
  #   point_ESD_raw$ESFSG_PREFIX <- ""
  # }
  #
  # point_ESD <- dplyr::left_join(x = point_elevation,
  #                               y = point_ESD_raw,
  #                               by = "PrimaryKey") |>
  #   # If the ESD coverage !=all, figure what portion of the plot the dominant
  #   # ESD is on the plot by taking END_MARK - START_MARK and dividing by the
  #   # line length.
  #   dplyr::mutate(.data = _,
  #                 ESD_coverage = dplyr::if_else(condition = COVERAGE == "all",
  #                                               true = as.integer(300),
  #                                               false = (END_MARK - START_MARK)),
  #                 # LMF schema is being updated to include F/R prefix under the
  #                 # column ESFSG_PREFIX replace NA's with "" in ESFSG_PREFIX
  #                 ESFSG_PREFIX = tidyr::replace_na(data = ESFSG_PREFIX,
  #                                                  replace = ""),
  #                 EcologicalSiteId = trimws(paste0(ESFSG_PREFIX,
  #                                                  ESFSG_MLRA,
  #                                                  ESFSG_SITE,
  #                                                  ESFSG_STATE)),
  #                 MLRA = gsub(x = ESFSG_MLRA,
  #                             pattern = "^$",
  #                             replacement = NA)) |>
  #   # Add up the coverage on each plot and get the percent coverage
  #   dplyr::group_by() |>
  #   dplyr::summarize(.data = _,
  #                    .by = tidyselect::all_of(c("PrimaryKey",
  #                                               "EcologicalSiteId")),
  #                    PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) |>
  #   # Arrange by ESD_coverage and find the dominant ecological site
  #   dplyr::group_by(.data = _,
  #                   PrimaryKey) |>
  #   dplyr::arrange(.data = _,
  #                  dplyr::desc(PercentCoveredByEcoSite),
  #                  .by_group = TRUE) |>
  #   dplyr::filter(.data = _,
  #                 dplyr::row_number() == 1) |>
  #   # Join to point.elevation to build the final header
  #   dplyr::left_join(x = point_elevation,
  #                    y = _,
  #                    relationship = "one-to-one",
  #                    by = "PrimaryKey") |>
  #   dplyr::mutate(.data = _,
  #                 PlotID = PrimaryKey)
  #
  # point_ESD
}


#' Gather NRI plot-level header data
#' @description This reads in metadata from NRI sampling used as headers for
#' various methods and returns it as a long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}.

# #' @export
gather_header_nri <- function(dsn = NULL,
                              speciesstate,
                              ...,
                              verbose = FALSE) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # if(!is.null(POINT)){
  #   point <- POINT
  # } else if(!is.null(dsn)){
  point <- read.csv(file.path(dsn, "POINT.csv"), stringsAsFactors = FALSE)
  # } else {
  #   stop("Provide either POINT or a path to a folder containing it")
  # }

  # Get the LMF points
  point <- point %>%
    # remove spatial attributes
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    # dplyr::filter(!!!filter_exprs) %>%
    dplyr::select(
      PrimaryKey,
      COUNTY, STATE#, DBKey
    )

  # County and State are referred to by number codes, let's use the name
  point <- read.csv(file.path(dsn, "COUNTYNM.csv"), stringsAsFactors = FALSE) %>%
    dplyr::select(COUNTY, COUNTYNM, STATE) %>%
    dplyr::left_join(point, .,
                     by = c("COUNTY", "STATE")
    ) %>%
    dplyr::distinct() %>%


    # Add state
    dplyr::left_join(read.csv(file.path(dsn, "STATENM.csv"), stringsAsFactors = FALSE),
                     # by = c("STATE", "DBKey")
                     by = "STATE"
    ) %>%

    # pair down to needed fields
    dplyr::select(
      PrimaryKey,
      # DBKey,
      County = COUNTYNM,
      State = STABBR
    ) %>%
    dplyr::distinct() #%>%

  # Populate DateLoadedInDB
  # dplyr::mutate(DateLoadedInDB = DBKey)

  # Get the field coordinates
  point_coordinate <- read.csv(file.path(dsn, "POINTCOORDINATES.csv"),
                               stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      Latitude_NAD83 = dplyr::coalesce(
        FIELD_LATITUDE,
        TARGET_LATITUDE
      ),
      Longitude_NAD83 = dplyr::coalesce(
        FIELD_LONGITUDE,
        TARGET_LONGITUDE
      ),
      LocationType = dplyr::if_else(Latitude_NAD83 == TARGET_LATITUDE, "Target", "Field")
    ) %>%
    dplyr::select(
      PrimaryKey,
      Latitude_NAD83,
      Longitude_NAD83,
      LocationType
    ) %>%
    dplyr::left_join(point, .,
                     by = "PrimaryKey"
    )

  # Add elevation data
  point_elevation <- read.csv(file.path(dsn, "GPS.csv"),
                              stringsAsFactors = FALSE
  ) %>%
    dplyr::select(PrimaryKey,
                  DateVisited = CAPDATE, # The GPS capture date is the best approx
                  ELEVATION#,
                  # DBKey
    ) %>%
    dplyr::left_join(point_coordinate, .,
                     # by = c("PrimaryKey", "DBKey")
                     by = "PrimaryKey"
    ) %>%

    # Convert elevation to meters
    dplyr::mutate(ELEVATION = ELEVATION * 0.3048)

  # Add Ecological Site Id
  point_ESD <- read.csv(file.path(dsn, "ESFSG.csv"),
                        stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(point_elevation, .,
                     # by = c("PrimaryKey", "DBKey")
                     by = "PrimaryKey") %>%
    dplyr::distinct() %>%

    # If the ESD coverage !=all, figure what portion of the plot the dominant ESD
    # is ion the plot by taking the End_Mark-Start_Mark and dividng by the line length
    dplyr::mutate(
      ESD_coverage =
        dplyr::if_else(
          condition = COVERAGE == "all",
          true = as.integer(300),
          false = (END_MARK - START_MARK)
        ),
      EcologicalSiteId =
        paste(ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE, sep = "")
    ) %>%

    # Add up the coverage on each plot and get the percent coverage
    dplyr::group_by(PrimaryKey,
                    # DBKey,
                    EcologicalSiteId) %>%
    dplyr::summarise(PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) %>%

    # Arrange by ESD_coverage and find the dominant ecological site
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey#,
                    # DBKey
    ) %>%
    dplyr::arrange(dplyr::desc(PercentCoveredByEcoSite), .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

    # Join to point.elevation to build the final header
    dplyr::left_join(point_elevation, .,
                     # by = c("PrimaryKey", "DBKey")
                     by = "PrimaryKey") %>%
    dplyr::distinct()

  # get a vector of which rows need prefixing
  point_ESD <- point_ESD %>% dplyr::mutate(
    EcologicalSiteId = dplyr::case_when(stringr::str_detect(point_ESD$EcologicalSiteId, "^[0-9]") ~
                                          paste0("R", EcologicalSiteId),
                                        TRUE ~ EcologicalSiteId),
    EcologicalSiteId = stringr::str_trim(EcologicalSiteId)
  )

  # Attach SpeciesState
  point_ESD$SpeciesState <- speciesstate

  # Create PlotID, which is needed in later functions
  point_ESD <- point_ESD %>% dplyr::mutate(PlotID = PrimaryKey)

  # Return the point_ESD as the header file
  return(point_ESD)
}

# # Build the header portion of the Survey123 table
# #' export gather_header_survey123
# #' rdname aim_gdb
# gather_header_survey123 <- function(PlotChar, speciesstate, ...){
#     # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
#     filter_exprs <- rlang::quos(...)
#
#     header <- PlotChar %>%
#       as.data.frame() %>%
#
#       # Filter using the filtering expression specified by user
#       dplyr::filter(!!!filter_exprs)
#
#     # Add these fields, to match terradat formatting
#     header$Design <- NA
#     header$DesignFlag <- NA
#     header$Purpose <- NA
#     header$PurposeFlag <- NA
#     header$ProjectName <- NA
#     header$State <- NA
#     header$DBKey <- NA
#     header$County <- NA
#     header$DateLoadedInDb <- NA
#     header$SpeciesState <- speciesstate
#     header$PrimaryKey <- header$PlotKey
#
#
#     header <- header %>%
#       # Select the field names we need in the final feature class
#       dplyr::select(PrimaryKey, PlotID, PlotKey, DBKey, DateVisited = DateFormat,
#                     EcologicalSiteId = Ecolsite, Latitude_NAD83 = y, Longitude_NAD83 = x, State, SpeciesState,
#                     County, DateEstablished = EstabDate,
#                     DateLoadedInDb,
#                     Design, DesignFlag, Purpose, PurposeFlag
#       ) %>%
#
#       # If there are any Sites with no PrimaryKeys, delete them
#       subset(!is.na(PrimaryKey))
#
#     # alert to  duplicate primary keys
#     dupkeys <- header$PrimaryKey[duplicated(header$PrimaryKey)]
#     if(length(dupkeys) > 0){
#       dupnames <- paste(unique(dupkeys), collapse = ", ")
#       warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#     }
#
#     # Return the header file
#     return(header)
# }


#' Gather plot-level header data
#' @description This is a wrapper function for specialized functions which read
#' in metadata from AIM sampling used as headers for various methods and returns
#' it as a long-format data frame suitable for use in indicator calculations
#' with the package \code{terradactyl}. Supported formats include those used in
#' AIM (Assessment, Inventory, and Monitoring) and TerrADat (Terrestrial AIM
#' Database), LMF (Landscape Monitoring Framework), and DIMA (the Database for
#' Inventory, Monitoring, and Assessment). For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_header_terradat]{gather_header_terradat()}}, \code{\link[terradactyl:gather_header_lmf]{gather_header_lmf()}}, and
#' \code{\link[terradactyl:gather_header_nri]{gather_header_nri()}}.
#'
#' @inheritParams gather_header_terradat
#' @inheritParams gather_header_lmf
#' @param source Character string. This specifies the expected data format(s)
#'   and determines which specialized gather function will be used. It must be
#'   one of \code{"AIM"}, \code{"TERRADAT"}, \code{"LMF"}, \code{"DIMA"} or
#'   \code{"NRI"}. This is case-insensitive.
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblPlots} and \code{date_tables} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the arguments \code{POINT}, \code{POINTCOORDINATES}, \code{GPS}, and \code{ESFSG} are all considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#'
#' @param speciesstate Optional. Used by NRI.
# #' @param autoQC Logical (currently disabled). If \code{TRUE} then automatic
# #'   quality control functions will be applied to the data before returning the
# #'   output. Defaults to \code{FALSE}.
# #' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
# #'   messages. Defaults to \code{FALSE}.
#'
#' @returns A long-format data frame of header data.
#' @examples
#' # Headers from a geodatabase in the format of the Terrestrial AIM Database
#' gather_header(dsn = "data_path/aim_data.gdb",
#'               source = "terradat")
#'
#' # Headers from a geodatabase in the format of the Landscape Monitoring Framework
#' gather_header(dsn = "data_path/lmf_data.gdb",
#'               source = "lmf")
#'
#' @export
#'
gather_header <- function(dsn = NULL,
                          source,
                          tblPlots = NULL,
                          POINT = NULL,
                          POINTCOORDINATES = NULL,
                          GPS = NULL,
                          ESFSG = NULL,
                          date_tables = NULL,
                          speciesstate = NULL,
                          ...,
                          autoQC = FALSE,
                          verbose = FALSE) {
  # Error check
  # Check for a valid source
  try(if (!(toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI"))) {
    stop("No valid source provided")
  })

  # Apply appropriate header function

  if (toupper(source) %in% c("TERRADAT", "AIM", "DIMA")) {
    output <- gather_header_terradat(dsn = dsn,
                                     tblPlots = tblPlots,
                                     date_tables = date_tables,
                                     ...,
                                     verbose = verbose)
  } else if (toupper(source) == "LMF") {
    output <- gather_header_lmf(dsn = dsn,
                                POINT = POINT,
                                POINTCOORDINATES = POINTCOORDINATES,
                                GPS = GPS,
                                ESFSG = ESFSG,
                                ...,
                                verbose == verbose)
  } else if (toupper(source) == "NRI") {
    output <- gather_header_nri(dsn = dsn,
                                speciesstate = speciesstate,
                                ...)
  } else {
    stop("No valid source provided")
  }

  # Add the source value to the data.
  output$source <- source

  # Make sure there's no geometry associated with the data.
  if("sf" %in% class(output)) {
    output <- sf::st_drop_geometry(x = output)
  }

  # Apply QC helper functions to remove duplicates
  # if(autoQC){
  #   message("Checking for duplicated rows. Disable by adding the parameter 'autoQC = FALSE'")
  #   header <- tdact_remove_duplicates(header)
  # }

  output
}


#### LINE-POINT INTERCEPT ######################################################
#' Function to convert TerrADat-format LPI data into a long format.
#' @description This reads in Line-Point Intercept from AIM sampling and returns it as a long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}.The expected format is that used in the Terrestrial AIM
#' Database (TerrADat).
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param tblLPIDetail Optional data frame or character string. If provided, this must contain the
#'   expected LPI data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblLPIDetail (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param tblLPIHeader Optional data frame or character string. If provided, this must contain the
#'   expected metadata for the LPI data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblLPIHeader (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param auto_qc_warnings Logical. If \code{TRUE} the function will test the
#'   data and metadata for duplicated records (records which are identical to
#'   each other across all critical variables) and orphaned records (records
#'   which appear in the data but do not have any corresponding metadata
#'   records), either of which can result in erroneous or unexpected outputs.
#'   The number of duplicated and orphaned records will be reported but without
#'   making any changes. Defaults to \code{TRUE}.
#' @param null_is_na Logical. If \code{TRUE} then records containing \code{NULL}
#'   will be included in those considered non-records and dropped. If
#'   \code{FALSE} then only records with any of the values in \code{c(NA, "N",
#'   "None", "")} will be dropped. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#'
#' @examples
#' # Using a geodatabase that contains the tables tblLPIHeader and tblLPIDetail
#' gather_lpi_terradat(dsn = "data_folder/aim_data.gdb")
#'
#' # Using CSV files for tblLPIHeader and tblLPIDetail.
#' # These two arguments can also take data frames, if the data have already
#' # been read in as data frames.
#' gather_lpi_terradat(tblLPIHeader = "data_folder/lpi_headers.csv",
#'                     tblLPIDetail = "data_folder/lpi_detail_records.csv")
#'
#' # Using data frames for tblLPIHeader and tblLPIDetail
#' aim_lpiheader <- read.csv("data_folder/lpi_headers.csv")
#' aim_lpidetail <- read.csv("data_folder/lpi_detail_records.csv")
#' gather_lpi_terradat(tblLPIHeader = aim_lpiheader,
#'                     tblLPIDetail = aim_lpidetail)
#'
#' @export
gather_lpi_terradat <- function(dsn = NULL,
                                tblLPIDetail = NULL,
                                tblLPIHeader = NULL,
                                auto_qc_warnings = TRUE,
                                null_is_na = TRUE,
                                verbose = FALSE) {

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
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")

  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblLPIHeader,
                               default_name = "tblLPIHeader",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()

  detail <- read_with_fallback(dsn = dsn,
                               tbl = tblLPIDetail,
                               default_name = "tblLPIDetail",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()


  # # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  # if (!is.null(tblLPIDetail) & !is.null(tblLPIHeader)) {
  #   if (verbose) {
  #     if (!is.null(dsn)) {
  #       message("Using the provided data frames. The provided dsn value is being ignored.")
  #     }
  #   }
  #   detail <- tblLPIDetail
  #   header <- tblLPIHeader
  # } else if(!is.null(dsn)){
  #   if (verbose) {
  #     message("Attempting to use the provided dsn value.")
  #   }
  #   if(!file.exists(dsn)){
  #     stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
  #   }
  #   # The suppressWarnings() here are so that it doesn't complain about pulling
  #   # tables without geometry. We know that's what should be happening.
  #   detail <- suppressWarnings(sf::st_read(dsn = dsn,
  #                                          layer = "tblLPIDetail",
  #                                          stringsAsFactors = FALSE,
  #                                          quiet = TRUE))
  #   header <- suppressWarnings(sf::st_read(dsn = dsn,
  #                                          layer = "tblLPIHeader",
  #                                          stringsAsFactors = FALSE,
  #                                          quiet = TRUE))
  # } else {
  #   stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing tables with those names.")
  # }
  #
  # # Clean these up!
  # detail <- dplyr::select(.data = detail,
  #                         -tidyselect::any_of(internal_gdb_vars)) |>
  #   dplyr::distinct()
  #
  # header <- dplyr::select(.data = header,
  #                         -tidyselect::any_of(internal_gdb_vars)) |>
  #   dplyr::distinct()

  #### Automatic QC ############################################################
  if (auto_qc_warnings) {
    if (verbose) {
      message("Running automatic QC checks for duplicated or orphaned records.")
    }
    auto_qc_warning(header_data = header,
                    detail_data = detail,
                    uid_variables = list(header = c("PrimaryKey",
                                                    "RecKey"),
                                         detail = c("PrimaryKey",
                                                    "RecKey",
                                                    "PointNbr")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }

  # These are values which are considered to be non-records in the data and are
  # used to filter during the pivot to a long/tall format.
  nonrecord_values <- c(NA, "N", "None", "")
  if (null_is_na) {
    nonrecord_values <- c(nonrecord_values,
                          "NULL")
  }

  #### Munging #################################################################
  # Make a tall data frame with the hit codes by layer.
  lpi_hits_tall <- dplyr::mutate(.data = detail,
                                 # Make sure we don't have factors in play.
                                 dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                               .fns = ~ as.character(.x))) |>
    # Get just the variables we need for this particular data frame.
    dplyr::select(.data = _,
                  PrimaryKey, RecKey,
                  PointLoc, PointNbr,
                  ShrubShape,
                  TopCanopy,
                  tidyselect::matches(match = "^Lower\\d$"),
                  SoilSurface) |>
    # Pivot it to be long (tall, in the older terminology).
    tidyr::pivot_longer(data = _,
                        # All those messy lower layer variable names are
                        # sandwiched between TopCanopy and SoilSurface, which
                        # makes it easy to do this and still snag them all.
                        cols = TopCanopy:SoilSurface,
                        names_to = "layer",
                        values_to = "code") |>
    # Dropping the values we can't work with because no valid hit was recorded.
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey),
                  !is.na(RecKey),
                  !code %in% nonrecord_values) |>
    dplyr::distinct(.data = _)

  # We want to handle the checkboxes, but sometimes they're not present because
  # the data we're handling comes from sources like the New Mexico Department of
  # Transportation which doesn't use those features.

  # If there are checkbox variables, we'll make those data tall and then join
  # them to lpi_hits_tall to create lpi_tall.
  # If there aren't checkbox variables, we'll just make lpi_tall from
  # lpi_hits_tall.
  # The code for handling checkbox variables should work regardless of how many
  # variables it finds as long as there's at least one.
  has_checkbox_variables <- any(stringr::str_detect(string = names(detail),
                                                    # This regex looks for a
                                                    # string that starts with
                                                    # "Chkbox" and ends in a
                                                    # known suffix, e.g.
                                                    # "ChkboxTop" or
                                                    # "ChkboxLower5"
                                                    pattern = "^Chkbox(Top|Soil|Lower\\d)$"))

  if (has_checkbox_variables) {
    # Make a tall data frame the checkbox status by layer.
    lpi_tall <- dplyr::mutate(.data = detail,
                              # Make sure we don't have factors in play.
                              dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                            .fns = ~ as.character(.x))) |>
      # Get just the variables we need for this particular data frame.
      dplyr::select(.data = _,
                    PrimaryKey, RecKey,
                    PointLoc, PointNbr,
                    # We only want the chkbox values for the top, soil, and
                    # numbered layers. This will exclude the woody and herbaceous
                    # ones.
                    tidyselect::matches(match = "^Chkbox(Top|Soil|Lower\\d)$")) |>
      # Pivot it to be long (tall, in the older terminology).
      tidyr::pivot_longer(data = _,
                          cols = tidyselect::matches(match = "^Chkbox"),
                          names_to = "layer",
                          # Removing the prefix so that these values will match
                          # the ones in our other tall data frame created above.
                          names_prefix = "^Chkbox",
                          values_to = "chckbox") |>
      # Keep only the records with valid checkbox values.
      dplyr::filter(.data = _,
                    chckbox %in% c(1, 0)) |>
      # And adjusting so that the non-numbered layers match those values we expect
      dplyr::mutate(.data = _,
                    layer = dplyr::case_when(layer == "Top" ~ "TopCanopy",
                                             layer == "Soil" ~ "SoilSurface",
                                             .default = layer)) |>
      dplyr::distinct(.data = _) |>
      dplyr::left_join(x = lpi_hits_tall,
                       y = _,
                       # relationship = "one-to-one",
                       by = c("PrimaryKey", "RecKey",
                              "PointLoc", "PointNbr",
                              "layer"))
  } else {
    warning("There were no 'checkbox' variables (usually used to record whether a record represents a live or dead plant part) in the provided data. If you were expecting them, please check your data.")
    lpi_tall <- lpi_hits_tall
  }

  # Print update because this function can take a while
  if (verbose) {
    message("Merging the header and detail tables")
  }

  # Join the header information to the tall data.
  # The suppressWarnings() and lack of defined relationships in the joins are to
  # allow the user to run this with data that have not been adequately cleaned.
  lpi_tall <- dplyr::select(.data = header,
                            # This is split so that we grab the first chunk of
                            # assumed-to-be-present metadata variables and then
                            # if we've got the checkbox ones we'll do those too
                            LineKey:HeightUOM,
                            tidyselect::any_of(c("ShowCheckbox",
                                                 "CheckboxLabel")),
                            PrimaryKey) |>
    dplyr::left_join(x = _,
                     y = lpi_tall,
                     # relationship = "one-to-many",
                     by = c("PrimaryKey", "RecKey")) |>
    suppressWarnings()

  # We want to coerce dates into character strings.
  # Find variables with a date class.
  date_vars <- which(sapply(X = names(lpi_tall),
                            lpi_tall = lpi_tall,
                            FUN = function(X, lpi_tall){
                              any(c("POSIXct", "POSIXt") %in% class(lpi_tall[[X]]))
                            }))
  # And then coerce the date values in those variables to strings.
  lpi_tall <- dplyr::mutate(.data = lpi_tall,
                            dplyr::across(.cols = tidyselect::all_of(date_vars),
                                          .fns = as.character))


  # Remove some of the header variables
  lpi_tall <- dplyr::select(.data = lpi_tall,
                            -tidyselect::any_of(c("DateModified",
                                                  "FormType",
                                                  "DataEntry",
                                                  "DataErrorChecking",
                                                  "DateVisited")))


  lpi_tall
}

#' Function to convert LMF-format LPI data into a long format.
#' @description This reads in Line-Point Intercept from LMF sampling and returns it as a long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}.The expected format is that used in the Landscape Monitoring Framework (LMF).
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param PINTERCEPT Optional data frame or character string. If provided, this must contain the
#'   expected LPI data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called PINTERCEPT (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param file_type Deprecated. This argument is no longer functional or necessary and is kept for backwards compatibility with legacy code.
#' @param verbose Logical. If \code{TRUE} then the function will report back diagnostic
#'   information as console messages while it works. Defaults to \code{FALSE}.
#'
#' @examples
#' # Using a geodatabase that contains the table PINTERCEPT.
#' gather_lpi_lmf(dsn = "data_folder/lmf_data.gdb")
#'
#' # Using a CSV file for PINTERCEPT.
#' # This argument can also take data frames, if the data have already been read
#' # in as a data frame.
#' gather_lpi_lmf(PINTERCEPT = "data_folder/lpi_detail_records.csv")
#'
#'
#' @export

gather_lpi_lmf <- function(dsn = NULL,
                           PINTERCEPT = NULL,
                           file_type = "gdb",
                           verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType",
                         "DBKey")

  #### Reading and cleanup #####################################################
  pintercept <- read_with_fallback(dsn = dsn,
                                   tbl = PINTERCEPT,
                                   default_name = "PINTERCEPT",
                                   regex = TRUE,
                                   best_guess = TRUE,
                                   accept_failure = FALSE,
                                   verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(PINTERCEPT)) {
    pintercept <- PINTERCEPT
  } else if (!is.null(dsn)) {
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath.")
    }
    file_type <- tools::file_ext(x = dsn) |>
      tolower()
    if (!(file_type %in% c("gdb", "csv", "txt"))) {
      stop("dsn must end in one of the following file extensions: gdb, csv, txt.")
    }
    # Read  PINTERCEPT table in .txt or .gdb or from a preformatted csv
    pintercept <- switch(file_type,
                         "gdb" = {
                           sf::st_read(dsn = dsn,
                                       layer = "PINTERCEPT",
                                       stringsAsFactors = FALSE,
                                       quiet = TRUE)
                         },
                         "txt" = {
                           utils::read.table(file = file.path(dsn,
                                                              "pintercept.txt"),
                                             stringsAsFactors = FALSE,
                                             strip.white = TRUE,
                                             header = FALSE,
                                             sep = "|")
                         },
                         "csv" = {
                           read.csv(file = dsn,
                                    header = TRUE,
                                    stringsAsFactors = FALSE)
                         })

    # if it is in a text file, there are no field names assigned.
    if (file_type == "txt") {
      colnames <- terradactyl::nri.data.column.explanations$FIELD.NAME[
        terradactyl::nri.data.column.explanations$TABLE.NAME == "PINTERCEPT"
      ]

      colnames <- colnames[1:ncol(pintercept)] |> subset(!is.na(.))
      names(pintercept) <- colnames
    }
  } else {
    stop("Supply one of the following: a data frame as the argument PINTERCEPT, the path to a GDB (containing a table called PINTERCEPT) as the argument dsn, or the path to a folder containing a file called 'pintercept.txt' as the argument dsn.")
  }

  # Sometimes NAs might be introduced as variable names, but we can make sure to
  # drop those.
  pintercept <- pintercept[, !is.na(colnames(pintercept))]

  ##### Dealing with intersecting transects ------------------------------------
  # The arrangement of the transects for an LMF point crosses in the middle of
  # the transects (point 75) and so that intersection gets recorded twice, once
  # per transect. We'll drop the 75th record on the northeast-southwest transect
  # but we also want to warn the user that it's happening to any situations
  # where the assumption that they're identical is violated.
  duplicated_75mark_indices <- dplyr::filter(.data = pintercept,
                                             MARK == 75) |>
    dplyr::select(.data = _,
                  -TRANSECT) |>
    duplicated(x = _)
  pintercept_75mark <- dplyr::filter(.data = pintercept,
                                     MARK == 75)
  pintercept_75mark[["duplicated"]] <- duplicated_75mark_indices
  pintercept_75mark_summary <- dplyr::summarize(.data = pintercept_75mark,
                                                .by = PrimaryKey,
                                                n_records = dplyr::n(),
                                                has_duplicate = any(duplicated))
  if (any(!pintercept_75mark_summary$has_duplicate)) {
    warning(paste0("There are ", sum(!pintercept_75mark_summary$has_duplicate),
                   " plots where the LPI records at the 75th sampling locations on the two transects are not identical to each other despite being the intersection of those transects. The records associated with the 'nesw' transects will still be dropped for these plots."))
  }

  pintercept <- dplyr::filter(.data = pintercept,
                              !(TRANSECT == "nesw" & MARK == 75))

  ##### Harmonizing with expected values and variable names --------------------
  # We don't need all the extra LMF-internal variables after this point, so
  # we'll pare down and rename here.
  pintercept <- dplyr::select(.data = pintercept,
                              PrimaryKey,
                              LineKey = TRANSECT,
                              PointNbr = MARK,
                              tidyselect::matches(match = "^HIT\\d$"),
                              BASAL,
                              NONSOIL,
                              ShrubShape = SAGEBRUSH_SHAPE)

  # The way soil surface hits are recorded needs to be standardized.
  pintercept <- dplyr::mutate(.data = pintercept,
                              # When there's no basal hit recorded and
                              # there's nothing in the NONSOIL variable,
                              # that means that there was a surface hit on
                              # soil. Otherwise we'll turn all the empty values
                              # to NA to make it easier to merge BASAL and
                              # NONSOIL into a single SoilSurface variable.
                              NONSOIL = dplyr::case_when(BASAL %in% c("None") & NONSOIL %in% c("", NA) ~ "S",
                                                         NONSOIL %in% c("") ~ NA,
                                                         .default = NONSOIL),
                              # If a basal hit was recorded over a NONSOIL
                              # value of "BR" then the BASAL value should
                              # actually be NA. Also, all "None" values should
                              # be NA for the next step. Otherwise, leave the
                              # BASAL values alone.
                              BASAL = dplyr::case_when(!(BASAL %in% c("None")) & NONSOIL %in% c("BR") ~ NA,
                                                       BASAL %in% c("None") ~ NA,
                                                       .default = BASAL)) |>
    # The BASAL and NONSOIL variables can be combined into SoilSurface and
    # dropped.
    dplyr::mutate(.data = _,
                  SoilSurface = dplyr::coalesce(BASAL,
                                                NONSOIL)) |>
    dplyr::select(.data = _,
                  -BASAL,
                  -NONSOIL)

  # We also need to standardize the way that sagebrush shrub shapes are recorded
  # because they're numeric values but we need them to be characters which
  # represent the shape where 1 means columnar, 2 means spreading, 3 means
  # mixed, and 0 means there was no shape to record.
  pintercept <- dplyr::mutate(.data = pintercept,
                              # I don't totally trust that this'll be numeric,
                              # so we'll check for a numeric and a character.
                              ShrubShape = dplyr::case_when(ShrubShape %in% c(1, "1") ~ "C",
                                                            ShrubShape %in% c(2, "2") ~ "S",
                                                            ShrubShape %in% c(3, "3") ~ "M",
                                                            .default = NA))

  # We need to rename the HIT variables to match the expectations.
  # Get the appropriate variable names
  hit_layer_rename_vector <- stringr::str_extract(string = names(pintercept),
                                                  pattern = "^HIT\\d+$") |>
    na.omit(object = _)
  # Make sure that they're in ascending order and have the new names associated.
  hit_layer_rename_vector <- hit_layer_rename_vector[stringr::str_extract(string = hit_layer_rename_vector,
                                                                          pattern = "\\d+") |>
                                                       as.numeric() |>
                                                       order()] |>
    setNames(object = _,
             nm = c("TopCanopy",
                    paste0("Lower",
                           seq_len(length(hit_layer_rename_vector) - 1))))
  # And use that vector to rename the variables.
  pintercept <- dplyr::rename(.data = pintercept,
                              tidyselect::all_of(hit_layer_rename_vector))

  # Make sure that we don't have any dates as POSITX values and coerce them into
  # character. This shouldn't be necessary, but just in case!
  pintercept <- dplyr::mutate(.data = pintercept,
                              dplyr::across(.cols = tidyselect::where(fn = ~ class(.x) %in% c("POSIXct", "POSIXt")),
                                            .fns = as.character))

  #### Pivoting to long ########################################################
  data_tall <- tidyr::pivot_longer(data = pintercept,
                                   cols = tidyselect::all_of(c(names(hit_layer_rename_vector),
                                                               "SoilSurface")),
                                   names_to = "layer",
                                   values_to = "code") |>
    # Remove all the non-records!
    dplyr::filter(.data = _,
                  !(code %in% c("")))

  # Return only the unique records remaining!
  dplyr::distinct(data_tall)
}

# Gather LPI data from NPS I&M networks
# currently not used
gather_lpi_nps <- function(dsn,
                           verbose = FALSE) {
  lpi_raw <- read.csv(dsn) |>

    # add plot metadata
    dplyr::mutate(.data = _,
                  PrimaryKey = Unit_Plot,
                  DBKey = dsn,
                  FormDate = Start_Date,
                  LineLengthAmount = 50,
                  SpacingIntervalAmount = 0.5,
                  SpacingType = "m",
                  PointNbr = Point,
                  LineKey = Transect,
                  RecKey = paste(Unit_Plot, Visit_Year,
                                 Transect,
                                 sep = "_"
                  )
    )
}

# #' export gather_lpi_survey123
# #' rdname gather_lpi
# gather_lpi_survey123 <- function(dsn = NULL,
#                                  LPI_0 = NULL,
#                                  LPIDetail_1 = NULL) {
#
#   # This code copy-and-paste from terradat gather
#   lpi_detail <- LPIDetail_1
#   lpi_header <- LPI_0
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- lpi_header$PlotKey[duplicated(lpi_header$PlotKey) & duplicated(lpi_header$LineKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Add null DBKey column if not present
#   if(!("DBKey" %in% colnames(lpi_header))) lpi_header$DBKey <- NA
#   if(!("DBKey" %in% colnames(lpi_detail))) lpi_detail$DBKey <- NA
#
#   # Survey123 uses GlobalID to join, and PlotKey becomes PrimaryKey
#   lpi_header$PrimaryKey <- lpi_header$PlotKey
#
#   lpi_detail <- dplyr::left_join(
#     x = lpi_detail,
#     y = lpi_header %>% dplyr::select(PrimaryKey, GlobalID),
#     by = c("ParentGlobalID" = "GlobalID")
#   ) %>%
#     dplyr::select(-"ParentGlobalID", -"GlobalID")
#
#
#   # Make a tall data frame with the hit codes by layer and the checkbox designation
#   lpi_hits_tall <- lpi_detail %>%
#     dplyr::mutate_if(is.factor, as.character) %>%
#     dplyr::select(
#       "PrimaryKey",
#       "PointLoc",
#       "PointNbr",
#       "RecKey",
#       "ShrubShape",
#       "TopCanopy",
#       "SoilSurface", dplyr::matches("^Lower")
#     ) %>%
#     tidyr::gather(
#       key = "layer",
#       value = "code",
#       "TopCanopy", "SoilSurface", dplyr::matches("^Lower")
#     )
#
#   # Remove all records where no hit was recorded (e.g., "None", "NA"
#
#   lpi_hits_tall <- dplyr::filter(
#     .data = lpi_hits_tall,
#     !is.na(code),
#     code != "",
#     code != "N",
#     code != "None",
#     !is.na(PrimaryKey),
#     !is.na(RecKey)
#   )
#
#
#   ## Make a tall data frame the checkbox status by layer
#
#   lpi_chkbox_tall <- lpi_detail %>%
#     dplyr::select(
#       "PrimaryKey",
#       "PointLoc",
#       "PointNbr",
#       "RecKey",
#       dplyr::matches("^Chkbox")
#     ) %>%
#     tidyr::gather(
#       key = "layer",
#       value = "chckbox",
#       dplyr::matches("^Chkbox")
#     )
#
#   # Remove Woody and Herbaceous Checkbox
#   lpi_chkbox_tall <- lpi_chkbox_tall[!(lpi_chkbox_tall$chckbox %in%
#                                          c(
#                                            "ChckboxWoody",
#                                            "ChckboxHerbaceous"
#                                          )), ]
#
#   ## Make the names in the layer variable match
#   lpi_chkbox_tall$layer <- gsub(lpi_chkbox_tall$layer,
#                                 pattern = "^Chkbox",
#                                 replacement = ""
#   )
#
#   lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Top"] <- "TopCanopy"
#   lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Soil"] <- "SoilSurface"
#
#   # Print update because this function can take a while
#   message("Merging LPI Header and LPI Detail tables")
#
#   # Merge checkbox and hit data as well as the header data
#   lpi_tall <- dplyr::left_join(
#     x = lpi_hits_tall,
#     y = lpi_chkbox_tall,
#     by = c("PrimaryKey", "PointLoc", "PointNbr", "RecKey", "layer")
#   ) %>%
#     dplyr::left_join(
#       x =
#
#         dplyr::select(
#         lpi_header,
#         "PrimaryKey",
#         "LineKey",
#         "DBKey",
#         "FormDate",
#         "Direction"#,
#         # "Measure", # Missing but necessary data
#         # "LineLengthAmount",
#         # "SpacingIntervalAmount",
#         # "SpacingType",
#         # "HeightOption",
#         # "HeightUOM",
#         # "ShowCheckBox",
#         # "CheckboxLabel",
#       ),
#       y = .,
#       by = c("PrimaryKey")
#     )
#
#   # Find date fields & convert to character
#   # Find fields that are in a Date structure
#   change_vars <- names(lpi_tall)[class(lpi_tall) %in%
#                                    c("POSIXct", "POSIXt")]
#
#   # Update fields
#   lpi_tall <- dplyr::mutate_at(
#     lpi_tall, dplyr::all_of(dplyr::vars(all_of(change_vars))),
#     list(as.character)
#   )
#
#
#   ## drops
#   lpi_tall <- lpi_tall %>% dplyr::select_if(!names(.) %in% c(
#
#     "DateModified", "FormType", "FormType",
#     "DataEntry", "DataErrorChecking", "DateVisited")
#   )
#
#
#   return(lpi_tall)
#   ## Output the list
# }



#' Function to convert LPI data into a long format.
#' @description This is a wrapper function for specialized functions which read
#' in Line-Point Intercept data from AIM anbd LMF sampling and return
#' them as a long-format data frame suitable for use in indicator calculations
#' with the package \code{terradactyl}. Supported formats include those used in
#' AIM (Assessment, Inventory, and Monitoring) and TerrADat (Terrestrial AIM
#' Database), LMF (Landscape Monitoring Framework), and DIMA (the Database for
#' Inventory, Monitoring, and Assessment). For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_lpi_terradat]{gather_lpi_terradat()}}, \code{\link[terradactyl:gather_lpi_lmf]{gather_lpi_lmf()}}, and
#' \code{\link[terradactyl:gather_lpi_nri]{gather_lpi_nri()}}.
#'
#' @inheritParams gather_lpi_terradat
#' @inheritParams gather_lpi_lmf
#' @param source Character string. This specifies the expected data format(s)
#'   and determines which specialized gather function will be used. It must be
#'   one of \code{"AIM"}, \code{"TERRADAT"}, \code{"LMF"}, \code{"DIMA"} or
#'   \code{"NRI"}. This is case-insensitive.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblLPIHeader} and \code{tblLPIDetail} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the argument \code{PINTERCEPT} is considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#' @examples
#' # LPI data from a geodatabase in the format of the Terrestrial AIM Database
#' gather_lpi(dsn = "data_path/aim_data.gdb,
#'            source = "terradat")
#'
#' # LPI data from a geodatabase in the format of the Landscape Monitoring Framework
#' gather_lpi(dsn = "data_path/lmf_data.gdb,
#'            source = "lmf")
#'
#'
#' @export

# Wrapper gather.lpi function
gather_lpi <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblLPIDetail = NULL,
                       tblLPIHeader = NULL,
                       PINTERCEPT = NULL,
                       # autoQC = TRUE,
                       verbose = FALSE
                       # LPI_0 = NULL,
                       # LPIDetail_1 = NULL
) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    lpi <- gather_lpi_terradat(dsn = dsn,
                               tblLPIDetail = tblLPIDetail,
                               tblLPIHeader = tblLPIHeader,
                               verbose = verbose)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    lpi <- gather_lpi_lmf(dsn = dsn,
                          file_type = file_type,
                          PINTERCEPT = PINTERCEPT,
                          verbose = verbose)
    lpi$chckbox <- NA
    # } else if(toupper(source) == "SURVEY123"){
    #   lpi <- gather_lpi_survey123(LPI_0 = LPI_0,
    #                               LPIDetail_1 = LPIDetail_1)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # Add source field
  # lpi$source <- toupper(source)
  lpi$source <- source

  if("sf" %in% class(lpi)){
    lpi <- sf::st_drop_geometry(lpi)
  }

  # Find date fields & convert to character
  # Find fields that are in a Date structure
  date_vars <- which(sapply(X = setNames(object = names(lpi),
                                         nm = names(lpi)),
                            lpi = lpi,
                            FUN = function(X, lpi){
                              any(c("POSIXct", "POSIXt") %in% class(lpi[[X]]))
                            }))

  # Update fields
  lpi <- dplyr::mutate(.data = lpi,
                       dplyr::across(.cols = tidyselect::all_of(date_vars),
                                     .fns = as.character))
  ## text field
  lpi$LineKey <- as.character(lpi$LineKey)

  # reorder so that primary key is leftmost column
  lpi <- dplyr::select(.data = lpi,
                       tidyselect::all_of(c("PrimaryKey",
                                            "LineKey")),
                       tidyselect::everything())

  # Drop rows with no data
  lpi <- dplyr::filter(.data = lpi,
                       !(is.na(LineKey) &
                           is.na(layer) &
                           is.na(code) &
                           is.na(ShrubShape) &
                           is.na(PointNbr)))

  # remove duplicates and empty rows
  # if(autoQC){
  #   message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
  #   lpi <- lpi %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "lpi")
  # }
  #
  lpi
}

#### HEIGHT ####################################################################
## Gather Height Data
#' Convert AIM-format height data into a long format.
#' @description This reads in height data from AIM sampling and returns it as a long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}.The expected format is that used in the Terrestrial AIM
#' Database (TerrADat).
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes.
#' @param tblLPIDetail Optional data frame or character string. If provided, this must contain the
#'   expected LPI data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblLPIDetail (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param tblLPIHeader Optional data frame or character string. If provided, this must contain the
#'   expected metadata for the LPI data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblLPIHeader (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param auto_qc_warnings  Logical. If \code{TRUE} the function will test the
#'   data and metadata for duplicated records (records which are identical to
#'   each other across all critical variables) and orphaned records (records
#'   which appear in the data but do not have any corresponding metadata
#'   records), either of which can result in erroneous or unexpected outputs.
#'   The number of duplicated and orphaned records will be reported but without
#'   making any changes. Defaults to \code{TRUE}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the tables tblLPIHeader and tblLPIDetail
#' gather_height_terradat(dsn = "data_folder/aim_data.gdb")
#'
#' # Using CSV files for tblLPIHeader and tblLPIDetail.
#' # These two arguments can also take data frames, if the data have already
#' # been read in as data frames.
#' gather_height_terradat(tblLPIHeader = "data_folder/lpi_headers.csv",
#'                        tblLPIDetail = "data_folder/lpi_detail_records.csv")
#'
#' # Using data frames for tblLPIHeader and tblLPIDetail
#' aim_lpiheader <- read.csv("data_folder/lpi_headers.csv")
#' aim_lpidetail <- read.csv("data_folder/lpi_detail_records.csv")
#' gather_height_terradat(tblLPIHeader = aim_lpiheader,
#'                        tblLPIDetail = aim_lpidetail)
#'
#' @export
gather_height_terradat <- function(dsn = NULL,
                                   tblLPIDetail= NULL,
                                   tblLPIHeader = NULL,
                                   auto_qc_warnings = TRUE,
                                   verbose = FALSE) {

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
                         "FormType")

  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblLPIHeader,
                               default_name = "tblLPIHeader",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()

  detail <- read_with_fallback(dsn = dsn,
                               tbl = tblLPIDetail,
                               default_name = "tblLPIDetail",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()
  # # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  # if (!is.null(tblLPIDetail) & !is.null(tblLPIHeader)) {
  #   if (verbose) {
  #     if (!is.null(dsn)) {
  #       message("Using the provided data frames. The provided dsn value is being ignored.")
  #     }
  #   }
  #   detail <- tblLPIDetail
  #   header <- tblLPIHeader
  # } else if(!is.null(dsn)){
  #   if (verbose) {
  #     message("Attempting to use the provided dsn value.")
  #   }
  #   if(!file.exists(dsn)){
  #     stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
  #   }
  #
  #   # The suppressWarnings() here are so that it doesn't complain about pulling
  #   # tables without geometry. We know that's what should be happening.
  #   detail <- suppressWarnings(sf::st_read(dsn = dsn,
  #                                          layer = "tblLPIDetail",
  #                                          stringsAsFactors = FALSE,
  #                                          quiet = TRUE))
  #   header <- suppressWarnings(sf::st_read(dsn = dsn,
  #                                          layer = "tblLPIHeader",
  #                                          stringsAsFactors = FALSE,
  #                                          quiet = TRUE))
  # } else {
  #   stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing tables with those names.")
  # }

  #### Cleanup #################################################################
  # Clean these up!
  # We're deliberating excluding some variables in both tables here. No sense
  # in bogging down the munging with extraneous things we'll never use.
  detail <- dplyr::select(.data = detail,
                          PrimaryKey, RecKey,
                          PointLoc, PointNbr,
                          tidyselect::matches(match = "(Woody)|(Herbaceous)|(LowerHerb)$")) |>
    dplyr::distinct()


  header <- dplyr::select(.data = header,
                          PrimaryKey,
                          LineKey:CheckboxLabel,
                          # tidyselect::matches(match = "DBKey"),
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()


  #### Automatic QC ############################################################
  if (auto_qc_warnings) {
    if (verbose) {
      message("Running automatic QC checks for duplicated or orphaned records.")
    }
    auto_qc_warning(header_data = header,
                    detail_data = detail,
                    uid_variables = list(header = c("PrimaryKey",
                                                    "RecKey"),
                                         detail = c("PrimaryKey",
                                                    "RecKey",
                                                    "PointNbr")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }

  #### Munging #################################################################
  # These are used to check for situations where there are going to be issues
  # with coerced data types or just the presence of NA values because some data
  # sets are not complete.
  variable_types <- c(woody = "Woody",
                      herbaceous = "Herbaceous",
                      lower_herbaceous = "LowerHerb")
  variable_classes <- list(Height = c("numeric",
                                      "integer"),
                           Species = "character")

  # Warn about variables which are the incorrect class for the data type
  coerced_variables_list <- lapply(X = setNames(object = names(variable_classes),
                                                nm = tolower(names(variable_classes))),
                                   variable_classes = variable_classes,
                                   variable_types = variable_types,
                                   detail = detail,
                                   FUN = function(X, variable_classes, variable_types, detail){
                                     detail_variables <- dplyr::select(.data = detail,
                                                                       tidyselect::matches(match = paste0("^", X,
                                                                                                          "((",
                                                                                                          paste(variable_types,
                                                                                                                collapse = ")|("),
                                                                                                          "))$"))) |>
                                       names()

                                     detail_variables_misclassed <- sapply(X = setNames(object = detail_variables,
                                                                                        nm = stringr::str_remove(string = detail_variables,
                                                                                                                 pattern = paste0("^", X))),
                                                                           detail = detail,
                                                                           current_variable_classes = variable_classes[[X]],
                                                                           FUN = function(X, detail, current_variable_classes){
                                                                             !(class(detail[[X]]) %in% current_variable_classes)
                                                                           })

                                     names(detail_variables_misclassed)[detail_variables_misclassed]
                                   })

  for (current_variable_prefix in names(coerced_variables_list)) {
    current_coerced_variables <- coerced_variables_list[[current_variable_prefix]]
    if (length(current_coerced_variables) > 0) {
      warning(paste0("There are one or more ", current_variable_prefix, " variables of the incorrect class in tblLPIDetail. ",
                     "These variables (", paste(current_coerced_variables,
                                                collapse = ", "), ") will be coerced into the correct class: ", variable_classes[[stringr::str_to_title(string = current_variable_prefix)]][1],"."))
    }
  }

  # Warn about introducing NAs by coercion.
  coerced_nas_list <- lapply(X = setNames(object = names(variable_classes),
                                          nm = tolower(names(variable_classes))),
                             variable_classes = variable_classes,
                             variable_types = variable_types,
                             detail = detail,
                             FUN = function(X, variable_classes, variable_types, detail){
                               nas_by_coercion <- sapply(X = variable_types,
                                                         variable_prefix = X,
                                                         variable_class = variable_classes[[X]],
                                                         detail = detail,
                                                         FUN = function(X, variable_prefix, variable_class, detail){
                                                           if (class(detail[[paste0(variable_prefix, X)]]) %in% variable_class) {
                                                             0
                                                           } else {
                                                             current_na_count <- sum(is.na(detail[[paste0(variable_prefix, X)]]))
                                                             coerced_na_count <- as.numeric(detail[[paste0(variable_prefix, X)]]) |>
                                                               suppressWarnings(expr = _) |>
                                                               is.na() |>
                                                               sum() - current_na_count

                                                             coerced_na_count
                                                           }
                                                         })
                               nas_by_coercion <- nas_by_coercion[nas_by_coercion > 0]
                             })

  for (current_variable_prefix in names(coerced_nas_list)) {
    current_nas_by_coercion <- coerced_nas_list[[current_variable_prefix]]
    if (length(current_nas_by_coercion) > 0) {
      warning(paste0("There are values that cannot be coerced into the correct class in at least one ", current_variable_prefix, " variable in tblLPIDetail. There will be ",
                     sum(nas_by_coercion),
                     " invalid ", current_variable_prefix, " values replaced with NA across the following ", current_variable_prefix, " types: ",
                     paste(names(nas_by_coercion) |>
                             stringr::str_replace(string = _,
                                                  pattern = "_",
                                                  replacement = " "),
                           collapse = ", "),
                     ". Any records with a ", current_variable_prefix, " value of NA, including those which were NA before any coercion, will be dropped from the output during processing."))
    }
  }


  # There are three height types that we're going to be working with here, so
  # we'll deal with each independently and then mash them together with
  # dplyr::bind_rows().
  # This approach with the lapply() and renaming makes more sense than doing
  # pivoting with tidyr functions because the value types are mixed (e.g., the
  # species are stored as character strings but heights are numeric) so we can't
  # put them all in one data frame variable when we pivot the data to a long
  # format.
  lpi_heights_tall <- lapply(X = variable_types,
                             detail = detail,
                             FUN = function(X, detail){
                               dplyr::select(.data = detail,
                                             PrimaryKey, RecKey,
                                             PointLoc, PointNbr,
                                             tidyselect::ends_with(match = X)) |>
                                 dplyr::rename_with(.data = _,
                                                    .cols = tidyselect::ends_with(match = X),
                                                    .fn = ~ stringr::str_extract(string = .x,
                                                                                 pattern = paste0(".+(?=", X, "$)"))) |>
                                 dplyr::mutate(.data = _,
                                               # We're going to suppress
                                               # warnings about introducing
                                               # NAs in this coercion because we
                                               # already warned the user above.
                                               Height = suppressWarnings(as.numeric(Height)),
                                               Species = suppressWarnings(as.character(Species)),
                                               type = dplyr::case_when(X == "LowerHerb" ~ "lower.herbaceous",
                                                                       .default = tolower(X)),
                                               GrowthHabit_measured = dplyr::case_when(X == "Woody" ~ "Woody",
                                                                                       X %in% c("Herbaceous",
                                                                                                "LowerHerb") ~ "NonWoody",
                                                                                       .default = NA)) |>
                                 dplyr::filter(.data = _,
                                               !is.na(Height))
                             }) |>
    dplyr::bind_rows()

  lpi_height <- suppressWarnings(dplyr::left_join(x = lpi_heights_tall,
                                                  y = header,
                                                  # As per usual, not enforcing
                                                  # the relationship because
                                                  # we're letting the users move
                                                  # ahead with not-quite-clean
                                                  # data.
                                                  # relationship = "many-to-one",
                                                  by = c("PrimaryKey",
                                                         "RecKey")))

  # Make sure that the values in Species are either at least possibly a species
  # code or NA
  lpi_height <- dplyr::mutate(.data = lpi_height,
                              Species = dplyr::case_when(stringr::str_detect(string = Species,
                                                                             pattern = "[[:digit:]]|[[:alpha:]]") ~ Species,
                                                         .default = NA))

  # Make sure we don't have duplicates because that could totally happen,
  # depending on the data quality.
  lpi_height <- dplyr::distinct(.data = lpi_height)

  # Output the woody/herbaceous level data
  return(lpi_height)
}


# Gather Height for LMF/NRI
#' Convert LMF-format height data into a long format.
#' @description This reads in height data from LMF sampling and returns it as a long-format data frame suitable for use
#' in indicator calculations with the package \code{terradactyl}.The expected format is that used in the Landscape Monitoring Framework (LMF).
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param file_type Deprecated. This argument is no longer functional or necessary and is kept for backwards compatibility with legacy code.
#' @param PASTUREHEIGHTS Optional data frame or character string. If provided, this must contain the
#'   expected height data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called PASTUREHEIGHTS (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the tables tblLPIHeader and tblLPIDetail
#' gather_height_lmf(dsn = "data_folder/lmf_data.gdb")
#'
#' # Using CSV files for PASTUREHEIGHTS.
#' # This argument can also take data frames, if the data have already been read
#' # in as a data frame.
#' gather_height_lmf(PASTUREHEIGHTS = "data_folder/lmf_pastureheights.csv")
#'
#' # Using data frames for PASTUREHEIGHTS
#' lmf_pastureheights <- read.csv("data_folder/lmf_pastureheights.csv")
#' gather_height_lmf(PASTUREHEIGHTS = lmf_pastureheights)
#'
#'
#' @export
gather_height_lmf <- function(dsn = NULL,
                              file_type = "gdb",
                              PASTUREHEIGHTS = NULL,
                              verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType",
                         "DBKey")

  #### Reading and cleanup #####################################################
  pastureheights <- read_with_fallback(dsn = dsn,
                                       tbl = PASTUREHEIGHTS,
                                       default_name = "PASTUREHEIGHTS",
                                       regex = TRUE,
                                       best_guess = TRUE,
                                       accept_failure = FALSE,
                                       verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()

  # if(!is.null(PASTUREHEIGHTS)){
  #   pastureheights <- PASTUREHEIGHTS
  # } else if (!is.null(dsn)) {
  #   if (!file.exists(dsn)) {
  #     stop("dsn must be a valid filepath to a geodatabase containing PASTUREHEIGHTS")
  #   }
  #
  #   # Read in the data as .txt or .gdb
  #   pastureheights <- switch(file_type,
  #                       "gdb" = {
  #                         suppressWarnings(sf::st_read(dsn,
  #                                                      layer = "PASTUREHEIGHTS",
  #                                                      stringsAsFactors = FALSE,
  #                                                      quiet = T
  #                         ))
  #                       },
  #                       "txt" = {
  #                         read.table(paste(dsn, "pastureheights.txt", sep = ""),
  #                                    stringsAsFactors = FALSE,
  #                                    header = FALSE,
  #                                    sep = "|",
  #                                    strip.white = TRUE
  #                         )
  #                       },
  #                       "csv" = {
  #                         read.csv(dsn)
  #                       }
  #   )
  #
  #   if (file_type == "txt") {
  #     # if it is in a text file, there are no field names assigned.
  #     colnames <- subset(
  #       terradactyl::nri.data.column.explanations,
  #       TABLE.NAME == "PASTUREHEIGHTS"
  #     ) |>
  #       dplyr::pull(FIELD.NAME) |>
  #       unique()
  #
  #     pastureheights <- pastureheights[seq_len(length(colnames))]
  #     names(pastureheights) <- colnames
  #
  #     # We need to establish and/or fix the PLOTKEY so it exists in a single field.
  #     pastureheights$PrimaryKey <- paste(pastureheights$SURVEY,
  #                                   pastureheights$STATE,
  #                                   pastureheights$COUNTY,
  #                                   pastureheights$PSU,
  #                                   pastureheights$POINT,
  #                                   sep = ""
  #     )
  #
  #     # Assign DBKey
  #     pastureheights$DBKey <- pastureheights$SURVEY
  #   }
  # } else {
  #   stop("Supply either PASTUREHEIGHTS or a path to a gdb containing that table")
  # }


  ##### Dealing with intersecting transects ------------------------------------
  # The arrangement of the transects for an LMF point crosses in the middle of
  # the transects (point 75) and so that intersection gets recorded twice, once
  # per transect. We'll drop the 75th record on the northeast-southwest transect
  # but we also want to warn the user that it's happening to any situations
  # where the assumption that they're identical is violated.
  duplicated_75mark_indices <- dplyr::filter(.data = pastureheights,
                                             DISTANCE == 75) |>
    dplyr::select(.data = _,
                  -TRANSECT) |>
    duplicated(x = _)
  pastureheights_75mark <- dplyr::filter(.data = pastureheights,
                                         DISTANCE == 75)
  pastureheights_75mark[["duplicated"]] <- duplicated_75mark_indices
  pastureheights_75mark_summary <- dplyr::summarize(.data = pastureheights_75mark,
                                                    .by = PrimaryKey,
                                                    n_records = dplyr::n(),
                                                    has_duplicate = any(duplicated))
  if (any(!pastureheights_75mark_summary$has_duplicate)) {
    warning(paste0("There are ", sum(!pastureheights_75mark_summary$has_duplicate),
                   " plots where the height records at the 75th sampling locations on the two transects are not identical to each other despite being the intersection of those transects. This is not unexpected and the records associated with the 'nesw' transects will still be dropped for these plots."))
  }

  pastureheights <- dplyr::filter(.data = pastureheights,
                                  !(TRANSECT == "nesw" & DISTANCE == 75))

  #### Reformatting and harmonizing ############################################
  # Most of the heavy lifting!
  # This will get us to the point where there's a separate record for each type
  # of measurement (woody and nonwoody) but there's a little more to adjust
  # after that.
  data_long <- dplyr::select(.data = pastureheights,
                             PrimaryKey,
                             LineKey = TRANSECT,
                             PointNbr = DISTANCE,
                             tidyselect::matches(match = "HEIGHT"),
                             tidyselect::matches(match = "PLANT")) |>
    # This splits the species and the heights into separate records, but that's
    # actually fine for now.
    tidyr::pivot_longer(data = _,
                        cols = -tidyselect::all_of(c("PrimaryKey",
                                                     "LineKey",
                                                     "PointNbr")),
                        names_to = "variable",
                        values_to = "value") |>
    # This removes all the records where there were no species or no heights.
    # "None" is fine because we'll have a measure of 0 there.
    dplyr::filter(.data = _,
                  !(value %in% c(NA,
                                 ""))) |>
    dplyr::mutate(.data = _,
                  # Add in the type and GrowthHabit_measured variables based on
                  # whether the variable name was prefixed with a W, assuming
                  # that if there's not a W that meant it was nonwoody.
                  GrowthHabit_measured = dplyr::case_when(stringr::str_detect(string = variable,
                                                                              pattern = "^W") ~ "Woody",
                                                          .default = "NonWoody"),
                  type = dplyr::case_when(GrowthHabit_measured == "Woody" ~ "woody",
                                          .default = "herbaceous"),
                  # Drop the prefixes from these values so that we can pivot
                  # wider to combine the record types.
                  variable = stringr::str_extract(string = variable,
                                                  pattern = "PLANT|HEIGHT")) |>
    tidyr::pivot_wider(data = _,
                       names_from = "variable",
                       values_from = "value")

  # Now we split the HEIGHT variable into one with the height and one with the
  # units.
  data_long <- dplyr::mutate(.data = data_long,
                             # The pattern used will find numbers with decimal places
                             # but they shouldn't be in there in the first place.
                             Height = stringr::str_extract(string = HEIGHT,
                                                           pattern = "^\\d+\\.?\\d*") |>
                               as.numeric() |>
                               tidyr::replace_na(data = _,
                                                 replace = 0),
                             HeightUOM = stringr::str_extract(string = HEIGHT,
                                                              pattern = "[a-z]{2}$")) |>
    dplyr::mutate(.data = _,
                  # Convert to centimeters!!!
                  Height = dplyr::case_when(HeightUOM %in% c("in") ~ Height * 2.54,
                                            HeightUOM %in% c("ft") ~ Height * 2.54 * 12,
                                            .default = Height),
                  HeightUOM = "cm") |>
    dplyr::select(.data = _,
                  -HEIGHT)

  # We'll also replace any "None" or "" values in PLANT with NA
  data_long <- dplyr::mutate(.data = data_long,
                             Species = dplyr::case_when(PLANT %in% c("", "None") ~ NA,
                                                        .default = PLANT))

  # Return only the unique records and with the variables ordered how we'd like
  dplyr::select(.data = data_long,
                tidyselect::all_of(c("PrimaryKey",
                                     "LineKey",
                                     "PointNbr",
                                     "Species",
                                     "Height",
                                     "HeightUOM",
                                     "type",
                                     "GrowthHabit_measured"))) |>
    dplyr::distinct()
}

# #' export gather_height_survey123
# #' rdname gather_height
# gather_height_survey123 <- function(LPI_0,
#                                     LPIDetail_1) {
#
#   # This code copy-and-pasted from gather_height_terradat
#   lpi_header <- LPI_0
#   lpi_detail <- LPIDetail_1
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- lpi_header$PlotKey[duplicated(lpi_header$PlotKey) & duplicated(LPIHeader123$LineKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Survery123 uses GlobalID to link data, not PrimaryKey. Bring in PrimaryKey from PlotKey
#   lpi_header$PrimaryKey <- lpi_header$PlotKey
#   lpi_detail <- dplyr::left_join(lpi_detail, lpi_header %>% dplyr::select(GlobalID, PrimaryKey), by = c("ParentGlobalID" = "GlobalID"))
#
#   if (any(colnames(lpi_header) %in% "DBKey")) {
#     levels <- rlang::quos(PrimaryKey, "DBKey")
#   } else {
#     levels <- rlang::quos(PrimaryKey)
#   }
#
#   # we only want to carry a subset of the lpi_header fields forward
#   # lpi_header <- dplyr::select(lpi_header, !!!levels, PlotID:CheckboxLabel)
#
#   # Add null DBKey column if not present
#   if(!("DBKey" %in% colnames(lpi_header))) lpi_header$DBKey <- NA
#   if(!("DBKey" %in% colnames(lpi_detail))) lpi_detail$DBKey <- NA
#
#   lpi_height_tall_woody <- dplyr::select(
#     .data = lpi_detail,
#     !!!levels,
#     PointLoc,
#     PointNbr,
#     RecKey,
#     DBKey,
#     dplyr::matches("Woody$")
#   ) %>% dplyr::mutate(type = "woody")
#   # Strip out the extra name stuff so woody and herbaceous variable names match.
#   names(lpi_height_tall_woody) <- stringr::str_replace_all(
#     string = names(lpi_height_tall_woody),
#     pattern = "Woody$",
#     replacement = ""
#   )
#   # Add observed growth habit field
#   lpi_height_tall_woody$GrowthHabit_measured <- "Woody"
#
#   # Herbaceous height
#   lpi_height_tall_herb <- dplyr::select(
#     .data = lpi_detail,
#     !!!levels,
#     PointLoc,
#     PointNbr,
#     RecKey,
#     DBKey,
#     dplyr::matches("Herbaceous$")
#   ) %>% dplyr::mutate(type = "herbaceous")
#   names(lpi_height_tall_herb) <- stringr::str_replace_all(
#     string = names(lpi_height_tall_herb),
#     pattern = "Herbaceous$",
#     replacement = ""
#   )
#   # Add observed growth habit field
#   lpi_height_tall_herb$GrowthHabit_measured <- "NonWoody"
#
#   # Merge all gather types together
#   lpi_height <- rbind(
#     lpi_height_tall_woody,
#     lpi_height_tall_herb
#   )
#
#   lpi_height <- lpi_height %>%
#     dplyr::full_join(x = ., y = lpi_header, by = c("PrimaryKey", "DBKey")) %>%
#     subset(., !is.na(Height))
#
#   # Add NA to fields with no species
#   lpi_height$Species[!grepl(pattern = "[[:digit:]]|[[:alpha:]]", lpi_height$Species)] <- NA
#
#   # Remove orphaned records and duplicates, if they exist
#   lpi_height <- unique(lpi_height)
#   lpi_height <- lpi_height[!is.na(lpi_height$PrimaryKey), ]
#
#   # Make sure height is a numeric field
#   lpi_height$Height <- suppressWarnings(as.numeric(lpi_height$Height))
#
#   # final drop
#   lpi_height <- lpi_height %>%
#     dplyr::select(
#       "PrimaryKey", "DBKey", "PointLoc", "PointNbr", "RecKey", "Height",
#       "Species", "Chkbox", "type", "GrowthHabit_measured", "LineKey", "FormDate",
#       "Direction",
#       ## These methods data are not in Survey123
#       # "Measure",
#       # "LineLengthAmount",
#       # "SpacingIntervalAmount",
#       # "SpacingType",
#       # "HeightOption",
#       # "HeightUOM",
#       # "ShowCheckbox",
#       # "CheckboxLabel"
#     )
#
#
#   # Output the woody/herbaceous level data
#   return(lpi_height)
# }
#

#' Convert height data into a tall, tidy data frame
#'
#' @description Given wide format line-point intercept or height data, create a tall
#' format data frame usable by other terradactyl functions. For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_height_terradat]{gather_height_terradat()}}, \code{\link[terradactyl:gather_height_lmf]{gather_height_lmf()}}, and
#' \code{\link[terradactyl:gather_height_nri]{gather_height_nri()}}.
#'
#' @inheritParams gather_height_terradat
#' @inheritParams gather_height_lmf
#' @param source Character string. The data source format. Must be one of \code{"AIM"}, \code{"TerrADat"}, \code{"DIMA"}, or \code{"LMF"} (case independent).
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblLPIHeader} and \code{tblLPIDetail} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the argument \code{PASTUREHEIGHTS} is considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#' @return A tall data frame containing the data from the height measurements.
#' @export
#' @examples
#' gather_height(dsn = "Path/To/AIM_Geodatabase.gdb",
#'               source = "AIM")
#' gather_height(dsn = "Path/To/LMF_Geodatabase.gdb",
#'               source = "LMF")
#'
#' aim_lpidetail <- read.csv("Path/To/tblLPIDetail.csv")
#' aim_lpiheader <- read.csv("Path/To/tblLPIHeader.csv")
#' gather_height(source = "AIM",
#'               tblLPIDetail = aim_lpidetail,
#'               tblLPIHeader = aim_lpiheader)
#'
#' lmf_heights <- read.csv("Path/To/PASTUREHEIGHTS.csv")
#' gather_height(source = "LMF",
#'               PASTUREHEIGHTS = lmf_heights)

gather_height <- function(dsn = NULL,
                          file_type = "gdb",
                          source,
                          tblLPIDetail = NULL,
                          tblLPIHeader = NULL,
                          PASTUREHEIGHTS = NULL,
                          autoQC = TRUE,
                          verbose = FALSE) {
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    height <- gather_height_terradat(dsn = dsn,
                                     tblLPIHeader = tblLPIHeader,
                                     tblLPIDetail = tblLPIDetail,
                                     verbose = verbose)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    height <- gather_height_lmf(dsn = dsn,
                                file_type = file_type,
                                PASTUREHEIGHTS = PASTUREHEIGHTS,
                                verbose = verbose)
    # } else if(toupper(source) %in% c("SURVEY123")){
    #   height <- gather_height_survey123(
    #     LPI_0 = LPI_0,
    #     LPIDetail_1 = LPIDetail_1
    #   )
  } else {
    stop('source must be "AIM", "TerrADat", "DIMA", "LMF", or "NRI" (case independent).')
  }

  # height$source <- toupper(source)
  height$source <- source

  if("sf" %in% class(height)) height <- sf::st_drop_geometry(height)

  if (any(class(height) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(height)[do.call(rbind, vapply(height,
                                                       class))[, 1] %in% c("POSIXct", "POSIXt")]
    height <- dplyr::mutate_at(height, dplyr::vars(change_vars),
                               dplyr::funs(as.character))
  }

  height <- dplyr::select(.data = height,
                          tidyselect::all_of(c("PrimaryKey",
                                               "LineKey")),
                          tidyselect::everything())

  # remove duplicates and empty rows
  # if(autoQC){
  #   message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
  #   height <- height %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "height")
  # }

  # Output height
  dplyr::distinct(.data = height)
}


#### GAP #######################################################################
#' Convert AIM-format gap data into a long/tall format.
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes.
#' @param tblGapDetail Optional data frame or character string. If provided, this must contain the
#'   expected gap data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblGapDetail (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param tblGapHeader Optional data frame or character string. If provided, this must contain the
#'   expected metadata for the gap data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblGapHeader (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param auto_qc_warnings  Logical. If \code{TRUE} the function will test the
#'   data and metadata for duplicated records (records which are identical to
#'   each other across all critical variables) and orphaned records (records
#'   which appear in the data but do not have any corresponding metadata
#'   records), either of which can result in erroneous or unexpected outputs.
#'   The number of duplicated and orphaned records will be reported but without
#'   making any changes. Defaults to \code{TRUE}.
#' @param drop_na Logical. If \code{TRUE} then records with \code{NA} values in
#' their Gap variable will be dropped. Defaults to \code{TRUE}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the tables tblGapHeader and tblGapDetail
#' gather_gap_terradat(dsn = "data_folder/aim_data.gdb")
#'
#' # Using CSV files for tblGapHeader and tblGapDetail.
#' # These two arguments can also take data frames, if the data have already
#' # been read in as data frames.
#' gather_gap_terradat(tblGapHeader = "data_folder/gap_headers.csv",
#'                     tblGapDetail = "data_folder/gap_detail_records.csv")
#'
#' # Using data frames for tblGapHeader and tblGapDetail
#' aim_gapheader <- read.csv("data_folder/gap_headers.csv")
#' aim_gapdetail <- read.csv("data_folder/gap_detail_records.csv")
#' gather_gap_terradat(tblGapHeader = aim_gapheader,
#'                     tblGapDetail = aim_gapdetail)
#'
#'
#'
#' @export
gather_gap_terradat <- function(dsn = NULL,
                                tblGapDetail = NULL,
                                tblGapHeader = NULL,
                                auto_qc_warnings = TRUE,
                                drop_na = TRUE,
                                verbose = FALSE) {

  # These are used for data management within a geodatabase and we're going to
  # drop them.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "DBKey",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")
  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblGapHeader,
                               default_name = "tblGapHeader",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  detail <- read_with_fallback(dsn = dsn,
                               tbl = tblGapDetail,
                               default_name = "tblGapDetail",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  ### switch by input types
  # if(!is.null(tblGapDetail) & !is.null(tblGapHeader)){
  #   if (verbose) {
  #     if (!is.null(dsn)) {
  #       message("Using the provided data frames. The provided dsn value is being ignored.")
  #     }
  #   }
  #   detail <- tblGapDetail
  #   header <- tblGapHeader
  # } else if(!is.null(dsn)){
  #   if (!file.exists(dsn)) {
  #     stop("dsn must be a valid filepath to a geodatabase containing tblGapDetail and tblGapHeader")
  #   }
  #   # The suppressWarnings() here are so that it doesn't complain about pulling
  #   # tables without geometry. We know that's what should be happening.
  #   # Read tblGapDetail
  #   detail <- suppressWarnings(sf::st_read(dsn = dsn,
  #                                          layer = "tblGapDetail",
  #                                          stringsAsFactors = FALSE,
  #                                          quiet = TRUE))
  #
  #   # Read tblGapHeader
  #   header <- suppressWarnings(sf::st_read(dsn = dsn,
  #                                          layer = "tblGapHeader",
  #                                          stringsAsFactors = FALSE,
  #                                          quiet = TRUE))
  #
  # } else {
  #   stop("Provide both tblGapDetail and tblGapHeader or the path to a GDB containing those tables.")
  # }
  #
  # # Clean these up!
  # detail <- dplyr::select(.data = detail,
  #                         -tidyselect::any_of(internal_gdb_vars)) |>
  #   dplyr::filter(.data = _,
  #                 !is.na(PrimaryKey)) |>
  #   dplyr::distinct()
  #
  # header <- dplyr::select(.data = header,
  #                         -tidyselect::any_of(internal_gdb_vars)) |>
  #   dplyr::filter(.data = _,
  #                 !is.na(PrimaryKey)) |>
  #   dplyr::distinct()

  # # add null DBKey field if not present
  # if(!"DBKey" %in% colnames(header)) header$DBKey <- NA
  # if(!"DBKey" %in% colnames(detail)) detail$DBKey <- NA

  # Make sure that Gap, GapStart, and GapEnd are all numeric.
  not_numeric_vars <- which(sapply(X = c(GapStart = "GapStart",
                                         GapEnd = "GapEnd",
                                         Gap = "Gap"),
                                   detail = detail,
                                   FUN = function(X, detail){
                                     class(detail[[X]]) != "numeric"
                                   }))
  detail <- dplyr::mutate(.data = detail,
                          dplyr::across(.cols = tidyselect::all_of(names(not_numeric_vars)),
                                        .fns = as.numeric))

  #### Automatic QC ############################################################
  if (auto_qc_warnings) {
    if (verbose) {
      message("Running automatic QC checks for duplicated or orphaned records.")
    }
    auto_qc_warning(header_data = header,
                    detail_data = detail,
                    uid_variables = list(header = c("PrimaryKey",
                                                    "RecKey"),
                                         detail = c("PrimaryKey",
                                                    "RecKey",
                                                    "RecType",
                                                    "GapStart",
                                                    "GapEnd",
                                                    "Gap")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }


  #### Munging #################################################################
  # Merge header and detail data together.
  # We're suppressing warnings and not enforcing the relationship so that the
  # user can move ahead without cleaning their data first.
  # This is a left join so that we can properly include LineKeys that don't have
  # associated gaps because there were none. That's critical for calculating
  # gap percentages.
  gap_tall <- suppressWarnings(dplyr::left_join(x = header,
                                                y = detail,
                                                # relationship = "one-to-many",
                                                by = c("PrimaryKey",
                                                       "RecKey")))

  # This bit corrects (or at least replaces) NA values in a few of the variables
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            # Make sure that any NA values in the variables
                            # NoCanopyGaps and NoBasalGaps are assumed to
                            # be 0.
                            dplyr::across(.cols = tidyselect::all_of(c("NoCanopyGaps",
                                                                       "NoBasalGaps")),
                                          .fns = ~ as.numeric(tidyr::replace_na(data = .x,
                                                                                replace = 0))),
                            # For any records where we don't know the
                            # RecType value and NoCanopyGaps is 1, assume
                            # that RecType should be "C"
                            RecType = dplyr::case_when(NoCanopyGaps == 1 ~ tidyr::replace_na(RecType,
                                                                                             "C"),
                                                       .default = RecType),
                            # For any records where we don't know the
                            # Measure value and NoCanopyGaps is 1, assume
                            # that Measure should be 1
                            Measure = dplyr::case_when(NoCanopyGaps == 1 ~ tidyr::replace_na(Measure,
                                                                                             1),
                                                       .default = Measure),
                            # Make sure that any NA values in the variables
                            # GapStart, GapEnd, and Gap are 0 when the
                            # relevant No*Gaps and RecType values say there
                            # should be no gaps.
                            dplyr::across(.cols = tidyselect::all_of(c("GapStart",
                                                                       "GapEnd",
                                                                       "Gap")),
                                          .fns = ~ dplyr::case_when(NoCanopyGaps == 1 & RecType == "C" ~ tidyr::replace_na(data = .x,
                                                                                                                           replace = 0),
                                                                    NoBasalGaps == 1 & RecType == "B" ~ tidyr::replace_na(data = .x,
                                                                                                                          replace = 0),
                                                                    .default  = .x)),
                            # Where possible, calculate the Gap for records that
                            # don't have it currently.
                            Gap = dplyr::case_when(is.na(Gap) & !is.na(GapStart) & !is.na(GapEnd) ~ abs(GapStart - GapEnd),
                                                   .default = Gap))


  # Create records to represent the 0 values for situations where we have no
  # canopy or basal gaps but don't already have 0 values.
  # The function here is reframe() and not summarize() because some of the
  # combinations of values in the "by" variables don't have any qualifying
  # records and summarize() complains about it.
  missing_records <- dplyr::reframe(.data = gap_tall,
                                    # .by = 1:PrimaryKey,
                                    .by = tidyselect::all_of(c("PrimaryKey",
                                                               "LineKey",
                                                               "RecKey",
                                                               "NoCanopyGaps",
                                                               "NoBasalGaps")),
                                    needs_canopy = !("C" %in% RecType) & NoCanopyGaps == 1,
                                    needs_basal = !("B" %in% RecType) & NoBasalGaps == 1) |>
    dplyr::filter(.data = _,
                  needs_canopy | needs_basal) |>
    dplyr::mutate(.data = _,
                  RecType = dplyr::case_when(needs_canopy ~ "C",
                                             needs_basal ~ "B",
                                             .default = NA),
                  GapStart = 0,
                  GapEnd = 0,
                  Gap = 0,
                  Measure = 1) |>
    dplyr::select(.data = _,
                  -needs_canopy,
                  -needs_basal) |>
    dplyr::left_join(x = _,
                     y = dplyr::select(.data = header,
                                       -LineKey,
                                       -Measure,
                                       -NoCanopyGaps,
                                       -NoBasalGaps),
                     by = c("PrimaryKey", "RecKey"))

  gap_tall <- dplyr::bind_rows(gap_tall,
                               missing_records) |>
    dplyr::distinct()

  # Make sure that RecType is "P" for records where only perennial vegetation
  # was considered when evaluating if there was sufficient canopy to end a gap.
  # This should only happen for CANOPY and not BASAL. "P" is specifically for
  # records where the method was for perennial-only canopy, nothing to do with
  # basal gaps.
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            RecType = dplyr::case_when(PerennialsCanopy == 1 &
                                                         AnnualForbsCanopy == 0 &
                                                         AnnualGrassesCanopy == 0 &
                                                         OtherCanopy == 0 &
                                                         RecType == "C" ~ "P",
                                                       .default = RecType))

  if (drop_na) {
    if (any(is.na(gap_tall$Gap))) {
      warning(paste0("There were ", sum(is.na(gap_tall$Gap)), " records with no valid Gap value or valid GapStart and GapEnd values to calculate from. These records will be dropped."))
      gap_tall <- dplyr::filter(.data = gap_tall,
                                !is.na(Gap))
    }
  }

  gap_tall
}

#' Convert LMF-format gap data into a long/tall format.
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param file_type Deprecated. This argument is no longer functional or necessary and is kept for backwards compatibility with legacy code.
#' @param GINTERCEPT Optional data frame or character string. If provided, this must contain the
#'   expected gap intercept data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called GINTERCEPT (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param POINT Optional data frame or character string. If provided, this must contain the expected
#'   point metadata. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called POINT (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the tables tblGapHeader and tblGapDetail
#' gather_gap_lmf(dsn = "data_folder/aim_data.gdb")
#'
#' # Using CSV files for tblGapHeader and tblGapDetail.
#' # These two arguments can also take data frames, if the data have already
#' # been read in as data frames.
#' gather_gap_lmf(tblGapHeader = "data_folder/gap_headers.csv",
#'                     tblGapDetail = "data_folder/gap_detail_records.csv")
#'
#' # Using data frames for tblGapHeader and tblGapDetail
#' aim_gapheader <- read.csv("data_folder/gap_headers.csv")
#' aim_gapdetail <- read.csv("data_folder/gap_detail_records.csv")
#' gather_gap_lmf(tblGapHeader = aim_gapheader,
#'                     tblGapDetail = aim_gapdetail)
#' @export

gather_gap_lmf <- function(dsn = NULL,
                           file_type = NULL,
                           GINTERCEPT = NULL,
                           POINT = NULL,
                           verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType",
                         "DBKey")

  #### Reading and cleanup #####################################################
  gintercept <- read_with_fallback(dsn = dsn,
                                   tbl = GINTERCEPT,
                                   default_name = "GINTERCEPT",
                                   regex = TRUE,
                                   best_guess = TRUE,
                                   accept_failure = FALSE,
                                   verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()
  point <- read_with_fallback(dsn = dsn,
                              tbl = POINT,
                              default_name = "POINT",
                              regex = TRUE,
                              best_guess = TRUE,
                              accept_failure = FALSE,
                              verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()

  # # Make sure we can read in the data.
  # valid_file_types <- c("gdb", "txt")
  #
  # # We'll extract the extension if file_type isn't provided
  # if(is.null(file_type)){
  #   file_type <- tools::file_ext(dsn)
  # }
  # # Sanitize it.
  # file_type <- tolower(file_type)
  #
  #
  # if(!is.null(GINTERCEPT) & !is.null(POINT)){
  #   gintercept <- GINTERCEPT
  #   point <- POINT
  # } else if (!is.null(dsn)){
  #   if (!file.exists(dsn)) {
  #     stop(paste0("The provided dsn (", dsn, ") does not exist."))
  #   }
  #   if (!(file_type %in% valid_file_types)) {
  #     stop(paste0("The current file_type value (", file_type, ") is invalid. ",
  #                 "Valid file types are: ", paste(valid_file_types,
  #                                                 collapse = ", ")))
  #   }
  #   # Read in GINTERCEPT
  #   gintercept <- switch(file_type,
  #                        "gdb" = {
  #                          sf::st_read(dsn = dsn,
  #                                      layer = "GINTERCEPT",
  #                                      stringsAsFactors = FALSE,
  #                                      quiet = TRUE)
  #                        },
  #                        "txt" = {
  #                          read.table(file = file.path(dsn,
  #                                                      "gintercept.txt"),
  #                                     stringsAsFactors = FALSE,
  #                                     strip.white = TRUE,
  #                                     header = FALSE,
  #                                     sep = "|")
  #                        })
  #   # Read in point file for other plot level information
  #   point <- switch(file_type,
  #                   "gdb" = {
  #                     sf::st_read(dsn = dsn,
  #                                 layer = "POINT",
  #                                 stringsAsFactors = FALSE,
  #                                 quiet = TRUE)
  #                   },
  #                   "txt" = {
  #                     read.table(file = file.path(dsn,
  #                                                 "point.txt"),
  #                                stringsAsFactors = FALSE,
  #                                strip.white = TRUE,
  #                                header = FALSE,
  #                                sep = "|")
  #                   })
  #
  #
  #   if (file_type == "txt") {
  #     # Add meaningful column names
  #     gintercept <- name_variables_nri(
  #       data = gintercept,
  #       table_name = "GINTERCEPT"
  #     )
  #     point <- name_variables_nri(
  #       data = gintercept,
  #       table_name = "POINT"
  #     )
  #   }
  # } else {
  #   stop("Supply either data frames for the arguments GINTERCEPT and POINT or the path to a GDB containing those tables as the argument dsn.")
  # }
  #
  #   # These are used for data management within a geodatabase and we're going to
  #   # drop them. This helps us to weed out duplicate records created by quirks of
  #   # the ingest processes.
  #   internal_gdb_vars <- c("GlobalID",
  #                          "created_user",
  #                          "created_date",
  #                          "last_edited_user",
  #                          "last_edited_date",
  #                          "DateLoadedInDb",
  #                          "DateLoadedinDB",
  #                          "rid",
  #                          "DataErrorChecking",
  #                          "DataEntry",
  #                          "DateModified",
  #                          "FormType",
  #                          "DBKey")
  #
  #   gintercept <- dplyr::select(.data = gintercept,
  #                               -tidyselect::any_of(internal_gdb_vars)) |>
  #     dplyr::distinct()
  #   point <- dplyr::select(.data = point,
  #                          -tidyselect::any_of(internal_gdb_vars)) |>
  #     dplyr::distinct()

  # Ensure START_GAP and END_GAP are numeric
  gintercept <- dplyr::mutate(.data = gintercept,
                              dplyr::across(.cols = tidyselect::all_of(c("START_GAP",
                                                                         "END_GAP")),
                                            .fns = as.numeric))

  if (any(gintercept$START_GAP > gintercept$END_GAP)) {
    warning("There are some records with negative gap sizes. These will be dropped.")
    gintercept <- dplyr::filter(.data = gintercept,
                                START_GAP <= END_GAP)
  }

  #### Identical perennial and all-plant canopy ################################
  # This takes the point table and identifies which transects at which points
  # are marked as having identical perennial-only and all-plant canopies
  # We'll use this to make records for the all-plant canopy values from the
  # perennial-only ones where necessary.

  # Grab only the relevant variables, the PrimaryKey and the ones indicating if
  # the gaps were different.
  potential_canopy_transects <- dplyr::select(.data = point,
                                              PrimaryKey,
                                              tidyselect::starts_with(match = "GAPS_DIFFERENT_")) |>
    # Pivot that so that there's one row for each PrimaryKey-transect
    # combination and a logical variable indicating if the gaps were different.
    tidyr::pivot_longer(data = _,
                        cols = -PrimaryKey,
                        names_to = "TRANSECT",
                        # This takes the variable names and gets the transect ID
                        # that's found in GINTERCEPT from them.
                        names_transform = ~ tolower(stringr::str_extract(string = .x,
                                                                         pattern = "[NSEW]{4}$")),
                        values_to = "same",
                        # This converts the character values into logical ones
                        # where TRUE corresponds to the gaps being the same.
                        values_transform = ~ .x %in% c("N")) |>
    dplyr::filter(.data = _,
                  same) |>
    dplyr::select(.data = _,
                  -same) |>
    dplyr::distinct()

  # These are the data which *might* need inference, by which we mean copying
  # the perennial-only records and changing the GAP_TYPE to "canopy".
  potential_inference_data <- dplyr::left_join(x = potential_canopy_transects,
                                               y = dplyr::filter(gintercept,
                                                                 GAP_TYPE == "peren"),
                                               relationship = "one-to-many",
                                               by = c("PrimaryKey",
                                                      "TRANSECT"))

  # Check to limit this to only transects where there's not already canopy data.
  agreement_joining_vars <- c("PrimaryKey",
                              "TRANSECT",
                              "START_GAP",
                              "END_GAP")
  agreement_summary <- dplyr::full_join(x = dplyr::filter(.data = potential_inference_data,
                                                          GAP_TYPE == "peren") |>
                                          dplyr::select(.data = _,
                                                        tidyselect::all_of(agreement_joining_vars)) |>
                                          dplyr::mutate(.data = _,
                                                        perennial = TRUE),
                                        y = dplyr::filter(.data = potential_inference_data,
                                                          GAP_TYPE == "canopy") |>
                                          dplyr::select(.data = _,
                                                        tidyselect::all_of(agreement_joining_vars)) |>
                                          dplyr::mutate(.data = _,
                                                        canopy = TRUE),
                                        by = agreement_joining_vars)

  # So, based on the agreement (or disagreement) we need to warn the user and
  # take action.
  if (any(is.na(agreement_summary$canopy)) | any(is.na(agreement_summary$perennial))) {
    disagreeing_transects <- dplyr::summarize(.data = agreement_summary,
                                              .by = tidyselect::all_of(c("PrimaryKey",
                                                                         "TRANSECT")),
                                              # We'll get the proportion of the
                                              # GAP_TYPE records that're NA
                                              # which should be the proportion
                                              # of records which don't match
                                              # between the gap types.
                                              proportion_na_canopy = sum(is.na(canopy)) / dplyr::n(),
                                              proportion_na_perennial = sum(is.na(perennial)) / dplyr::n())
    # This identifies the places where there were no canopy records but there
    # were perennial records. For those, we'll copy the perennial records and
    # change GAP_TYPE to "canopy"
    if (any(disagreeing_transects$proportion_na_canopy == 1)) {
      warning("There are transects where it was indicated that perennial-only and all-plant canopy gaps were identical but only perennial records exist. This is not unexpected and the all-plant records will be inferred from the perennial-only records.")
      inferred_canopy <- dplyr::left_join(x = dplyr::filter(.data = disagreeing_transects,
                                                            proportion_na_canopy == 1) |>
                                            dplyr::select(.data = _,
                                                          PrimaryKey,
                                                          TRANSECT),
                                          y = potential_inference_data,
                                          by = c("PrimaryKey",
                                                 "TRANSECT")) |>
        dplyr::mutate(.data = _,
                      GAP_TYPE = "canopy")
      gintercept <- dplyr::bind_rows(gintercept,
                                     inferred_canopy)
    }

    # This does the same but for perennial-only records.
    # This shouldn't happen based on how LMF is implemented in the field, but
    # better safe than sorry.
    if (any(disagreeing_transects$proportion_na_perennial == 1)) {
      warning("There are transects where it was indicated that perennial-only and all-plant canopy gaps were identical but only canopy records exist. This is unexpected based on LMF data collection methods, but the perennial-only records will be inferred from the all-plant records.")
      inferred_perennial <- dplyr::left_join(x = dplyr::filter(.data = disagreeing_transects,
                                                               proportion_na_perennial == 1) |>
                                               dplyr::select(.data = _,
                                                             PrimaryKey,
                                                             TRANSECT),
                                             y = potential_inference_data,
                                             by = c("PrimaryKey",
                                                    "TRANSECT")) |>
        dplyr::mutate(.data = _,
                      GAP_TYPE = "peren")
      gintercept <- dplyr::bind_rows(gintercept,
                                     inferred_perennial)
    }



    # This is where we warn the user if there were only partial mismatches.
    # This really shouldn't happen if the values are expected to be the same!
    partial_mismatches <- dplyr::filter(.data = disagreeing_transects,
                                        proportion_na_canopy < 1,
                                        proportion_na_canopy > 0,
                                        proportion_na_perennial < 1,
                                        proportion_na_perennial > 0) |>
      dplyr::select(.data = _,
                    PrimaryKey,
                    TRANSECT) |>
      dplyr::distinct()
    if (nrow(partial_mismatches) > 0) {
      warning(paste0("There are ", nrow(partial_mismatches), " transects representing ",
                     length(unique(partial_mismatches$PrimaryKey)),
                     " sampling locations where perennial-only and all-plant canopy were indicated to be identical but they are not. These records will be left as-is and calculations using those transects will produce different values for perennial-only and all-plant canopy."))
    }
  }

  #### Zero canopy gaps ########################################################
  # Find the places where there were supposedly no gaps, adding 0 records where
  # appropriate and warning the user when there are unexpected non-zero records.

  # Identify the places where the point table indicates that there aren't gaps.
  potential_zero_gap_transects <- dplyr::select(.data = point,
                                                PrimaryKey,
                                                tidyselect::matches(match = "_GAPS_[NSEW]{4}$")) |>
    tidyr::pivot_longer(data = _,
                        cols = tidyselect::matches(match = "_GAPS_[NSEW]{4}$"),
                        names_to = c("GAP_TYPE",
                                     "TRANSECT"),
                        # This will produce the corresponding GAP_TYPE and
                        # TRANSECT values from the variable names.
                        names_pattern = "(^[A-Z]+).+([NSEW]{4})",
                        names_transform = ~ stringr::str_extract(string = tolower(.x),
                                                                 pattern = "basal|canopy|peren|[nesw]{4}"),
                        values_to = "not_present",
                        values_transform = ~ .x %in% c("N")) |>
    dplyr::filter(.data = _,
                  not_present) |>
    dplyr::select(.data = _,
                  -not_present) |>
    dplyr::distinct()

  # Find the transects that claimed to not have data but actually do.
  actually_present <- dplyr::left_join(x = potential_zero_gap_transects,
                                       y = gintercept,
                                       by = c("PrimaryKey",
                                              "TRANSECT",
                                              "GAP_TYPE")) |>
    dplyr::filter(.data = _,
                  # Assuming that gaps are always recorded with the GAP_END
                  # value being larger than the START_GAP value, this will
                  # limit records to only those where there were no 0-length
                  # gaps
                  !(END_GAP %in% c(0, NA)))

  actually_present_summary <- dplyr::summarize(.data = actually_present,
                                               .by = c("GAP_TYPE"),
                                               pk_count = length(unique(PrimaryKey)),
                                               transect_count = length(unique(TRANSECT)))

  # If there were data where we didn't expect them, warn the user!
  if (nrow(actually_present_summary) > 0) {
    warning(paste0("There are ", sum(actually_present_summary$transect_count),
                   " transects across ", sum(actually_present_summary$pk_count),
                   " sampling locations which are flagged as not having gaps (perennial-only canopy, all-plant canopy, or basal) but which do have gaps recorded. These records will be left as-is."))

    # Remove the ones that have data from the set that we're making 0 records for
    potential_zero_gap_transects <- dplyr::select(.data = actually_present,
                                                  PrimaryKey,
                                                  TRANSECT) |>
      dplyr::distinct() |>
      dplyr::mutate(.data = _,
                    present = TRUE) |>
      dplyr::left_join(x = potential_zero_gap_transects,
                       y = _,
                       by = c("PrimaryKey",
                              "TRANSECT")) |>
      dplyr::filter(.data = _,
                    is.na(present)) |>
      dplyr::select(.data = _,
                    -present)
  }

  # Create the 0 records
  if (nrow(potential_zero_gap_transects) > 0) {
    zero_gaps <- dplyr::mutate(.data = potential_zero_gap_transects,
                               SEQNUM = 1,
                               START_GAP = 0,
                               END_GAP = 0) |>
      dplyr::left_join(x = _,
                       y = dplyr::distinct(dplyr::select(.data = gintercept,
                                                         -tidyselect::all_of(c("TRANSECT",
                                                                               "SEQNUM",
                                                                               "GAP_TYPE",
                                                                               "START_GAP",
                                                                               "END_GAP")))),
                       by = c("PrimaryKey"))
    gintercept <- dplyr::bind_rows(gintercept,
                                   zero_gaps)
  }

  #### Cleanup and conversion ##################################################
  # We need to reformat this so that it's in the standard format.

  output <- dplyr::rename(.data = gintercept,
                          LineKey = TRANSECT,
                          RecType = GAP_TYPE,
                          GapStart = START_GAP,
                          GapEnd = END_GAP,
                          SeqNo = SEQNUM) |>
    dplyr::mutate(.data = _,
                  # Convert from decimal feet to centimeters
                  dplyr::across(.cols = tidyselect::all_of(c("GapStart",
                                                             "GapEnd")),
                                .fns = ~ .x * 12 * 2.54),
                  # The abs() shouldn't be necessary, but just to be safe!
                  Gap = abs(GapStart - GapEnd),
                  RecType = dplyr::case_when(stringr::str_detect(string = RecType,
                                                                 "peren") ~ "P",
                                             stringr::str_detect(string = RecType,
                                                                 "canopy") ~ "C",
                                             stringr::str_detect(string = RecType,
                                                                 "basal") ~ "B",
                                             .default = RecType),
                  # The 1 here indicates that units are metric
                  Measure = 1,
                  # LMF transects are always 150 feet, but we want that length
                  # in meters.
                  LineLengthAmount = 150 * 12 * 2.54 / 100,
                  # The minimum gap size in LMF is 12 inches but we want cm
                  GapMin = 12 * 2.54)

  # Return only the variables we want and in an order we want.
  output_vars <- c("PrimaryKey",
                   "LineKey",
                   "LineLengthAmount",
                   "Measure",
                   "GapMin",
                   "RecType",
                   "SeqNo",
                   "GapStart",
                   "GapEnd",
                   "Gap")
  output <- dplyr::select(.data = output,
                          tidyselect::all_of(output_vars))

  dplyr::distinct(output)
}

#' export gather_gap_survey123
#' rdname gather_gap
# gather_gap_survey123 <- function(Gap_0, GapDetail_1) {
#
#   # Add fields to match terradat schema
#   GapDetail_1$DBKey <- NA
#   GapDetail_1$SeqNo <- NA
#   Gap_0$DBKey <- NA
#   Gap_0$RecKey <- NA
#
#   # Select only the necessary fields
#   gap_detail <- GapDetail_1 %>%
#     dplyr::select(
#       GlobalID,
#       ParentGlobalID,
#       DBKey,
#       GapStart,
#       GapEnd,
#       RecType,
#       Gap,
#       SeqNo
#     )
#
#   gap_header <- Gap_0 %>%
#     dplyr::select(
#       GlobalID,
#       PrimaryKey = PlotKey,
#       DBKey,
#       LineKey,
#       RecKey,
#       FormDate,
#       Direction,
#       # Measure, # This is necessary but missing data
#       LineLengthAmount = LineLengthCM,
#       GapMin,
#       GapData, # Will have to be reclassified later
#       Notes = CollectNote,
#       NoCanopyGaps,
#       NoBasalGaps
#     )
#
#
#   ## ensure that gap, gapstart, and gapend are numeric
#   gap_detail$GapStart <- as.numeric(gap_detail$GapStart)
#   gap_detail$GapEnd <- as.numeric(gap_detail$GapEnd)
#   gap_detail$Gap <- as.numeric(gap_detail$Gap)
#   ##
#
#   # Merge header and detail data together
#   # Survey123 uses global ID, we have to translate it, then drop GlobalIDs
#   gap_tall <- dplyr::left_join(
#     x = gap_header,
#     y = gap_detail,
#     by = c("GlobalID" = "ParentGlobalID", "DBKey")
#   ) %>%
#     dplyr::select(-"GlobalID.y", -"GlobalID")
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- gap_tall$PrimaryKey[duplicated(gap_tall$PrimaryKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Remove all orphaned records
#   gap_tall <- gap_tall[!is.na(gap_tall$PrimaryKey), ]
#
#   # reclassify GapData from text to code
#   gap_tall$GapData <-
#     dplyr::recode(
#       gap_tall$GapData,
#       "Both" = 1, ### Probable bug here -- I do not know what survey123 codes this value as
#       "Canopy" = 2,
#       "Basal" = 3
#     )
#
#   # Look for NA values in NoCanopyGaps and NoBasalGaps, we assume they are 0
#   gap_tall <- gap_tall %>%
#     dplyr::mutate(
#       NoCanopyGaps = tidyr::replace_na(NoCanopyGaps, replace = 0),
#       NoBasalGaps = tidyr::replace_na(NoBasalGaps, replace = 0)
#     )
#
#   ## Add zero values where there is no canopy gap present on line
#   gap_tall[gap_tall$NoCanopyGaps == 1, ] <- gap_tall %>%
#     dplyr::filter(NoCanopyGaps == 1) %>%
#     tidyr::replace_na(list(
#       RecType = "C", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1
#     ))
#   gap_tall$NoCanopyGaps <- as.numeric(gap_tall$NoCanopyGaps)
#   gap_tall$NoBasalGaps <- as.numeric(gap_tall$NoBasalGaps)
#
#   ## Add zero values where there is no canopy gap present on the line, AND there is basal gap on the line
#   # Find missing records
#   gap_tall_missing_c <-
#     gap_tall %>%
#     dplyr::filter(NoCanopyGaps == 1,
#                   RecType != "C") %>%
#     dplyr::select(PrimaryKey, LineKey, RecKey, NoBasalGaps, NoCanopyGaps) %>%
#     unique() %>%
#     dplyr::mutate(RecType = "C", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1)
#
#   # Append them to gap_tall
#   gap_tall <-
#     dplyr::bind_rows(gap_tall, gap_tall_missing_c)
#
#   ## Add zero values where there is no basal gap present on line
#   gap_tall[gap_tall$NoBasalGaps == 1, ] <- gap_tall %>%
#     dplyr::filter(NoBasalGaps == 1) %>%
#     tidyr::replace_na(list(
#       RecType = "B", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1
#     ))
#
#   ## Add zero values where there is no basal gap present on the line, AND there is canopy gap on the line
#   # Find missing records
#   gap_tall_missing_b <-
#     gap_tall %>%
#     dplyr::filter(NoBasalGaps == 1,
#                   RecType != "B" | is.na(RecType)) %>%
#     dplyr::select(PrimaryKey, LineKey, RecKey, NoCanopyGaps, NoBasalGaps) %>%
#     unique() %>%
#     dplyr::mutate(RecType = "B", GapStart = 0, GapEnd = 0, Gap = 0, Measure = 1)
#
#   # Append them to gap_tall
#   gap_tall <-
#     dplyr::bind_rows(gap_tall, gap_tall_missing_b)
#
#   return(gap_tall)
# }
#

#' Convert wide gap data into a long/tall format.
#' @description
#' Given wide format line-point intercept or height data, create a tall
#' format data frame usable by other terradactyl functions. For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_gap_terradat]{gather_gap_terradat()}} and \code{\link[terradactyl:gather_gap_lmf]{gather_gap_lmf()}}.
#'
#' @param source Character string. The data source format. Must be one of \code{"AIM"}, \code{"TerrADat"}, \code{"DIMA"}, or \code{"LMF"} (case independent).
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblGapHeader} and \code{tblGapDetail} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the arguments \code{GINTERCEPT} and \code{POINT} are considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#'
#' @export
gather_gap <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblGapHeader = NULL,
                       tblGapDetail = NULL,
                       POINT = NULL,
                       GINTERCEPT = NULL,
                       autoQC = TRUE,
                       verbose = FALSE#,
                       # Gap_0 = NULL,
                       # GapDetail_1 = NULL
) {

  # Gather gap using the appropriate method
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    gap <- gather_gap_terradat(dsn = dsn,
                               tblGapDetail = tblGapDetail,
                               tblGapHeader = tblGapHeader,
                               verbose = verbose)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    gap <- gather_gap_lmf(dsn = dsn,
                          file_type = file_type,
                          POINT = POINT,
                          GINTERCEPT = GINTERCEPT,
                          verbose = verbose)
    # } else if(toupper(source) %in% c("SURVEY123")){
    # gap <- gather_gap_survey123(Gap_0 = Gap_0, GapDetail_1 = GapDetail_1)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  gap$source <- source

  if("sf" %in% class(gap)) gap <- sf::st_drop_geometry(gap)

  if (any(class(gap) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(gap)[do.call(rbind, vapply(gap,
                                                    class))[, 1] %in% c("POSIXct", "POSIXt")]
    gap <- dplyr::mutate_at(gap, dplyr::vars(change_vars),
                            dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  gap <- dplyr::select(.data = gap,
                       tidyselect::all_of(c("PrimaryKey",
                                            "LineKey")),
                       tidyselect::everything())

  # Drop rows with no data
  gap <- dplyr::filter(.data = gap,
                       !(is.na(Gap) &
                           is.na(GapEnd) &
                           is.na(GapMin) &
                           is.na(GapStart) &
                           is.na(LineKey) &
                           is.na(LineLengthAmount) &
                           is.na(Measure) &
                           is.na(RecType) &
                           is.na(SeqNo)))

  # remove duplicates and empty rows
  # if(autoQC){
  #   message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
  #   gap <- gap %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "gap")
  # }

  dplyr::distinct(.data = gap)
}


#### SOIL STABILITY ############################################################

#' Convert AIM-format soil stability data into a long/tall format
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param tblSoilStabDetail Optional data frame or character string. If provided, this must contain the
#'   expected soil stability data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblSoilStabDetail (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param tblSoilStabHeader Optional data frame or character string. If provided, this must contain the
#'   expected metadata for the soil stability data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblSoilStabHeader (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param auto_qc_warnings  Logical. If \code{TRUE} the function will test the
#'   data and metadata for duplicated records (records which are identical to
#'   each other across all critical variables) and orphaned records (records
#'   which appear in the data but do not have any corresponding metadata
#'   records), either of which can result in erroneous or unexpected outputs.
#'   The number of duplicated and orphaned records will be reported but without
#'   making any changes. Defaults to \code{TRUE}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the tables tblGapHeader and tblGapDetail
#' gather_soil_stability_terradat(dsn = "data_folder/aim_data.gdb")
#'
#' # Using CSV files for tblSoilStabHeader and tblSoilStabDetail.
#' # These two arguments can also take data frames, if the data have already
#' # been read in as data frames.
#' gather_soil_stability_terradat(tblSoilStabHeader = "data_folder/soilstability_headers.csv",
#'                                tblSoilStabDetail = "data_folder/soilstability_detail_records.csv")
#'
#' # Using data frames for tblSoilStabHeader and tblSoilStabDetail
#' aim_soilstabilityheader <- read.csv("data_folder/soilstability_headers.csv")
#' aim_soilstabilitydetail <- read.csv("data_folder/soilstability_detail_records.csv")
#' gather_soil_stability_terradat(tblSoilStabHeader = aim_soilstabilityheader,
#'                                tblSoilStabDetail = aim_soilstabilitydetail)
#'
#' @export
gather_soil_stability_terradat <- function(dsn = NULL,
                                           tblSoilStabDetail = NULL,
                                           tblSoilStabHeader = NULL,
                                           auto_qc_warnings = TRUE,
                                           verbose = FALSE) {

  # These are used for data management within a geodatabase and we're going to
  # drop them.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "DBKey",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")
  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblSoilStabHeader,
                               default_name = "tblSoilStabHeader",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  detail <- read_with_fallback(dsn = dsn,
                               tbl = tblSoilStabDetail,
                               default_name = "tblSoilStabDetail",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  # Clean these up!
  detail <- dplyr::select(.data = detail,
                          -tidyselect::any_of(internal_gdb_vars),
                          # -tidyselect::any_of("DBKey"),
                          # These are some book-keeping variables and don't
                          # contain relevant data.
                          -dplyr::matches(match = "^(In)|(Dip)|(DateLoaded)")) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  header <- dplyr::select(.data = header,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  # In some cases (due to bad DIMA defaults) empty rows may exist in DIMA data.
  # This finds all the records where there were no valid ratings and drops them.
  detail <- detail[which(apply(X = dplyr::select(.data = detail,
                                                 tidyselect::matches(match = "^Rating\\d+$")),
                               MARGIN = 1,
                               FUN = function(X){
                                 !all(is.na(as.vector(X)))
                               })), ]

  #### Automatic QC ############################################################
  if (auto_qc_warnings) {
    if (verbose) {
      message("Running automatic QC checks for duplicated or orphaned records.")
    }
    auto_qc_warning(header_data = header,
                    detail_data = detail,
                    uid_variables = list(header = c("PrimaryKey",
                                                    "RecKey"),
                                         detail = c("PrimaryKey",
                                                    "RecKey")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }

  #### Munging #################################################################
  # Convert to tall format
  detail_tall <- tidyr::pivot_longer(data = detail,
                                     cols = -tidyselect::all_of(c("PrimaryKey",
                                                                  "RecKey")),
                                     names_to = c("variable",
                                                  "Position"),
                                     # This is a goofy regex, but the first
                                     # group, (^.*), will snag everything
                                     # prior to the second group,
                                     # ((?<=[A-z])\\d+$), which finds all the
                                     # digits between the last letter and the
                                     # end of the string. That first group
                                     # is the variable and the second is the
                                     # variable position in the sampling box.
                                     names_pattern = "(^.*)((?<=[A-z])\\d+$)",
                                     values_to = "value",
                                     # We have to coerce everything to character to
                                     # accommodate the cover values which are
                                     # character strings.
                                     values_transform = as.character,
                                     values_drop_na = TRUE) |>
    dplyr::filter(.data = _,
                  value != "",
                  # Correctly remove non-hydrophobic records with 0 values
                  # and hydrophobic records with non-zero values
                  !(variable == "Hydro" & value != "0"),
                  !(variable != "Hydro" & value == "0")) |>
    dplyr::distinct()

  # This will make things wider. We can't just use tidyr::pivot_wider() because
  # we don't have unique identifiers for the values which means that the value
  # variable would contain a list, which is unhelpful for how people want to use
  # these. So we'll make a list and join them together.
  # The lapply() works through the values in "variable" (Rating, Veg, etc.) and
  # grabs only the records with identical "variable" values. It then renames the
  # "value" variable to use whatever the "variable" value is for that set of
  # data and drops the "variable" variable.
  detail_tidy <- lapply(X = unique(detail_tall$variable),
                        detail_tall = detail_tall,
                        FUN = function(X, detail_tall){
                          # Renaming the "value" variable and then
                          # dropping the "variable" variable.
                          dplyr::filter(.data = detail_tall,
                                        variable == X) |>
                            # A little clunky, but this way we
                            # don't have to hardcode any
                            # "variable" values.
                            dplyr::rename(.data = _,
                                          tidyselect::all_of(setNames(object = c("value"),
                                                                      nm = X))) |>
                            dplyr::select(.data = _,
                                          -variable)
                        }) |>
    # The purrr::reduce() then binds all those together according to the
    # identifying variables. The use of a full_join() makes sure we don't drop
    # anything that had incomplete data. Yet.
    purrr::reduce(.x = _,
                  .f = dplyr::full_join,
                  by = c("RecKey",
                         "PrimaryKey",
                         "Position")) |>
    # However, there are likely going to be records in the source data where
    # there were no ratings but somehow there were other kinds of values, so
    # those get dropped.
    dplyr::filter(.data = _,
                  !is.na(Rating)) |>
    # And the last bit is coercing things to numeric.
    dplyr::mutate(.data = _,
                  dplyr::across(.cols = -tidyselect::all_of(c("RecKey",
                                                              "PrimaryKey")),
                                # Can't trust the variables to be coercible to
                                # numeric without introducing NAs. Even though
                                # that'd be possible in a pristine data set,
                                # you'll probably never have one. So, we check
                                # to see if coercion does violence and only
                                # coerce variables we won't damage.
                                .fns = ~ if(any(suppressWarnings(is.na(as.numeric(.x))))) {
                                  .x
                                } else {
                                  as.numeric(.x)
                                }))

  # Merge soil stability detail and header tables
  soil_stability_tall <- suppressWarnings(dplyr::left_join(x = header,
                                                           y = detail_tidy,
                                                           # Not enforcing this relationship
                                                           # because there may be duplicate
                                                           # records and the user can go ahead
                                                           # with this anyway.
                                                           # relationship = "one-to-many",
                                                           by = c("RecKey",
                                                                  "PrimaryKey")))

  # Return final merged file
  return(soil_stability_tall)
}

#' Convert LMF-format soil stability data into a long/tall format
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param file_type Deprecated. This argument is no longer functional or necessary and is kept for backwards compatibility with legacy code.
#' @param SOILDISAG Optional data frame or character string. If provided, this must contain the
#'   expected soil stability intercept data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called SOILDISAG (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the table SOILDISAG.
#' gather_soil_stability_lmf(dsn = "data_folder/lmf_data.gdb")
#'
#' # Using a data frame for SOILDISAG.
#' lmf_soilstability <- read.csv("data_folder/SOILDISAG_detail_records.csv")
#' gather_soil_stability_lmf(SOILDISAG = lmf_soilstability)
#'
#' @export
gather_soil_stability_lmf <- function(dsn = NULL,
                                      file_type = "gdb",
                                      SOILDISAG = NULL,
                                      verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType",
                         "DBKey")

  #### Reading and cleanup #####################################################
  soildisag <- read_with_fallback(dsn = dsn,
                                  tbl = SOILDISAG,
                                  default_name = "SOILDISAG",
                                  regex = TRUE,
                                  best_guess = TRUE,
                                  accept_failure = FALSE,
                                  verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()

  #   # Make sure we can read in the data.
  # valid_file_types <- c("gdb", "txt")
  #
  # # We'll extract the extension if file_type isn't provided
  # if (is.null(file_type)){
  #   file_type <- tools::file_ext(dsn)
  # }
  # # Sanitize it.
  # file_type <- tolower(file_type)
  #
  #
  # if (!is.null(SOILDISAG)) {
  #   soildisag <- SOILDISAG
  # } else if (!is.null(dsn)) {
  #   if (!file.exists(dsn)) {
  #     stop(paste0("The provided dsn (", dsn, ") does not exist."))
  #   }
  #   if (!(file_type %in% valid_file_types)) {
  #     stop(paste0("The current file_type value (", file_type, ") is invalid. ",
  #                 "Valid file types are: ", paste(valid_file_types,
  #                                                 collapse = ", ")))
  #   }
  #   # Read in GINTERCEPT
  #   soildisag <- switch(file_type,
  #                       "gdb" = {
  #                         sf::st_read(dsn = dsn,
  #                                     layer = "SOILDISAG",
  #                                     stringsAsFactors = FALSE,
  #                                     quiet = TRUE)
  #                       },
  #                       "txt" = {
  #                         read.table(file = file.path(dsn,
  #                                                     "soildisag.txt"),
  #                                    stringsAsFactors = FALSE,
  #                                    strip.white = TRUE,
  #                                    header = FALSE,
  #                                    sep = "|")
  #                       })
  #
  #   # Update variable names
  #   if (file_type == "txt") {
  #     # Add meaningful column names
  #     soildisag <- name_variables_nri(data = soildisag,
  #                                     table_name = "SOILDISAG")
  #   }
  # } else {
  #   stop("Supply either a data frames for the argument SOILDISAG or the path to a GDB containing a table called SOILDISAG as the argument dsn.")
  # }
  #
  # # These are used for data management within a geodatabase and we're going to
  # # drop them. This helps us to weed out duplicate records created by quirks of
  # # the ingest processes.
  # internal_gdb_vars <- c("GlobalID",
  #                        "created_user",
  #                        "created_date",
  #                        "last_edited_user",
  #                        "last_edited_date",
  #                        "DateLoadedInDb",
  #                        "DateLoadedinDB",
  #                        "rid",
  #                        "DataErrorChecking",
  #                        "DataEntry",
  #                        "DateModified",
  #                        "FormType",
  #                        "DBKey")
  #
  # soildisag <- dplyr::select(.data = soildisag,
  #                            -tidyselect::any_of(internal_gdb_vars)) |>
  #   dplyr::distinct()

  #### Pivoting longer and harmonizing #########################################
  # This pares down the variables to only what we need and pivots to a long
  # format. It also splits variables into type and position.
  data_long <- dplyr::select(.data = soildisag,
                             PrimaryKey,
                             tidyselect::matches(match = "^VEG\\d+$"),
                             tidyselect::matches(match = "^STABILITY\\d+$")) |>
    tidyr::pivot_longer(data = _,
                        cols = -tidyselect::all_of("PrimaryKey"),
                        names_to = c("variable",
                                     "Position"),
                        # This just says that all the letters at the beginning
                        # of the variable name go into "variable" and all the
                        # digits at the end go into "Position"
                        names_pattern = "(^[A-z]+)(\\d+)",
                        values_to = "value",
                        # This is just for now so that we can combine the two
                        # classes of values (numeric and character).
                        values_transform = as.character) |>
    dplyr::mutate(.data = _,
                  variable = dplyr::case_when(variable == "VEG" ~ "Veg",
                                              variable == "STABILITY" ~ "Rating")) |>
    tidyr::pivot_wider(data = _,
                       names_from = variable) |>
    # And this makes sure that the values that should be numeric are.
    dplyr::mutate(.data = _,
                  dplyr::across(.cols = tidyselect::all_of(c("Position",
                                                             "Rating")),
                                .fns = as.numeric))

  # Return only valid records!
  dplyr::filter(.data = data_long,
                # Apparently we might have 0 ratings???
                !(Rating %in% c(0, NA))) |>
    dplyr::distinct()
}


# gather_soil_stability_survey123 <- function(dsn = NULL,
#                                             SoilStability_0 = NULL) {
#
#
#   if(!is.null(SoilStability_0)){
#     soil_stability_detail = SoilStability_0
#   } else if (!is.null(dsn)){
#     if (!file.exists(dsn)) {
#       stop("dsn must be a valid filepath to a geodatabase containing tblSoilStabDetail and tblSoilStabHeader")
#     }
#     soil_stability_detail <-
#       suppressWarnings(sf::st_read(dsn,
#                                    layer = "tblSoilStabDetail",
#                                    stringsAsFactors = FALSE, quiet = T
#       ))
#
#     soil_stability_header <-
#       suppressWarnings(sf::st_read(dsn,
#                                    layer = "tblSoilStabHeader",
#                                    stringsAsFactors = FALSE, quiet = T
#       ))
#   } else {
#     stop("Supply either tblSoilStabDetail and tblSoilStabHeader, or a path to a GDB containing those tables")
#   }
#
#   # Turn PlotKey into PrimaryKey
#   soil_stability_detail$PrimaryKey <- soil_stability_detail$PlotKey
#
#   # Pare down columns to only necessary ones
#   soil_stability_detail <- soil_stability_detail %>%
#     dplyr::select(
#       PrimaryKey, Veg1:Hydro6
#     )
#
#   # In some cases (due to bad DIMA defaults) empty rows may exist in DIMA data. Remove them.
#   soil_stability_detail <- soil_stability_detail %>%
#     dplyr::filter(
#       !(is.na(Rating1) & is.na(Rating2) & is.na(Rating3) & is.na(Rating4) & is.na(Rating5) & is.na(Rating6)))
#
#   # remove orphaned records
#   soil_stability_detail <-
#     soil_stability_detail[!is.na(soil_stability_detail$PrimaryKey), ]
#
#   # If DBKey Key exists, remove it
#   if ("DBKey" %in% colnames(soil_stability_detail)) {
#     soil_stability_detail <- dplyr::select(soil_stability_detail, -DBKey)
#   }
#
#   gathered <- soil_stability_detail %>%
#     # Remove standard columns (In and Dip Times and Date Downloaded in DB)
#     dplyr::select(.,
#                   match = -dplyr::starts_with("In"),
#                   -dplyr::starts_with("Dip"),
#                   -dplyr::starts_with("DateLoaded")
#     ) %>%
#
#     # Convert to tall format
#     tidyr::gather(.,
#                   key = variable, value = value,
#                   -PrimaryKey, na.rm = TRUE
#     )
#
#   # Remove blank values
#   gathered <- subset(gathered, value != "")
#
#   # Separate numerical suffixes from field type
#   gathered$key <- stringr::str_extract(
#     string = gathered$variable,
#     pattern = "^[A-z]+"
#   )
#   gathered$Position <- stringr::str_extract(
#     string = gathered$variable,
#     pattern = "[0-9]+"
#   )
#
#   gathered <- subset(gathered, select = -c(variable, BoxNum))
#
#   # Remove Hydro = 0
#   gathered <- gathered %>% subset(!(key == "Hydro" & value != 0))
#
#   # Spread the gathered data so that Line, Rating, Vegetation,
#   # and Hydro are all different variables
#
#   soil_stability_detail_list <- lapply(
#     X = as.list(unique(gathered$key)),
#     FUN = function(k = as.list(unique(gathered$key)), df = gathered) {
#       test <- df[df$key == k, ] %>%
#         dplyr::mutate(id = 1:dplyr::n()) %>%
#         tidyr::spread(key = key, value = value) %>%
#         dplyr::select(-id)
#     }
#   )
#   # create a single tidy dataframe
#   soil_stability_detail_tidy <- purrr::reduce(
#     soil_stability_detail_list,
#     dplyr::full_join, by = c("RecKey", "PrimaryKey", "Position")
#   ) %>% unique()
#
#   soil_stability_detail_tidy$Rating <- soil_stability_detail_tidy$Rating %>%
#     as.numeric()
#
#   # Merge soil stability detail and header tables
#   soil_stability_tall <- dplyr::left_join(
#     soil_stability_header,
#     soil_stability_detail_tidy, by = c("RecKey", "PrimaryKey")
#   ) %>% dplyr::select_if(!names(.) %in% c(
#     'PlotKey', 'DateModified', 'FormType', 'DataEntry', 'DataErrorChecking', 'DateLoadedInDb'
#   )
#   )
#
#   # In some cases, the Hydro variable is lost. Add it back in
#   if (!"Hydro" %in% colnames(soil_stability_tall)){
#     soil_stability_tall$Hydro <- NA
#   }
#
#   # Return final merged file
#   return(soil_stability_tall)
# }
#

# Wrapper function for all soil stability gather functions
#' Convert soil stability data into tall, tidy data frame
#'
#' @description Given soil stability create a tall format data frame usable by
#' other terradactyl functions. For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_soil_stability_terradat]{gather_soil_stability_terradat()}} and \code{\link[terradactyl:gather_soil_stability_lmf]{gather_soil_stability_lmf()}}.
#' @inheritParams gather_soil_stability_terradat
#' @inheritParams gather_soil_stability_lmf
#' @param source Character string. The data source format, can be \code{"AIM"},
#' \code{"TerrADat"}, \code{"DIMA"}, \code{"LMF"}, or \code{"NRI"} (case independent).
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblSoilStabHeader} and \code{tblSoilStabDetail} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the argument \code{SOILDISAG} is considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#' @return A tall data frame containing soil horizon data.
#' @examples
#' gather_soil_stability(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                       source = "AIM")
#' gather_soil_stability(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                       source = "LMF")
#'
#' aim_soilstabdetail <- read.csv("Path/To/tblSoilStabDetail.csv")
#' aim_soilstabheader <- read.csv("Path/To/tblSoilStabHeader.csv")
#' gather_soil_stability(source = "AIM",
#'                       tblSoilStabDetail = aim_soilstabdetail,
#'                       tblSoilStabHeader = aim_soilstabheader)
#'
#' lmf_horizons <- read.csv("Path/To/SOILHORIZON.csv")
#' gather_soil_stability(source = "LMF",
#'                       SOILDISAG = lmf_horizons)
#' @export
gather_soil_stability <- function(dsn = NULL,
                                  source,
                                  file_type = "gdb",
                                  tblSoilStabDetail = NULL,
                                  tblSoilStabHeader = NULL,
                                  SOILDISAG = NULL,
                                  autoQC = TRUE,
                                  verbose = FALSE
) {

  if (toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    soil_stability <- gather_soil_stability_terradat(dsn = dsn,
                                                     tblSoilStabDetail = tblSoilStabDetail,
                                                     tblSoilStabHeader = tblSoilStabHeader,
                                                     verbose = verbose)
  } else if (toupper(source) %in% c("LMF", "NRI")){
    soil_stability <- gather_soil_stability_lmf(dsn = dsn,
                                                file_type = file_type,
                                                SOILDISAG = SOILDISAG,
                                                verbose = verbose)

  } else {
    stop("source must be one of 'AIM', 'TerrADat', 'DIMA', 'LMF', or 'NRI' (all case independent).")
  }

  soil_stability$source <- source

  if("sf" %in% class(soil_stability)) {
    soil_stability <- sf::st_drop_geometry(x = soil_stability)
  }

  if (any(class(soil_stability) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(soil_stability)[do.call(rbind, vapply(soil_stability,
                                                               class))[, 1] %in% c("POSIXct", "POSIXt")]
    soil_stability <- dplyr::mutate_at(soil_stability, dplyr::vars(change_vars),
                                       dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  soil_stability <- dplyr::select(.data = soil_stability,
                                  PrimaryKey,
                                  tidyselect::everything())

  # Drop rows with no data
  soil_stability <- dplyr::filter(.data = soil_stability,
                                  !(is.na(Position) &
                                      is.na(Rating) &
                                      is.na(Veg)))

  # remove duplicates and empty rows
  # if(autoQC){
  #   message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
  #   soil_stability <- soil_stability %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "soilstab")
  # }

  dplyr::distinct(.data = soil_stability)
}

#### INTERPRETING INDICATORS OF RANGELAND HEALTH ###############################
#' Convert AIM-format Interpreting Indicators of RangelandHealth data into a long/tall format.
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes.
#' @param tblQualDetail Optional data frame or character string. If provided, this must contain the
#'   expected IIRH data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblQualDetail (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param tblQualHeader Optional data frame or character string. If provided, this must contain the
#'   expected metadata for the IIRH data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblQualHeader (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the tables tblQualHeader and tblQualDetail
#' gather_rangeland_health_terradat(dsn = "data_folder/aim_data.gdb")
#'
#' # Using data frames for tblQualHeader and tblQualDetail
#' aim_iirhheader <- read.csv("data_folder/iirh_headers.csv")
#' aim_iirhdetail <- read.csv("data_folder/iirh_detail_records.csv")
#' gather_rangeland_health_terradat(tblQualHeader = aim_iirhheader,
#'                     tblQualDetail = aim_iirhdetail)
#'
#'
#'
#' @export
gather_rangeland_health_terradat <- function(dsn = NULL,
                                             tblQualHeader = NULL,
                                             tblQualDetail = NULL,
                                             verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "DBKey",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")
  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblQualHeader,
                               default_name = "tblQualHeader",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  detail <- read_with_fallback(dsn = dsn,
                               tbl = tblQualDetail,
                               default_name = "tblQualDetail",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    # Just making sure these are for real numeric.
    dplyr::mutate(.data = _,
                  dplyr::across(.cols = tidyselect::all_of(x = c("Seq",
                                                                 "Rating")),
                                .fns = as.numeric)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))


  # if(!is.null(tblQualHeader) & !is.null(tblQualDetail)){
  #   IIRH_header <- tblQualHeader
  #   IIRH_detail <- tblQualDetail
  # } else if(!is.null(dsn)) {
  #   # check file
  #   if (!file.exists(dsn)) {
  #     stop("dsn must be a valid filepath to a geodatabase containing tblQualDetail and tblQualHeader")
  #   }
  #
  #   # Read in tblQualHeader
  #   rh_header <- sf::st_read(dsn = dsn,
  #                            layer = "tblQualHeader",
  #                            stringsAsFactors = FALSE,
  #                            quiet = TRUE)
  #
  #   # Read in tblQualDetail
  #   rh_detail <- sf::st_read(dsn = dsn,
  #                            layer = "tblQualDetail",
  #                            stringsAsFactors = FALSE,
  #                            quiet = TRUE)
  #
  # } else {
  #   stop("Provide either tblQualHeader and tblQualDetail or a path to a geodatabase containing those tables")
  # }

  # The data are coming in with numeric variables identifying the indicators and
  # values, but we need to convert those into human-legible character strings
  # for the output.
  # The variable names are ordered by their corresponding Seq values to easily
  # build the lookup table.
  indicator_names <- c("RH_Rills",
                       "RH_WaterFlowPatterns",
                       "RH_PedestalsTerracettes",
                       "RH_BareGround",
                       "RH_Gullies",
                       "RH_WindScouredAreas",
                       "RH_LitterMovement",
                       "RH_SoilSurfResisErosion",
                       "RH_SoilSurfLossDeg",
                       "RH_PlantCommunityComp",
                       "RH_Compaction",
                       "RH_FuncSructGroup",
                       "RH_DeadDyingPlantParts",
                       "RH_LitterAmount",
                       "RH_AnnualProd",
                       "RH_InvasivePlants",
                       "RH_ReprodCapabilityPeren")
  indicator_names_lookup <- data.frame(Seq = seq_len(length(indicator_names)),
                                       indicator = indicator_names)
  # And value strings are ordered by the numeric values they correspond to.
  # NA corresponds to 0, so the table uses seq_len() - 1.
  value_strings <- c(NA,
                     "NS",
                     "SM",
                     "M",
                     "ME",
                     "ET")
  value_strings_lookup <- data.frame(Rating = seq_len(length(value_strings)) - 1,
                                     rating_string = value_strings)

  detail_wide <- dplyr::left_join(x = detail,
                                  y = indicator_names_lookup,
                                  by = "Seq",
                                  relationship = "many-to-one") |>
    dplyr::left_join(x = _,
                     y = value_strings_lookup,
                     by = "Rating",
                     relationship = "many-to-one") |>
    dplyr::mutate(.data = _,
                  Rating = rating_string) |>
    dplyr::filter(.data = _,
                  !is.na(Rating)) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(x = c("RecKey",
                                           "indicator",
                                           "Rating"))) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(data = _,
                       names_from = tidyselect::all_of(x = c("indicator")),
                       values_from = tidyselect::all_of(x = c("Rating")))

  output <- dplyr::select(.data = header,
                          # Renaming the various header variables that need it
                          tidyselect::all_of(x = c("PrimaryKey",
                                                   "RecKey",
                                                   "RH_HydrologicFunction" = "HFVxWRatingFinal",
                                                   "RH_BioticIntegrity" = "BIVxWRatingFinal",
                                                   "RH_SoilSiteStability" = "SSSVxWRatingFinal",
                                                   "RH_CommentsBI" = "CommentBI",
                                                   "RH_CommentsHF" = "CommentHF",
                                                   "RH_CommentsSS" = "CommentSSS"))) |>
    dplyr::left_join(x = _,
                     y = detail_wide,
                     by = "RecKey",
                     relationship = "one-to-many")

  output
  # # Clean up the Indicators Table
  # rangeland_health_indicators <- IIRH_detail %>%
  #   dplyr::mutate(
  #     indicator = Seq %>%
  #       as.character() %>%
  #       # Rename Seq from a number to an Indicator name
  #       stringr::str_replace_all(c(
  #         "\\b1\\b" = "RH_Rills",
  #         "\\b2\\b" = "RH_WaterFlowPatterns",
  #         "\\b3\\b" = "RH_PedestalsTerracettes",
  #         "\\b4\\b" = "RH_BareGround",
  #         "\\b5\\b" = "RH_Gullies",
  #         "\\b6\\b" = "RH_WindScouredAreas", #
  #         "\\b7\\b" = "RH_LitterMovement", #
  #         "\\b8\\b" = "RH_SoilSurfResisErosion", #
  #         "\\b9\\b" = "RH_SoilSurfLossDeg", #
  #         "\\b10\\b" = "RH_PlantCommunityComp", #
  #         "\\b11\\b" = "RH_Compaction", #
  #         "\\b12\\b" = "RH_FuncSructGroup", #
  #         "\\b13\\b" = "RH_DeadDyingPlantParts", #
  #         "\\b14\\b" = "RH_LitterAmount", #
  #         "\\b15\\b" = "RH_AnnualProd", #
  #         "\\b16\\b" = "RH_InvasivePlants", #
  #         "\\b17\\b" = "RH_ReprodCapabilityPeren"
  #       )),
  #     Rating = Rating %>%
  #       as.character() %>%
  #       stringr::str_replace_all(c(
  #         "1" = "NS",
  #         "2" = "SM",
  #         "3" = "M",
  #         "4" = "ME",
  #         "5" = "ET",
  #         "0" = NA
  #       ))
  #   ) %>%
  #   subset(!is.na(Rating)) %>%
  #   dplyr::select(RecKey, indicator, Rating) %>%
  #   dplyr::distinct() %>%
  #   tidyr::spread(key = indicator, value = Rating)
  #
  # # Attributes and then joined to Indicators
  # IIRH <- dplyr::select(IIRH_header, DBKey, PrimaryKey, RecKey, DateLoadedInDb,
  #                       RH_HydrologicFunction = HFVxWRatingFinal,
  #                       RH_BioticIntegrity = BIVxWRatingFinal,
  #                       RH_SoilSiteStability = SSSVxWRatingFinal,
  #                       RH_CommentsBI = CommentBI,
  #                       RH_CommentsHF = CommentHF,
  #                       RH_CommentsSS = CommentSSS#,
  #                       # Observer,
  #                       # Recorder
  # ) %>%
  #
  #   # Add the indicators
  #   dplyr::left_join(rangeland_health_indicators, by = "RecKey")
  #
  # ## last drop
  # IIRH <- IIRH %>% dplyr::select(
  #   -c(DateLoadedInDb)
  # )
  #
  # return(IIRH)
}

#' Convert LMF-format Interpreting Indicators of RangelandHealth data into a long/tall format.
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param file_type Deprecated. This argument is no longer functional or necessary and is kept for backwards compatibility with legacy code.
#' @param RANGEHEALTH Optional data frame or character string. If provided, this must contain the
#'   expected rangeland health intercept data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called RANGEHEALTH (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the table RANGEHEALTH.
#' gather_rangeland_health_lmf(dsn = "data_folder/lmf_data.gdb")
#'
#' # Using a data frame for RANGEHEALTH.
#' lmf_iirh <- read.csv("data_folder/RANGEHEALTH_detail_records.csv")
#' gather_rangeland_health_lmf(RANGEHEALTH = lmf_iirh)
#'
#' @export
gather_rangeland_health_lmf <- function(dsn = NULL,
                                        file_type = NULL,
                                        RANGEHEALTH = NULL,
                                        verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType",
                         "DBKey")

  #### Reading #################################################################
  rangehealth <- read_with_fallback(dsn = dsn,
                                    tbl = RANGEHEALTH,
                                    default_name = "RANGEHEALTH",
                                    regex = TRUE,
                                    best_guess = TRUE,
                                    accept_failure = FALSE,
                                    verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()
  # if ("character" %in% class(RANGEHEALTH)) {
  #   if (tools::file_ext(RANGEHEALTH) == "Rdata") {
  #     IIRH <- readRDS(file = RANGEHEALTH)
  #   } else {
  #     stop("When RANGEHEALTH is a character string it must be the path to a .Rdata file containing RANGEHEALTH data.")
  #   }
  # } else if ("data.frame" %in% class(RANGEHEALTH)) {
  #   IIRH <- RANGEHEALTH
  # } else {
  #   if (!is.null(dsn)) {
  #     if (!file.exists(dsn)) {
  #       stop("dsn must be a valid filepath to a geodatabase containing a table called RANGEHEALTH or the filepath to a text file containing those values.")
  #     }
  #
  #     file_type <- tools::file_ext(x = dsn) |>
  #       toupper()
  #
  #     IIRH <- switch(file_type,
  #                    "gdb" = {
  #                      sf::st_read(dsn = dsn,
  #                                  layer = "RANGEHEALTH",
  #                                  stringsAsFactors = FALSE, quiet = TRUE)
  #                    },
  #                    "txt" = {
  #                      read.table(paste(dsn, "rangehealth.txt", sep = ""),
  #                                 stringsAsFactors = FALSE,
  #                                 header = FALSE,
  #                                 sep = "|",
  #                                 strip.white = TRUE
  #                      )
  #                    },
  #                    "csv" = {
  #                      read.csv(dsn)
  #                    }
  #     )
  #     # if it is in a text file, there are no field names assigned.
  #     if (file_type == "txt") {
  #       IIRH <- name_variables_nri(
  #         data = IIRH,
  #         table_name = "RHSUMMARY"
  #       )
  #     }
  #   }else {
  #     stop("Provide either dsn or RANGEHEALTH.")
  #   }

  # Clean up the field names so they are human readable and match TerrAdat names
  output <- dplyr::select(.data = rangehealth,
                          tidyselect::all_of(x = c("PrimaryKey",
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
                                                   RH_HydrologicFunction = "HYDROLOGIC_FUNCTION")))

  output
  # }
}

#' export gather_rangeland_health_survey123
#' rdname IIRH
# gather_rangeland_health_survey123 <- function(PlotObservation_0 = NULL) {
#
#   # Clean up the Indicators Table
#   rangeland_health_indicators <- PlotObservation_0 %>%
#     dplyr::select(PrimaryKey = PlotKey,
#                   FormDate,
#                   RH_Rills = Rills,
#                   RH_Gullies = Gullies,
#                   RH_PedestalsTerracettes = Pedestals,
#                   RH_WindScouredAreas = Deposition)
#     dplyr::mutate(
#       indicator = Seq %>%
#         as.character() %>%
#         # Rename Seq from a number to an Indicator name
#         stringr::str_replace_all(c(
#           "\\b1\\b" = "RH_Rills",
#           "\\b2\\b" = "RH_WaterFlowPatterns",
#           "\\b3\\b" = "RH_PedestalsTerracettes",
#           "\\b4\\b" = "RH_BareGround",
#           "\\b5\\b" = "RH_Gullies",
#           "\\b6\\b" = "RH_WindScouredAreas", #
#           "\\b7\\b" = "RH_LitterMovement", #
#           "\\b8\\b" = "RH_SoilSurfResisErosion", #
#           "\\b9\\b" = "RH_SoilSurfLossDeg", #
#           "\\b10\\b" = "RH_PlantCommunityComp", #
#           "\\b11\\b" = "RH_Compaction", #
#           "\\b12\\b" = "RH_FuncSructGroup", #
#           "\\b13\\b" = "RH_DeadDyingPlantParts", #
#           "\\b14\\b" = "RH_LitterAmount", #
#           "\\b15\\b" = "RH_AnnualProd", #
#           "\\b16\\b" = "RH_InvasivePlants", #
#           "\\b17\\b" = "RH_ReprodCapabilityPeren"
#         )),
#       Rating = Rating %>%
#         as.character() %>%
#         stringr::str_replace_all(c(
#           "1" = "NS",
#           "2" = "SM",
#           "3" = "M",
#           "4" = "ME",
#           "5" = "ET",
#           "0" = NA
#         ))
#     ) %>%
#     subset(!is.na(Rating)) %>%
#     dplyr::select(RecKey, indicator, Rating) %>%
#     dplyr::distinct() %>%
#     tidyr::spread(key = indicator, value = Rating)
#
#   # Attributes and then joined to Indicators
#   IIRH <- dplyr::select(IIRH_header, DBKey, PrimaryKey, RecKey, DateLoadedInDb,
#                         RH_HydrologicFunction = HFVxWRatingFinal,
#                         RH_BioticIntegrity = BIVxWRatingFinal,
#                         RH_SoilSiteStability = SSSVxWRatingFinal,
#                         RH_CommentsBI = CommentBI,
#                         RH_CommentsHF = CommentHF,
#                         RH_CommentsSS = CommentSSS#,
#                         # Observer,
#                         # Recorder
#   ) %>%
#
#     # Add the indicators
#     dplyr::left_join(rangeland_health_indicators, by = "RecKey")
#
#   ## last drop
#   IIRH <- IIRH %>% dplyr::select(
#     -c(DateLoadedInDb)
#   )
#
#   return(IIRH)
# }




#' Convert Interpreting Indicators of Rangeland Health (IIRH) data into a tall,
#' tidy data frame
#'
#' @description
#' Given wide format species richness data, create a tall
#' format data frame usable by other terradactyl functions. For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_rangeland_health_terradat]{gather_rangeland_health_terradat()}} and \code{\link[terradactyl:gather_rangeland_health_lmf]{gather_rangeland_health_lmf()}}.
#'
#' @inheritParams gather_rangeland_health_terradat
#' @inheritParams gather_rangeland_health_lmf
#' @param source Character string. The data source format. Must be one of \code{"AIM"}, \code{"TerrADat"}, \code{"DIMA"}, or \code{"LMF"} (case independent).
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblQualHeader} and \code{tblQualDetail} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the argument \code{RANGEHEALTH} is considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#'
#' @returns A tall data frame containing the data from the rangeland health
#' measurements.
#' @examples
#' gather_rangeland_health(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                         source = "AIM")
#' gather_rangeland_health(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                         source = "LMF")
#'
#' aim_rhdetail <- read.csv("Path/To/tblQualDetail.csv")
#' aim_rhheader <- read.csv("Path/To/tblQualHeader.csv")
#' gather_rangeland_health(source = "AIM",
#'                         tblQualDetail = aim_rhdetail,
#'                         tblQualHeader = aim_rhheader)
#'
#' lmf_rh <- read.csv("Path/To/RANGEHEALTH.csv")
#' gather_rangeland_health(source = "LMF",
#'                         RANGEHEALTH = lmf_rh)
#' @export
gather_rangeland_health <- function(dsn = NULL,
                                    source,
                                    file_type = NULL,
                                    tblQualHeader = NULL,
                                    tblQualDetail = NULL,
                                    RANGEHEALTH = NULL,
                                    autoQC = TRUE,
                                    verbose = FALSE) {


  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    IIRH <- gather_rangeland_health_terradat(dsn = dsn,
                                             tblQualDetail = tblQualDetail,
                                             tblQualHeader = tblQualHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    IIRH <- gather_rangeland_health_lmf(dsn = dsn,
                                        file_type = file_type,
                                        RANGEHEALTH = RANGEHEALTH)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # IIRH$source <- toupper(source)
  if(nrow(IIRH) > 0) IIRH$source <- source

  if("sf" %in% class(IIRH)) IIRH <- sf::st_drop_geometry(IIRH)

  if (any(class(IIRH) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(IIRH)[do.call(rbind, vapply(IIRH,
                                                     class))[, 1] %in% c("POSIXct", "POSIXt")]
    IIRH <- dplyr::mutate_at(IIRH, dplyr::vars(change_vars),
                             dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  IIRH <- dplyr::select(.data = IIRH,
                        tidyselect::any_of(x = c("PrimaryKey",
                                                 "DBKey")),
                        tidyselect::everything())

  # remove duplicates and empty rows
  # if(autoQC){
  #   message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
  #   IIRH <- IIRH %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "rh")
  # }

  return(IIRH)
}


#### SPECIES INVENTORY #########################################################
#' Convert AIM-format species inventory data into a long/tall format
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param tblSpecRichDetail Optional data frame or character string. If provided, this must contain the
#'   expected species inventory data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblSpecRichDetail (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param tblSpecRichHeader Optional data frame or character string. If provided, this must contain the
#'   expected metadata for the species inventory data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called tblSpecRichHeader (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @export
gather_species_inventory_terradat <- function(dsn = NULL,
                                              tblSpecRichDetail = NULL,
                                              tblSpecRichHeader = NULL,
                                              verbose = FALSE) {

  # These are used for data management within a geodatabase and we're going to
  # drop them.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "DBKey",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")
  #### Reading #################################################################
  header <- read_with_fallback(dsn = dsn,
                               tbl = tblSpecRichHeader,
                               default_name = "tblSpecRichHeader",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  detail <- read_with_fallback(dsn = dsn,
                               tbl = tblSpecRichDetail,
                               default_name = "tblSpecRichDetail",
                               regex = TRUE,
                               best_guess = TRUE,
                               accept_failure = FALSE,
                               verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey))

  # Clean these up!
  detail <- dplyr::select(.data = detail,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  header <- dplyr::select(.data = header,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::filter(.data = _,
                  !is.na(PrimaryKey)) |>
    dplyr::distinct()

  #### Munging #################################################################
  # This shouldn't be necessary as of 2025, but working with older data requires
  # that the species be separated because they're stored as a single character
  # string with each species code separated by semicolons.
  detail_tall <- tall_species(species_inventory_detail = detail)

  # Join with header data and strip out NA codes
  output <- dplyr::left_join(x = header,
                             y = detail_tall,
                             by = c("RecKey", "PrimaryKey")) |>
    dplyr::filter(.data = _,
                  !is.na(Species))

  output
}


#' Create distinct species inventory records from semicolon-separated values
#' @description
#' Some species inventory data stored all species codes in a single character string as semicolon-separated values. This will create distinct records for each code.
#' This will only work with a data frame that has the variables PrimaryKey, RecKey, and SpeciesList.
#'
#' @param species_inventory_detail Data frame. The detail species inventory data to split into multiple records. Must contain the variables PrimaryKey, RecKey, and SpeciesList. SpeciesList must contain character strings composed of the species codes separated by semicolons, e.g., "ARTRW8;PRGL2;ARTR4".
#' @returns A data frame with the variables PrimaryKey, RecKey, and Species where each record represents one species code and its associated PrimaryKey and RecKey values.
#' @export
tall_species <- function(species_inventory_detail) {
  tall_list <- apply(X = species_inventory_detail,
                     MARGIN = 1,
                     FUN = function(X) {
                       current_species_string <- X["SpeciesList"]
                       # message(class(current_species_string))
                       # split species strings concatenated in a single field
                       current_codes <- stringr::str_split(current_species_string,
                                                           pattern = ";") |>
                         unlist()

                       data.frame("PrimaryKey" = X["PrimaryKey"],
                                  "RecKey" = X["RecKey"],
                                  "Species" = current_codes)
                     })
  # Combine output
  output <- dplyr::bind_rows(tall_list) |>
    dplyr::filter(.data = _,
                  !(Species %in% c("", NA)))

  output
}

# Gather LMF data
#' Convert LMF-format soil stability data into a long/tall format
#'
#' @param dsn Optional character string. If provided, this must be the filepath
#'   to a geodatabase which contains the relevant feature classes. Defaults to \code{NULL}.
#' @param file_type Deprecated. This argument is no longer functional or necessary and is kept for backwards compatibility with legacy code.
#' @param PLANTCENSUS Optional data frame or character string. If provided, this must contain the
#'   expected species data. If \code{NULL} then the argument
#'   \code{dsn} must be provided. If a character string, this must either correspond to the filepath to a CSV or RDATA file containing a table with the data or the name of the feature class in the geodatabase provided as \code{dsn}. If \code{NULL}, the function will attempt to find a feature class called PLANTCENSUS (making a best guess if there's a partial match) in the \code{dsn} geodatabase. Defaults to \code{NULL}.
#' @param verbose  Logical. If \code{TRUE} then the function will report back
#'   diagnostic information as console messages while it works. Defaults to
#'   \code{FALSE}.
#' @examples
#' # Using a geodatabase that contains the table SOILDISAG.
#' gather_species_inventory_lmf(dsn = "data_folder/lmf_data.gdb")
#'
#' # Using a data frame for SOILDISAG.
#' lmf_plantcensus <- read.csv("data_folder/PLANTCENSUS_detail_records.csv")
#' gather_species_inventory_lmf(PLANTCENSUS = lmf_plantcensus)
#'
#' @export
gather_species_inventory_lmf <- function(dsn = NULL,
                                         file_type = "gdb",
                                         PLANTCENSUS = NULL,
                                         verbose = FALSE) {
  # These are used for data management within a geodatabase and we're going to
  # drop them. This helps us to weed out duplicate records created by quirks of
  # the ingest processes.
  internal_gdb_vars <- c("GlobalID",
                         "created_user",
                         "created_date",
                         "last_edited_user",
                         "last_edited_date",
                         "DateLoadedInDb",
                         "DateLoadedinDB",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType",
                         "DBKey")

  #### Reading #################################################################
  plantcensus <- read_with_fallback(dsn = dsn,
                                    tbl = PLANTCENSUS,
                                    default_name = "PLANTCENSUS",
                                    regex = TRUE,
                                    best_guess = TRUE,
                                    accept_failure = FALSE,
                                    verbose = verbose) |>
    dplyr::select(.data = _,
                  -tidyselect::any_of(x = internal_gdb_vars)) |>
    dplyr::distinct()
  # if(!is.null(PLANTCENSUS)){
  #   plantcensus <- PLANTCENSUS
  # } else if(!is.null(dsn)){
  #
  #   plantcensus <- switch(file_type,
  #                         "gdb" = {
  #                           suppressWarnings(sf::st_read(dsn,
  #                                                        layer = "PLANTCENSUS",
  #                                                        stringsAsFactors = FALSE, quiet = T
  #                           ))
  #                         },
  #                         "txt" = {
  #                           read.table(paste(dsn, "plantcensus.txt", sep = ""),
  #                                      stringsAsFactors = FALSE,
  #                                      header = FALSE, sep = "|",
  #                                      strip.white = TRUE
  #                           )
  #                         },
  #                         "csv" = {
  #                           read.csv(dsn,
  #                                    stringsAsFactors = FALSE
  #                           )
  #                         }
  #   )
  #
  #   # if it is in a text file, there are no field names assigned.
  #   if (file_type == "txt") {
  #     plantcensus <- name_variables_nri(
  #       data = plantcensus,
  #       table_name = "PLANTCENSUS"
  #     )
  #   }
  #
  # } else {
  #   stop("Supply either PLANTCENSUS or the path to a GDB containing that table")
  # }

  #### Munging #################################################################
  species_inventory <- dplyr::summarize(.data = plantcensus,
                                        .by = "PrimaryKey",
                                        SpeciesCount = dplyr::n()) |>
    dplyr::inner_join(x = _,
                      y = plantcensus,
                      by = "PrimaryKey",
                      relationship = "one-to-many")

  # rename fields
  species_inventory <- dplyr::rename(.data = species_inventory,
                                     Species = CPLANT)


  dplyr::select(.data = species_inventory,
                -c(SURVEY:SEQNUM),
                -tidyselect::any_of(internal_gdb_vars))

  species_inventory
}

#' export gather_species_inventory_survey123
#' rdname gather_species_inventory
# gather_species_inventory_survey123 <- function(SpeciesRichness_0 = NULL,
#                                                SpecRichDetail_1 = NULL) {
#
#   species_inventory_detail <- SpecRichDetail_1
#   species_inventory_header <- SpeciesRichness_0
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- species_inventory_header$PlotKey[duplicated(species_inventory_header$PlotKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   # Add null DBKey column if not present
#   if(!("DBKey" %in% colnames(species_inventory_header))) species_inventory_header$DBKey <- NA
#   if(!("DBKey" %in% colnames(species_inventory_detail))) species_inventory_detail$DBKey <- NA
#
#   # Convert PlotKey to PrimaryKey and attach to detail
#   species_inventory_header$PrimaryKey <- species_inventory_header$PlotKey
#   species_inventory_detail <- dplyr::left_join(species_inventory_detail,
#                                                species_inventory_header %>% dplyr::select(PrimaryKey, GlobalID),
#                                                by = c("ParentGlobalID" = "GlobalID"))
#
#   # Make Species Inventory Detail  a tall dataframe
#   species_detail_tall <- tall_species(species_inventory_detail = species_inventory_detail)
#
#   # Join with header data and strip out NA codes
#   species_inventory_tall <- dplyr::left_join(
#     x = species_inventory_header,
#     y = species_detail_tall#,
#     # by = c("RecKey", "PrimaryKey")
#   ) %>%
#     subset(!is.na(Species)) %>%
#     dplyr::select_if(!names(.) %in%
#                        c("DateModified", "FormType", "DataEntry",
#                          "DataErrorChecking", "DateLoadedInDb", "created_user", "created_date", "last_edited_user", "last_edited_date", "GlobalID")
#     )
#
#   return(species_inventory_tall)
# }


#' Species Inventory Gather wrapper
#' @description
#' Given wide format species richness data, create a tall
#' format data frame usable by other terradactyl functions. For additional information about
#' arguments, see the documentation for the functions \code{\link[terradactyl:gather_species_inventory_terradat]{gather_species_inventory_terradat()}} and \code{\link[terradactyl:gather_species_inventory_lmf]{gather_species_inventory_lmf()}}.
#'
#' @inheritParams gather_species_inventory_terradat
#' @inheritParams gather_species_inventory_lmf
#' @param source Character string. The data source format. Must be one of \code{"AIM"}, \code{"TerrADat"}, \code{"DIMA"}, or \code{"LMF"} (case independent).
#' @details
#' The \code{source} argument determines which other arguments are used or ignored.
#'
#' When \code{source} is one of \code{"AIM"}, \code{"TERRADAT"}, or \code{"DIMA"}
#' then the arguments \code{tblSpecRichHeader} and \code{tblSpecRichDetail} are both considered.
#'
#' When \code{source} is \code{"LMF"} then the argument \code{PLANTCENSUS} is considered.
#'
#' Regardless of the value of \code{source}, the data sources represented by those other arguments are required. The simplest way to provide them is to provide the filepath to a geodatabase as \code{dsn} with each of those feature classes appearing by the same name as the corresponding argument in that geodatabase.
#' @returns A tall data frame containing the data from the species inventory inputs.
#' @export
gather_species_inventory <- function(dsn = NULL,
                                     source,
                                     tblSpecRichDetail = NULL,
                                     tblSpecRichHeader = NULL,
                                     PLANTCENSUS = NULL,
                                     file_type = "gdb",
                                     autoQC = TRUE,
                                     verbose = FALSE) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    species_inventory <- gather_species_inventory_terradat(dsn = dsn,
                                                           tblSpecRichDetail = tblSpecRichDetail,
                                                           tblSpecRichHeader = tblSpecRichHeader,
                                                           verbose = verbose)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    species_inventory <- gather_species_inventory_lmf(dsn = dsn,
                                                      file_type = file_type,
                                                      PLANTCENSUS = PLANTCENSUS,
                                                      verbose = verbose)
    # } else if (toupper(source) == "SURVEY123"){
    #   species_inventory <- gather_species_inventory_survey123(
    #     SpeciesRichness_0 = SpeciesRichness_0,
    #     SpecRichDetail_1 = SpecRichDetail_1)

  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # Add source field so that we know where the data came from
  # species_inventory$source <- toupper(source)
  species_inventory$source <- source

  if("sf" %in% class(species_inventory)) {
    species_inventory <- sf::st_drop_geometry(species_inventory)
  }

  if (any(class(species_inventory) %in% c("POSIXct", "POSIXt"))) {
    # change_vars <- names(species_inventory)[do.call(rbind, vapply(species_inventory,
    #                                                               class))[, 1] %in% c("POSIXct", "POSIXt")]
    species_inventory <- dplyr::mutate(.data = species_inventory,
                                       dplyr::across(.cols = tidyselect::where(fn = ~ class(.x) %in% c("POSIXct", "POSIXt")),
                                                     .fns = as.character))
    # species_inventory <- dplyr::mutate_at(species_inventory, dplyr::vars(change_vars),
    #                                       dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  species_inventory <- dplyr::select(.data = species_inventory,
                                     PrimaryKey,
                                     tidyselect::everything())

  # remove duplicates and empty rows
  # if(autoQC){
  #   message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
  #   species_inventory <- species_inventory %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "specinv")
  # }

  return(species_inventory)
}



# PLOT CHARACTERIZATION ###
# #' Convert plot data into a tall, tidy data frame
# #'
# #' @description Given wide format plot data, create a tall format data frame
# #' usable by other terradactyl functions.
# #' @param dsn Character string. The full filepath and filename (including file
# #' extension) of the geodatabase or text file containing the table of interest.
# #' This field is unnecessary if you provide either tblPlots (AIM/DIMA/TerrADat)
# #' or POINT (LMF/NRI).
# #' @param source Character string. The data source format,
# #' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
# #' @param tblPlots Dataframe of the data structure tblPlots from the
# #' DIMA database with the addition of PrimaryKey and DBKey fields. Use when data
# #' source is AIM, DIMA, or TerrADat; alternately provide dsn.
# #' @param POINT Dataframe of the data structure PINTERCEPT from the LMF/NRI
# #' database with the addition of PrimaryKey and DBKey fields. Use when source
# #' is LMF or NRI; alternately provide dsn.
# #' @param POINTCOORDINATES Dataframe of the data structure POINTCOORDINATES from the LMF/NRI
# #' database with the addition of PrimaryKey and DBKey fields. Use when source
# #' is LMF or NRI; alternately provide dsn.
# #' @param GPS Dataframe of the data structure GPS from the LMF/NRI
# #' database with the addition of PrimaryKey and DBKey fields. Use when source
# #' is LMF or NRI; alternately provide dsn.
# #' #' @param file_type Character string that denotes the source file type of the
# #' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
# #' AIM/DIMA/TerrADat, or if POINT, POINTCOORDINATES, and GPS are provided.
# #' @importFrom magrittr %>%
# #' @name gather_plot_characterization
# #' @family <gather>
# #' @return A tall data frame containing plot characterization data
# #' @examples
# #' gather_plot_characterization(dsn = "Path/To/AIM_Geodatabase.gdb",
# #'                              source = "AIM")
# #' gather_plot_characterization(dsn = "Path/To/LMF_Geodatabase.gdb",
# #'                              source = "LMF")
# #'
# #' aim_plots <- read.csv("Path/To/tblPlots.csv")
# #' gather_plot_characterization(source = "AIM",
# #'                              tblPlots = aim_plots)
# #'
# #' lmf_pintercept <- read.csv("Path/To/PINTERCEPT.csv")
# #' lmf_pointcoords <- read.csv("Path/To/POINTCOORDINATES.csv")
# #' lmf_gps <- read.csv("Path/To/GPS.csv")
# #' gather_plot_characterization(source = "LMF",
# #'                              PINTERCEPT = lmf_pintercept,
# #'                              POINTCOORDINATES = lmf_pointcoords,
# #'                              GPS = lmf_gps)
#
# #' #' @export gather_plot_characterization_terradat
# #' #' @rdname gather_plot_characterization
# #' gather_plot_characterization_terradat <- function(dsn = NULL,
# #'                                                   tblPlots = NULL){
# #'   if(!is.null(tblPlots)){
# #'     plot_raw <- tblPlots
# #'   } else if(!is.null(dsn)){
# #'     plot_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblPlots", stringsAsFactors = FALSE, quiet = T))
# #'   } else {
# #'     stop("Supply either tblPlots or the path to a GDB containing that table")
# #'   }
# #'   plot_tall <- plot_raw %>%
# #'     dplyr::select_if(names(.) %in% c(
# #'       'PrimaryKey', 'DBKey', 'ProjectKey',
# #'       'Latitude', 'Longitude',
# #'       'State', 'County',
# #'       'EcolSite', 'ParentMaterial', 'Slope', 'Elevation', 'Aspect', 'ESD_SlopeShape',
# #'       'LandscapeType', 'LandscapeTypeSecondary', #'HillslopeType',
# #'       'ESD_Series',
# #'       # 'Observer', 'Recorder',
# #'       'EstablishDate'
# #'       # 'ESD_Investigators'
# #'     )) %>%
# #'     dplyr::rename(
# #'       Latitude_NAD83 = Latitude,
# #'       Longitude_NAD83 = Longitude,
# #'       SlopeShape = ESD_SlopeShape,
# #'       SoilSeries = ESD_Series,
# #'     ) %>%
# #'     dplyr::mutate(
# #'       SlopeShapeVertical = dplyr::case_when(
# #'         SlopeShape %in% c("CC", "CV", "CL", "concave concave", "concave convex", "concave linear") ~ "concave",
# #'         SlopeShape %in% c("LC", "LV", "LL", "linear concave", "linear convex", "linear linear") ~ "linear",
# #'         SlopeShape %in% c("VC", "VV", "VL", "convex concave", "convex convex", "convex linear") ~ "convex"
# #'       ),
# #'       SlopeShapeHorizontal = dplyr::case_when(
# #'         SlopeShape %in% c("CC", "LC", "VC", "concave concave", "linear concave", "convex concave") ~ "concave",
# #'         SlopeShape %in% c("CL", "LL", "VL", "concave linear", "linear linear", "convex linear") ~ "linear",
# #'         SlopeShape %in% c("CV", "LV", "VV", "concave convex", "linear convex", "convex convex") ~ "convex"
# #'       ),
# #'       Aspect = suppressWarnings(as.numeric(Aspect)),
# #'       Slope = suppressWarnings(as.numeric(Slope)),
# #'       Latitude_NAD83 = suppressWarnings(as.numeric(Latitude_NAD83)),
# #'       Longitude_NAD83 = suppressWarnings(as.numeric(Longitude_NAD83)),
# #'       PrimaryKey = as.character(PrimaryKey),
# #'       MLRA = substr(EcolSite, 2, 5) %>% gsub("NKNO", NA, .)) %>%
# #'     dplyr::select(-SlopeShape)
# #'
# #'   return(plot_tall)
# #' }
# #'
# #' #' LMF plot characterization function
# #' #' @export gather_plot_characterization_lmf
# #' #' @rdname gather_plot_characterization
# #' gather_plot_characterization_lmf <-   function(dsn = NULL,
# #'                                                POINT = NULL,
# #'                                                POINTCOORDINATES = NULL,
# #'                                                GPS = NULL,
# #'                                                ESFSG = NULL,
# #'                                                file_type = NULL
# #' ) {
# #'
# #'   if (!is.null(POINT) & !is.null(POINTCOORDINATES) & !is.null(GPS) & !is.null(ESFSG)){
# #'     point_lmf_raw <- POINT
# #'     coord_lmf_raw <- POINTCOORDINATES
# #'     gps_lmf_raw   <- GPS
# #'     esfsg_lmf_raw <- ESFSG
# #'   } else if(!is.null(dsn)){
# #'     point_lmf_raw <-
# #'       sf::st_read(dsn = dsn, layer = "POINT", stringsAsFactors = FALSE, quiet = T)
# #'
# #'     coord_lmf_raw <-
# #'       sf::st_read(dsn = dsn, layer = "POINTCOORDINATES", stringsAsFactors = FALSE, quiet = T)
# #'
# #'     gps_lmf_raw <-
# #'       sf::st_read(dsn = dsn, layer = "GPS", stringsAsFactors = FALSE, quiet = T)
# #'
# #'     esfsg_lmf_raw <-
# #'       sf::st_read(dsn = dsn, layer = "ESFSG", stringsAsFactors = FALSE, quiet = T)
# #'
# #'
# #'   } else{
# #'     stop("Supply either POINT, POINTCOORDINATES, ESFSG, and GPS, or the path to a GDB containing those tables")
# #'   }
# #'
# #'   # get slope shape from POINT
# #'   point_lmf <- point_lmf_raw %>% dplyr::select(
# #'     DBKey, PrimaryKey,
# #'     SlopeShapeVertical = VERTICAL_SLOPE_SHAPE,
# #'     SlopeShapeHorizontal = HORIZONTAL_SLOPE_SHAPE,
# #'     Slope = SLOPE_PERCENT, Aspect = SLOPE_ASPECT
# #'   ) %>% dplyr::mutate(
# #'     # reclass aspect into degrees
# #'     Aspect = suppressWarnings(as.numeric(dplyr::recode(Aspect,
# #'                                                        "N" = "0",
# #'                                                        "NE" = "45",
# #'                                                        "E" = "90",
# #'                                                        "SE" = "135",
# #'                                                        "S" = "180",
# #'                                                        "SW" = "225",
# #'                                                        "W" = "270",
# #'                                                        "NW" = "315")))
# #'     # get MLRA from ecological site id
# #'   )
# #'
# #'   # get gis data from POINTCOORDINATES
# #'   coord_lmf <- coord_lmf_raw %>% dplyr::select(
# #'     PrimaryKey, DBKey,
# #'     Latitude_NAD83 = REPORT_LATITUDE,
# #'     Longitude_NAD83 = REPORT_LONGITUDE,
# #'   )
# #'
# #'   # get gis from GPS
# #'   gps_lmf <- gps_lmf_raw %>% dplyr::select(
# #'     PrimaryKey, DBKey,
# #'     Elevation = ELEVATION
# #'   )
# #'
# #'   # get ecological site and mlra from ESFSG
# #'   esfsg_lmf <- esfsg_lmf_raw %>% dplyr::mutate(
# #'     EcolSite = paste0(ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE),
# #'     MLRA = ESFSG_MLRA %>% gsub("^$", NA, .)
# #'   ) %>% dplyr::select(
# #'     PrimaryKey, DBKey,
# #'     EcolSite, MLRA
# #'   )
# #'
# #'   # join GIS
# #'   gis_lmf  <- dplyr::full_join(coord_lmf, gps_lmf, by = c("PrimaryKey", "DBKey"))
# #'
# #'   # join gis and plot data
# #'   plot_lmf <- dplyr::left_join(point_lmf, gis_lmf, by = c("PrimaryKey", "DBKey")) %>%
# #'     # and ecolsite data
# #'     dplyr::left_join(esfsg_lmf, by = c("PrimaryKey", "DBKey"))
# #'
# #'
# #'   # last drop
# #'   plot_lmf <- plot_lmf %>% dplyr::select_if(!names(.) %in% c(
# #'     "Shape", "StateNo", "CountyNo")
# #'   )
# #'
# #'   return(plot_lmf)
# #' }
#
# # export gather_plot_characterization_survey123
# # rdname gather_plot_characterization
# # gather_plot_characterization_survey123 <- function(dsn = NULL,
# #                                                    PlotChar_0 = NULL){
# #
# #   if(!is.null(PlotChar_0)){
# #     plot_raw <- PlotChar_0
# #   } else if(!is.null(dsn)){
# #     plot_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblPlots", stringsAsFactors = FALSE, quiet = T))
# #   } else {
# #     stop("Supply either tblPlots or the path to a GDB containing that table")
# #   }
# #
# #   # Rename plotkey to primarykey
# #   plot_raw$PrimaryKey <- plot_raw$PlotKey
# #
# #   # Add null DBKey
# #   plot_raw$DBKey <- NA
# #
# #   # Check for duplicate PrimaryKeys
# #   dupkeys <- plot_raw$PrimaryKey[duplicated(plot_raw$PrimaryKey)]
# #   if(length(dupkeys) > 0){
# #     dupnames <- paste(unique(dupkeys), collapse = ", ")
# #     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
# #   }
# #
# #   plot_tall <- plot_raw %>%
# #     dplyr::select(
# #       PrimaryKey, DBKey, # ProjectKey,
# #       Latitude_NAD83 = y, Longitude_NAD83 = x,
# #       # State, County,
# #       EcolSite = Ecolsite, ParentMaterial, Slope, Elevation, Aspect, #ESD_SlopeShape,
# #       SLopeShapeVertical = vertshape, SlopeShapeHorizontal = horizshape,
# #       LandscapeType, LandscapeTypeSecondary, #HillslopeType,
# #       SoilSeries = ESD_Series,
# #       # Observer, Recorder,
# #       EstablishDate = EstabDate
# #       # ESD_Investigators
# #     ) %>%
# #     dplyr::mutate(
# #       Aspect = suppressWarnings(as.numeric(Aspect)),
# #       Slope = suppressWarnings(as.numeric(Slope)),
# #       Latitude_NAD83 = suppressWarnings(as.numeric(Latitude_NAD83)),
# #       Longitude_NAD83 = suppressWarnings(as.numeric(Longitude_NAD83)),
# #       PrimaryKey = as.character(PrimaryKey),
# #       MLRA = substr(EcolSite, 2, 5) %>% gsub("NKNO", NA, .))
# #
# #   return(plot_tall)
# # }
#
#
#
# #' #' Wrapper function
# #' #' @export gather_plot_characterization
# #' #' @rdname gather_plot_characterization
# #' gather_plot_characterization <- function(dsn = NULL,
# #'                                          source,
# #'                                          tblPlots = NULL,
# #'                                          POINT = NULL,
# #'                                          POINTCOORDINATES = NULL,
# #'                                          GPS = NULL,
# #'                                          ESFSG = NULL,
# #'                                          # PlotChar_0 = NULL,
# #'                                          file_type = "gdb"){
# #'
# #'   if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
# #'     plotchar <- gather_plot_characterization_terradat(dsn = dsn,
# #'                                                       tblPlots = tblPlots)
# #'   } else if(toupper(source) %in% c("LMF", "NRI")){
# #'     plotchar <- gather_plot_characterization_lmf(dsn = dsn,
# #'                                                  file_type = file_type,
# #'                                                  POINT = POINT,
# #'                                                  POINTCOORDINATES = POINTCOORDINATES,
# #'                                                  GPS = GPS,
# #'                                                  ESFSG = ESFSG)
# #'     # } else if(toupper(source) == "SURVEY123"){
# #'     # plotchar <- gather_plot_characterization_survey123(dsn = dsn,
# #'     # PlotChar_0 = PlotChar_0)
# #'   } else {
# #'     stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
# #'   }
# #'
# #'   # plotchar$source <- toupper(source)
# #'   plotchar$source <- source
# #'
# #'   if("sf" %in% class(plotchar)) plotchar <- sf::st_drop_geometry(plotchar)
# #'
# #'   if (any(class(plotchar) %in% c("POSIXct", "POSIXt"))) {
# #'     change_vars <- names(plotchar)[do.call(rbind, vapply(plotchar,
# #'                                                          class))[, 1] %in% c("POSIXct", "POSIXt")]
# #'     plotchar <- dplyr::mutate_at(plotchar, dplyr::vars(change_vars),
# #'                                  dplyr::funs(as.character))
# #'   }
# #'
# #'   # reorder so that primary key is leftmost column
# #'   plotchar <- plotchar %>%
# #'     dplyr::select(PrimaryKey, DBKey, tidyselect::everything())
# #'
# #'   return(plotchar)
# #' }
#
#
# #' #### SOIL HORIZONS #############################################################
# #' #' Convert horizon data into a tall, tidy data frame
# #' #'
# #' #' @description Given wide format soil horizon data, create a tall
# #' #' format data frame usable by other terradactyl functions.
# #' #' @param dsn Character string. The full filepath and filename (including file
# #' #' extension) of the geodatabase or text file containing the table of interest.
# #' #' This field is unnecessary if you provide either tblSoilPitHorizons
# #' #' (AIM/DIMA/TerrADat) or SOILHORIZON (LMF/NRI).
# #' #' @param source Character string. The data source format,
# #' #' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
# #' #' @param tblSoilPitHorizons Dataframe of the data structure tblSoilPitHorizons
# #' #' from the DIMA database with the addition of PrimaryKey and DBKey fields.
# #' #' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
# #' #' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from the
# #' #' LMF/NRI database with the addition of PrimaryKey and DBKey fields;
# #' #' alternately provide dsn.
# #' #' @importFrom magrittr %>%
# #' #' @name gather_soil_horizon
# #' #' @family <gather>
# #' #' @return A tall data frame containing soil horzon data.
# #' #' @examples
# #' #' gather_soil_horizon(dsn = "Path/To/AIM_Geodatabase.gdb",
# #' #'                     source = "AIM")
# #' #' gather_soil_horizon(dsn = "Path/To/LMF_Geodatabase.gdb",
# #' #'                     source = "LMF")
# #' #'
# #' #' aim_horizons <- read.csv("Path/To/tblSoilPitHorizons.csv")
# #' #' gather_soil_horizon(source = "AIM",
# #' #'                     tblSoilPitHorizons = aim_horizons)
# #' #'
# #' #' lmf_horizons <- read.csv("Path/To/SOILHORIZON.csv")
# #' #' gather_soil_horizon(source = "LMF",
# #' #'                     SOILHORIZON = lmf_horizons)
# #'
# #' #' @export gather_soil_horizon_terradat
# #' #' @rdname gather_soil_horizon
# #' gather_soil_horizon_terradat <- function(dsn = NULL,
# #'                                          tblSoilPitHorizons = NULL){
# #'
# #'   # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
# #'   if (!is.null(tblSoilPitHorizons)) {
# #'     hz_aim_raw <- tblSoilPitHorizons
# #'   } else if(!is.null(dsn)){
# #'     if(!file.exists(dsn)){
# #'       stop("dsn must be a valid filepath to a geodatabase containing tblSoilPitHorizons")
# #'     }
# #'
# #'     hz_aim_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
# #'                                                stringsAsFactors = FALSE, quiet = T))
# #'   } else {
# #'     stop("Supply either tblSoilPitHorizons, or the path to a GDB containing those tables")
# #'   }
# #'
# #'   horizons_aim <- hz_aim_raw %>%
# #'     ### select ###
# #'     dplyr::select(
# #'       PrimaryKey, DBKey, HorizonKey, HorizonDepthUpper, HorizonDepthLower,
# #'       DepthUOM = DepthMeasure, HorizonName = ESD_Horizon,
# #'       Texture, TextureModifier = ESD_HorizonModifier,
# #'       pH = ESD_pH, EC = ESD_EC, Effervescence = Effer,
# #'       ClayPct = ESD_PctClay, SandPct = ESD_PctSand,
# #'
# #'       StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure,
# #'       # StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2,
# #'       StructureQuality = ESD_StructQual,
# #'
# #'       # PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic,
# #'       # ClayFilm = ESD_ClayFilm,
# #'       Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color,
# #'       # RootSize = ESD_RootSize, RootQty = ESD_RootQty,
# #'
# #'       Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
# #'       Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
# #'       Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,
# #'
# #'       HorizonNotes = ESD_Notes
# #'
# #'       ### cleaning ###
# #'     ) %>%
# #'     dplyr::mutate_all(
# #'       stringr::str_trim # defensive, early qc seems to catch this well
# #'     ) %>%
# #'     ### recode class data###
# #'     dplyr::mutate(
# #'       StructureGrade = dplyr::recode(StructureGrade,
# #'                                      "0" = "Structureless",
# #'                                      "1" = "Weak",
# #'                                      "2" = "Moderate",
# #'                                      "3" = "Strong"),
# #'       # StructureGrade2 = dplyr::recode(StructureGrade2,
# #'       #                          "0" = "Structureless",
# #'       #                          "1" = "Weak",
# #'       #                          "2" = "Moderate",
# #'       #                          "3" = "Strong"),
# #'       StructureSize = dplyr::recode(StructureSize %>% tolower(),
# #'                                     "vf" = "Very fine",
# #'                                     "vn" = "Very thin",
# #'                                     "f"  = "Fine",
# #'                                     "tn" = "Thin",
# #'                                     "m"  = "Medium",
# #'                                     "co" = "Coarse",
# #'                                     "tk" = "Thick",
# #'                                     "vc" = "Very coarse",
# #'                                     "vk" = "Very thick",
# #'                                     "ec" = "Extremely coarse"),
# #'       # StructureSize2 = dplyr::recode(StructureSize2 %>% tolower(),
# #'       #                         "vf" = "Very fine",
# #'       #                         "vn" = "Very thin",
# #'       #                         "f"  = "Fine",
# #'       #                         "tn" = "Thin",
# #'       #                         "m"  = "Medium",
# #'       #                         "co" = "Coarse",
# #'       #                         "tk" = "Thick",
# #'       #                         "vc" = "Very coarse",
# #'       #                         "vk" = "Very thick",
# #'       #                         "ec" = "Extremely coarse"),
# #'       StructureType = dplyr::recode(StructureType %>% tolower(),
# #'                                     "gr"  = "Granular",
# #'                                     "abk" = "Angular blocky",
# #'                                     "sbk" = "Subangular blocky",
# #'                                     "pl"  = "Platy",
# #'                                     "weg" = "Wedge",
# #'                                     "pr"  = "Prismatic",
# #'                                     "col" = "Columnar",
# #'                                     "sg"  = "Single grain",
# #'                                     "ma"  = "Massive",
# #'                                     "cdy" = "Cloddy",
# #'                                     "other" = "Other"),
# #'       # StructureType2 = dplyr::recode(StructureType2 %>% tolower(),
# #'       #                         "gr"  = "Granular",
# #'       #                         "abk" = "Angular blocky",
# #'       #                         "sbk" = "Subangular blocky",
# #'       #                         "pl"  = "Platy",
# #'       #                         "weg" = "Wedge",
# #'       #                         "pr"  = "Prismatic",
# #'       #                         "col" = "Columnar",
# #'       #                         "sg"  = "Single grain",
# #'       #                         "ma"  = "Massive",
# #'       #                         "cdy" = "Cloddy",
# #'       #                         "other" = "Other"),
# #'     ) %>%
# #'     ### complex mutates that depend on >1 var ###
# #'     dplyr::mutate(
# #'       SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
# #'       FragVolGravel = dplyr::case_when(
# #'         Fragment1Type %in% c("GR", "Gravel", "1") ~ Fragment1VolPct,
# #'         Fragment2Type %in% c("GR", "Gravel", "1") ~ Fragment2VolPct,
# #'         Fragment3Type %in% c("GR", "Gravel", "1") ~ Fragment3VolPct
# #'       ),
# #'       FragVolCobble = dplyr::case_when(
# #'         Fragment1Type %in% c("CB", "Cobble", "2") ~ Fragment1VolPct,
# #'         Fragment2Type %in% c("CB", "Cobble", "2") ~ Fragment2VolPct,
# #'         Fragment3Type %in% c("CB", "Cobble", "2") ~ Fragment3VolPct
# #'       ),
# #'       FragVolStone = dplyr::case_when(
# #'         Fragment1Type %in% c("ST", "Stone", "6") ~ Fragment1VolPct,
# #'         Fragment2Type %in% c("ST", "Stone", "6") ~ Fragment2VolPct,
# #'         Fragment3Type %in% c("ST", "Stone", "6") ~ Fragment3VolPct
# #'       ),
# #'       FragVolNodule = dplyr::case_when(
# #'         Fragment1Type %in% c("8", "Nodule") ~ Fragment1VolPct,
# #'         Fragment2Type %in% c("8", "Nodule") ~ Fragment2VolPct,
# #'         Fragment3Type %in% c("8", "Nodule") ~ Fragment3VolPct
# #'       ),
# #'       FragVolDurinode = dplyr::case_when(
# #'         Fragment1Type %in% c("9", "Durinode") ~ Fragment1VolPct,
# #'         Fragment2Type %in% c("9", "Durinode") ~ Fragment2VolPct,
# #'         Fragment3Type %in% c("9", "Durinode") ~ Fragment3VolPct
# #'       ),
# #'       HorizonDepthLower = dplyr::case_when(
# #'         DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
# #'         DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
# #'       HorizonDepthUpper = dplyr::case_when(
# #'         DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54,
# #'         DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
# #'       DepthUOM = "cm"
# #'     ) %>%
# #'     ### drop vars that are no longer relevant ###
# #'     dplyr::select(
# #'       -Fragment1Type,
# #'       -Fragment2Type,
# #'       -Fragment3Type,
# #'       -Fragment1VolPct,
# #'       -Fragment2VolPct,
# #'       -Fragment3VolPct,
# #'     )   %>%
# #'     dplyr::arrange(PrimaryKey, HorizonDepthUpper) %>%
# #'     dplyr::group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
# #'       PrimaryKey
# #'     ) %>%
# #'
# #'     dplyr::mutate(HorizonNumber = as.character(dplyr::row_number()),
# #'                   # across(c(RockFragments), ~ suppressWarnings(as.integer(.x))),
# #'                   across(c(pH,
# #'                            EC, ClayPct, SandPct, SiltPct, # poreqty,
# #'                            FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule,
# #'                            FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
# #'                   ), ~ suppressWarnings(as.double(.x))),
# #'                   # across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
# #'     )
# #'   horizons_aim <- as.data.frame(horizons_aim)
# #'
# #'   return(horizons_aim)
# #' }
# #'
# #' #' @export gather_soil_horizon_lmf
# #' #' @rdname gather_soil_horizon
# #' gather_soil_horizon_lmf <- function(dsn = NULL,
# #'                                     SOILHORIZON = NULL){
# #'   # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
# #'   if (!is.null(SOILHORIZON)){
# #'     hz_lmf_raw <- SOILHORIZON
# #'   } else if(!is.null(dsn)){
# #'     if(!file.exists(dsn)){
# #'       stop("dsn must be a valid filepath to a geodatabase containing SOILHORIZON")
# #'     }
# #'     hz_lmf_raw <- sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T)
# #'   } else {
# #'     stop("Supply either SOILHORIZON or the path to a GDB containing that table")
# #'   }
# #'
# #'   horizons_lmf <- hz_lmf_raw %>%
# #'     dplyr::select(
# #'       PrimaryKey, DBKey, HorizonNumber = SEQNUM,
# #'       HorizonDepthLower = DEPTH, Effervescence = EFFERVESCENCE_CLASS,
# #'       Texture = HORIZON_TEXTURE, TextureModifier = TEXTURE_MODIFIER,
# #'       HorizonNotes = UNUSUAL_FEATURES
# #'     )
# #'
# #'   horizons_lmf <- horizons_lmf %>% # have to have already created horizons_lmf before the HorizonDepthUpper parsing below, as it refers to the df by name
# #'     dplyr::mutate(
# #'       DepthUOM = "cm",
# #'       HorizonNumber = as.character(HorizonNumber),
# #'       HorizonDepthUpper = sapply(unique(PrimaryKey), function(x) {
# #'         lower <- horizons_lmf$HorizonDepthLower[horizons_lmf$PrimaryKey == x]
# #'         upper <- c(0, lower[1:length(lower) - 1])
# #'         return(upper)}
# #'       ) %>% unlist(),
# #'       ### ARE THEY ALWAYS INCHES? No measure type recorded, though they may use decifeet sometimes
# #'       HorizonDepthLower = suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
# #'       HorizonDepthUpper = suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54
# #'     )
# #'
# #'   return(horizons_lmf)
# #' }
# #' #' export gather_soil_horizon_survey123
# #' #' rdname gather_soil_horizon
# #' # gather_soil_horizon_survey123 <- function(dsn = NULL,
# #' #                                           PlotChar_0 = NULL,
# #' #                                           SoilPitHorizons_1 = NULL){
# #' #
# #' #   # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
# #' #   if (!is.null(SoilPitHorizons_1) & !is.null(PlotChar_0)) {
# #' #     hz_raw <- SoilPitHorizons_1
# #' #     plotchar_raw <- PlotChar_0
# #' #   } else if(!is.null(dsn)){
# #' #     if(!file.exists(dsn)){
# #' #       stop("dsn must be a valid filepath to a geodatabase containing tblSoilPitHorizons")
# #' #     }
# #' #
# #' #     hz_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
# #' #                                                stringsAsFactors = FALSE, quiet = T))
# #' #   } else {
# #' #     stop("Supply either tblSoilPitHorizons, or the path to a GDB containing those tables")
# #' #   }
# #' #
# #' #   # Survey123 data uses PlotKey instead of PrimaryKey
# #' #   hz_raw <- dplyr::left_join(hz_raw, plotchar_raw %>% dplyr::select(PrimaryKey = PlotKey, GlobalID), by = c("ParentGlobalID" = "GlobalID"))
# #' #
# #' #   # Check for duplicate PrimaryKeys
# #' #   dupkeys <- hz_raw$PrimaryKey[duplicated(hz_raw$PrimaryKey)]
# #' #   if(length(dupkeys) > 0){
# #' #     dupnames <- paste(unique(dupkeys), collapse = ", ")
# #' #     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
# #' #   }
# #' #
# #' #   horizons <- hz_raw %>%
# #' #     ### select ###
# #' #     dplyr::select(
# #' #       PrimaryKey, DBKey, HorizonKey, HorizonDepthUpper, HorizonDepthLower,
# #' #       DepthUOM = DepthMeasure, HorizonName = ESD_Horizon,
# #' #       Texture, TextureModifier = ESD_HorizonModifier,
# #' #       pH = ESD_pH, EC = ESD_EC, Effervescence = Effer,
# #' #       ClayPct = ESD_PctClay, SandPct = ESD_PctSand,
# #' #
# #' #       StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure,
# #' #       # StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2,
# #' #       StructureQuality = ESD_StructQual,
# #' #
# #' #       # PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic,
# #' #       # ClayFilm = ESD_ClayFilm,
# #' #       Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color,
# #' #       # RootSize = ESD_RootSize, RootQty = ESD_RootQty,
# #' #
# #' #       Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
# #' #       Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
# #' #       Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,
# #' #
# #' #       HorizonNotes = ESD_Notes
# #' #
# #' #       ### cleaning ###
# #' #     ) %>%
# #' #     dplyr::mutate_all(
# #' #       stringr::str_trim # defensive, early qc seems to catch this well
# #' #     ) %>%
# #' #     ### recode class data###
# #' #     dplyr::mutate(
# #' #       StructureGrade = dplyr::recode(StructureGrade,
# #' #                                      "0" = "Structureless",
# #' #                                      "1" = "Weak",
# #' #                                      "2" = "Moderate",
# #' #                                      "3" = "Strong"),
# #' #       # StructureGrade2 = dplyr::recode(StructureGrade2,
# #' #       #                          "0" = "Structureless",
# #' #       #                          "1" = "Weak",
# #' #       #                          "2" = "Moderate",
# #' #       #                          "3" = "Strong"),
# #' #       StructureSize = dplyr::recode(StructureSize %>% tolower(),
# #' #                                     "vf" = "Very fine",
# #' #                                     "vn" = "Very thin",
# #' #                                     "f"  = "Fine",
# #' #                                     "tn" = "Thin",
# #' #                                     "m"  = "Medium",
# #' #                                     "co" = "Coarse",
# #' #                                     "tk" = "Thick",
# #' #                                     "vc" = "Very coarse",
# #' #                                     "vk" = "Very thick",
# #' #                                     "ec" = "Extremely coarse"),
# #' #       # StructureSize2 = dplyr::recode(StructureSize2 %>% tolower(),
# #' #       #                         "vf" = "Very fine",
# #' #       #                         "vn" = "Very thin",
# #' #       #                         "f"  = "Fine",
# #' #       #                         "tn" = "Thin",
# #' #       #                         "m"  = "Medium",
# #' #       #                         "co" = "Coarse",
# #' #       #                         "tk" = "Thick",
# #' #       #                         "vc" = "Very coarse",
# #' #       #                         "vk" = "Very thick",
# #' #       #                         "ec" = "Extremely coarse"),
# #' #       StructureType = dplyr::recode(StructureType %>% tolower(),
# #' #                                     "gr"  = "Granular",
# #' #                                     "abk" = "Angular blocky",
# #' #                                     "sbk" = "Subangular blocky",
# #' #                                     "pl"  = "Platy",
# #' #                                     "weg" = "Wedge",
# #' #                                     "pr"  = "Prismatic",
# #' #                                     "col" = "Columnar",
# #' #                                     "sg"  = "Single grain",
# #' #                                     "ma"  = "Massive",
# #' #                                     "cdy" = "Cloddy",
# #' #                                     "other" = "Other"),
# #' #       # StructureType2 = dplyr::recode(StructureType2 %>% tolower(),
# #' #       #                         "gr"  = "Granular",
# #' #       #                         "abk" = "Angular blocky",
# #' #       #                         "sbk" = "Subangular blocky",
# #' #       #                         "pl"  = "Platy",
# #' #       #                         "weg" = "Wedge",
# #' #       #                         "pr"  = "Prismatic",
# #' #       #                         "col" = "Columnar",
# #' #       #                         "sg"  = "Single grain",
# #' #       #                         "ma"  = "Massive",
# #' #       #                         "cdy" = "Cloddy",
# #' #       #                         "other" = "Other"),
# #' #     ) %>%
# #' #     ### complex mutates that depend on >1 var ###
# #' #     dplyr::mutate(
# #' #       SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
# #' #       FragVolGravel = dplyr::case_when(
# #' #         Fragment1Type %in% c("GR", "Gravel", "1") ~ Fragment1VolPct,
# #' #         Fragment2Type %in% c("GR", "Gravel", "1") ~ Fragment2VolPct,
# #' #         Fragment3Type %in% c("GR", "Gravel", "1") ~ Fragment3VolPct
# #' #       ),
# #' #       FragVolCobble = dplyr::case_when(
# #' #         Fragment1Type %in% c("CB", "Cobble", "2") ~ Fragment1VolPct,
# #' #         Fragment2Type %in% c("CB", "Cobble", "2") ~ Fragment2VolPct,
# #' #         Fragment3Type %in% c("CB", "Cobble", "2") ~ Fragment3VolPct
# #' #       ),
# #' #       FragVolStone = dplyr::case_when(
# #' #         Fragment1Type %in% c("ST", "Stone", "6") ~ Fragment1VolPct,
# #' #         Fragment2Type %in% c("ST", "Stone", "6") ~ Fragment2VolPct,
# #' #         Fragment3Type %in% c("ST", "Stone", "6") ~ Fragment3VolPct
# #' #       ),
# #' #       FragVolNodule = dplyr::case_when(
# #' #         Fragment1Type %in% c("8", "Nodule") ~ Fragment1VolPct,
# #' #         Fragment2Type %in% c("8", "Nodule") ~ Fragment2VolPct,
# #' #         Fragment3Type %in% c("8", "Nodule") ~ Fragment3VolPct
# #' #       ),
# #' #       FragVolDurinode = dplyr::case_when(
# #' #         Fragment1Type %in% c("9", "Durinode") ~ Fragment1VolPct,
# #' #         Fragment2Type %in% c("9", "Durinode") ~ Fragment2VolPct,
# #' #         Fragment3Type %in% c("9", "Durinode") ~ Fragment3VolPct
# #' #       ),
# #' #       HorizonDepthLower = dplyr::case_when(
# #' #         DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
# #' #         DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
# #' #       HorizonDepthUpper = dplyr::case_when(
# #' #         DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54,
# #' #         DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
# #' #       DepthUOM = "cm"
# #' #     ) %>%
# #' #     ### drop vars that are no longer relevant ###
# #' #     dplyr::select(
# #' #       -Fragment1Type,
# #' #       -Fragment2Type,
# #' #       -Fragment3Type,
# #' #       -Fragment1VolPct,
# #' #       -Fragment2VolPct,
# #' #       -Fragment3VolPct,
# #' #     )   %>%
# #' #     dplyr::arrange(PrimaryKey, HorizonDepthUpper) %>%
# #' #     dplyr::group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
# #' #       PrimaryKey
# #' #     ) %>%
# #' #
# #' #     dplyr::mutate(HorizonNumber = as.character(dplyr::row_number()),
# #' #                   # across(c(RockFragments), ~ suppressWarnings(as.integer(.x))),
# #' #                   across(c(pH,
# #' #                            EC, ClayPct, SandPct, SiltPct, # poreqty,
# #' #                            FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule,
# #' #                            FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
# #' #                   ), ~ suppressWarnings(as.double(.x))),
# #' #                   # across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
# #' #     )
# #' #   horizons <- as.data.frame(horizons)
# #' #
# #' #   return(horizons)
# #' # }
# #'
# #'
# #' #' @export gather_soil_horizon
# #' #' @rdname gather_soil_horizon
# #' gather_soil_horizon <- function(dsn = NULL,
# #'                                 source,
# #'                                 SOILHORIZON = NULL,
# #'                                 tblSoilPitHorizons = NULL,
# #'                                 autoQC = TRUE) {
# #'
# #'   if(toupper(source) %in% c("AIM", "TERRADAT")) {
# #'     soil <- gather_soil_horizon_terradat(dsn = dsn, tblSoilPitHorizons = tblSoilPitHorizons)
# #'   } else if(toupper(source) %in% c("LMF", "NRI")){
# #'     soil <- gather_soil_horizon_lmf(dsn = dsn, SOILHORIZON = SOILHORIZON)
# #'   } else {
# #'     stop("source must be AIM, TerraDat, DIMA, LMF, or NRI (all case independent)")
# #'   }
# #'
# #'   soil$source <- source
# #'
# #'   if("sf" %in% class(soil)) soil <- sf::st_drop_geometry(soil)
# #'
# #'   if (any(class(soil) %in% c("POSIXct", "POSIXt"))) {
# #'     change_vars <- names(soil)[do.call(rbind, vapply(soil,
# #'                                                      class))[, 1] %in% c("POSIXct", "POSIXt")]
# #'     soil <- dplyr::mutate_at(soil, dplyr::vars(change_vars),
# #'                              dplyr::funs(as.character))
# #'   }
# #'
# #'   # change from tibble to data frame
# #'   soil <- as.data.frame(soil) %>%
# #'
# #'     # reorder so that primary key is leftmost column
# #'     dplyr::select(PrimaryKey, DBKey, tidyselect::everything())
# #'
# #'   # remove duplicates and empty rows
# #'   if(autoQC){
# #'     message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
# #'     soil <- soil %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "soilhz")
# #'   }
# #'
# #'   return(soil)
# #' }
# #'


