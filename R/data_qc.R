#' Identify records without matches in a second data frame
#' @description Given two data frames and a set of variables which should join them, find the records in one or both data frames which have no corresponding records in the other.
#' @param x Data frame. The data frame which will always be checked for records which do not correspond to any records in \code{y}. Must contain the variables specified in \code{joining_variables}.
#' @param y Data frame. The data frame against which \code{x} will be checked. Must contain the variables specified in \code{joining_variables}. If \socde{symmetric} is \code{TRUE} then this will also be checked against \code{x}.
#' @param joining_variables Character vector or list of character vectors. The variables to use for the join \code{x} between \code{y}. If the variable names are the same in both \code{x} and \code{y}, then a single character vector containing the names of the variables will work. Otherwise, it should be formatted as a list of two character vectors, one for the variable names in \code{x} and one for \code{y}. In the case that you are providing a list, make sure that the variables containing the same values are in the same order between the two vectors. Naming the vectors in the list is optional but recommended: if the names of the vectors in the list are not \code{"x"} and \code{"y"}, then the function will make a best guess, defaulting to assuming that the first vector is for \code{x} and the second for \code{y}.
#' @param symmetric Logical. If \code{TRUE} then orphaned records will be found for both \code{x} and \code{y}. If \code{FALSE} then orphaned records will only be found for \code{x}. Defaults to \code{TRUE}.
#' @returns If \code{symmetric} is \code{TRUE}, then a named list of two data frames containing all orphaned records from \code{x} and \code{y}. If \code{symmetric} is \code{FALSE} then a single data frame of all orphaned records from \code{x}. If no orphaned records were found, the returned data frame(s) will contain no values.
#' @examples
#' # detail_data <- data.frame(detail_key = rep(x = c(1, 2), times = 3),
#' #                           detail_value = rep(x = c("a", "b", "c"), times = 2))
#' # header_data <- data.frame(header_key = rep(x = c(1, 2, 3), times = 2),
#' #                           header_value = rep(x = c("x", "y", "z"), times = 2))
#' # check_orphaned_records(x = detail_data,
#' #                        y = header_data,
#' #                        joining_variables = list(x = "detail_key",
#' #                                                 y = "header_key"))
#' @export
check_orphaned_records <- function(x,
                                   y,
                                   joining_variables,
                                   symmetric = TRUE){

  if (!"data.frame" %in% class(x)) {
    stop("x must be a data frame.")
  }
  if ("sf" %in% class(x)) {
    x <- sf::st_drop_geometry(x = x)
  }

  if (!"data.frame" %in% class(y)) {
    stop("y must be a data frame.")
  }
  if ("sf" %in% class(y)) {
    y <- sf::st_drop_geometry(x = y)
  }

  if (class(joining_variables) != "list") {
    if (class(joining_variables) == "character") {
      joining_variables <- list(x = joining_variables,
                                y = joining_variables)
    } else {
      stop("joining_variables must either be a character vector or a list of character vectors.")
    }
  } else {
    if (length(joining_variables) == 1) {
      joining_variables <- list(x = joining_variables[[1]],
                                y = joining_variables[[1]])
    } else if (length(joining_variables) != 2) {
      stop("When joining_variables is a list, it must contain either 1 character vector if the variable names are identical between x and y or 2 character vectors if they are not.")
    } else {
      # Changing the names to "x" and "y" if they aren't already
      missing_expected_list_names <- setdiff(x = c("x", "y"),
                                             y = names(joining_variables))
      unexpected_list_names <- setdiff(x = names(joining_variables),
                                       y = c("x", "y"))
      if (length(unexpected_list_names) > 0) {
        names(joining_variables)[names(joining_variables) %in% unexpected_list_names] <- missing_expected_list_names
      }
    }

    x_joining_var_classes <- sapply(X = joining_variables[["x"]],
                                    current_data = x,
                                    FUN = function(X, current_data){
                                      if (!(X %in% names (current_data))) {
                                        stop(paste("The variable", X,
                                                   "does not appear in x."))
                                      }
                                      class(current_data[[X]])
                                    })
    y_joining_var_classes <- sapply(X = joining_variables[["y"]],
                                    current_data = y,
                                    FUN = function(X, current_data){
                                      if (!(X %in% names (current_data))) {
                                        stop(paste("The variable", X,
                                                   "does not appear in y."))
                                      }
                                      class(current_data[[X]])
                                    })

    if (length(x_joining_var_classes) != length(y_joining_var_classes)) {
      stop("The number of joining variables for x and y must be identical.")
    }
    if (!identical(unname(x_joining_var_classes), unname(y_joining_var_classes))) {
      stop("The classes of the joining variables for x and y must be identical. The check for this is and the eventual join depend the order that the variables appear in joining_variables, so please make sure those match.")
    }
  }

  # Strip the input data to just the unique combinations of values in the
  # joining variables and a new variable indicating that there were records for
  # those combinations.
  x_minimal <- dplyr::select(.data = x,
                             tidyselect::all_of(joining_variables[["x"]])) |>
    dplyr::distinct(.data = _) |>
    dplyr::mutate(.data = _,
                  x_present = TRUE)

  y_minimal <- dplyr::select(.data = y,
                             tidyselect::all_of(joining_variables[["y"]])) |>
    dplyr::distinct(.data = _) |>
    dplyr::mutate(.data = _,
                  y_present = TRUE)

  # Join the two minimal data frames so we can get all the unique combinations
  # of values in the joining variables represented between the two and have
  # the two variables indicating which of the source data frames they occur in.

  # We need to make a vector that dplyr understands for the join.
  joining_vector <- setNames(object = joining_variables[["y"]],
                             nm = joining_variables[["x"]])
  check_lookup <- dplyr::full_join(x = x_minimal,
                                   y = y_minimal,
                                   by = joining_vector) |>
    dplyr::mutate(.data = _,
                  dplyr::across(.cols = c("x_present",
                                          "y_present"),
                                .fns = ~ tidyr::replace_na(data = .x,
                                                           replace = FALSE)))

  # Get just the records in x and y which have combinations of values in the
  # joining variables not represented in the other data frame.
  # Everything after the join is just making sure that the outputs maintain the
  # variable order of the inputs.
  x_orphaned <- dplyr::left_join(x = dplyr::filter(.data = check_lookup,
                                                   !y_present),
                                 y = x,
                                 by = joining_variables[["x"]]) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(names(x)))

  output <- x_orphaned

  if (symmetric) {
    y_orphaned <- dplyr::left_join(x = dplyr::filter(.data = check_lookup,
                                                     !x_present),
                                   y = y,
                                   # Note that when the full_join() above happens
                                   # it keeps the names of the variables from x
                                   # so we'll be using the joining variable
                                   # vector here.
                                   by = joining_vector) |>
      # And then we need to rename those joining variables so they match the
      # names they had in the original y input.
      dplyr::rename(.data = _,
                    tidyselect::all_of(setNames(object = names(joining_vector),
                                                nm = unname(joining_vector)))) |>
      dplyr::select(.data = _,
                    tidyselect::all_of(names(y)))

    output <- list(x = x_orphaned,
                   y = y_orphaned)
  }

  output
}


#' Identify non-unique records in a data frame.
#' @description Given a data frame and a set of variables, return the records from the data frame which do not have a unique combination of values in those variables.
#' This is intended to be used to find duplicated or impossible records.
#' @param data Data frame. The data which will be checked for non-unique records.
#' @param uid_variables Character vector. One or more variable names corresponding to variables in the data frame. This must include only and all of the variables which when taken together should have only only one record for each combination of values. In a wide data set you would likely only have one variable but in a tall data set you would need to specify multiple. For example, in the case of tblGapDetail, this should be \code{c("PrimaryKey", "RecKey", "RecType", "Gap", "GapStart", "GapEnd")}.
#' @returns The original input \code{data} restricted to records which were non-unique with the added variable "id_nonunique_group" indicating which records are duplicates of each other.
#' @examples
#' # example_data <- data.frame(key = rep(x = c(1, 2), times = 3),
#' #                            value1 = rep(x = c("terrestrial", "aquatic"), times = 3),
#' #                            value2 = 1:6)
#' # check_uniqueness(data = example_data,
#' #                  uid_variables = c("key",
#' #                                    "value1"))
#'
#' @export
check_uniqueness <- function(data,
                             uid_variables){
  if (!"data.frame" %in% class(data)) {
    stop("data must be a data frame")
  }

  if (class(uid_variables) != "character") {
    stop("uid_variables must be a single character string or a vector of character strings.")
  }

  if (!all(uid_variables %in% names(data))) {
    stop(paste0("The following variables did not occur in data: ",
                paste(uid_variables[!(uid_variables %in% names(data))],
                      collapse = ", ")))
  }

  nonunique_summary <- dplyr::summarize(.data = data,
                                        .by = tidyselect::all_of(uid_variables),
                                        record_count = dplyr::n()) |>
    dplyr::filter(.data = _,
                  record_count > 1) |>
    dplyr::mutate(.data = _,
                  id_nonunique_group = dplyr::row_number())

  output <- dplyr::left_join(x = dplyr::select(.data = nonunique_summary,
                                               -record_count),
                             y = data,
                             by = uid_variables,
                             relationship = "one-to-many") |>
    dplyr::select(.data = _,
                  id_nonunique_group,
                  tidyselect::all_of(names(data)))

  output
}

#' Check for uniqueness and orphaned records in raw AIM data.
#' @description For a given geodatabase containing AIM (or AIM-compatible) data, check the request data types for uniqueness and orphaned records. This is a wrapper for check_uniqueness() and check_orphaned_records() which you can use if you have nonstandard data.
#' @param dsn Character string. The filepath to the geodatabase containing the data to test. The data must be stored in the standard format, including table names, e.g., tblGapDetail.
#' @param data_types Character vector. The data type or types to test. Valid strings are \code{"lpi"}, \code{"gap"}, \code{"soilstability"}, and \code{"heights"}. Defaults to \code{c("lpi", "gap", "soilstability", "heights")}.
#' @param output_path Optional character string. The filepath to write all nonunique and orphaned records to. There will be one "nonuniques" CSV for each table where nonunique records were found and one "orphaned" CSV for each table where orphaned records were found. Filenames are formed from the name of the source table and the type of records, e.g. "tblGapHeader_nonuniques.CSV", "tblLLPIDetail_orphaned.CSV". No data will be written to CSV if this is \code{NULL}. Defaults to \code{NULL}.
#' @param make_directory Logical. If \code{TRUE} then the directory \code{output_path} will be created if it does not already exist. Defaults to \code{FALSE}.
#' @returns A named list of lists of data frame. The "nonuniques" list will contain named data frames with all records which were not unique. The "orphaned" list will contain named data frames with all records which didn't correspond to any records in their companion table. Data frames for tables which held only unique records or had no orphaned records will contain no values.
#' @export
check_terradat_data <- function(dsn,
                                data_types = c("lpi", "gap", "soilstability", "heights"),
                                output_path = NULL,
                                make_directory = FALSE,
                                verbose = FALSE){
  valid_data_types <- c("lpi" = "LPI",
                        "gap" = "Gap",
                        "soilstability" = "SoilStab",
                        "heights" = "LPI")

  # Doing standard santiziation stuff.
  if (!is.null(output_path)) {
    if (length(output_path) > 1 | class(output_path) != "character") {
      stop("output_path must be a character string pointing to a filepath to save the results of the checks to.")
    }
    if (!dir.exists(output_path)) {
      if (make_directory) {
        if (verbose) {
          message("The output directory didn't exist, but will be created to save the output tables.")
        }
        dir.create(output_path)
      } else {
        stop(paste("The requested output directory doesn't exist. If the output_path value is correct, either create the directory yourself or change make_directory to TRUE."))
      }
    }
  }


  if (!all(data_types %in% names(valid_data_types))) {
    stop("Valid values for data_type are: 'lpi', 'gap', 'soilstability', and 'heights'.")
  }

  # In case they stuck a "/" at the end of the filepath (which RStudio will do
  # for you if you're using TAB to autocomplete a filepath as you type).
  dsn <- stringr::str_remove(string = dsn,
                             pattern = "(/)+$")

  filename <- basename(dsn)

  if (!stringr::str_detect(string = filename,
                           pattern = "\\.(GDB|gdb)$")) {
    stop("dsn must point to a geodatabase and therefore must end in the file extension GDB.")
  }

  dsn_check <- file.exists(dsn)

  if (!dsn_check) {
    stop("Unable to find a geodatabase at the path provided.")
  }

  # Make sure that we know which tables in the geodatabase wer're looking for.
  required_layers <- lapply(X = valid_data_types[data_types],
                            FUN = function(X){
                              paste0("tbl",
                                     X,
                                     c("Detail",
                                       "Header"))
                            })

  available_layers <- sf::st_layers(dsn = dsn)

  missing_layers <- setdiff(x = unique(unlist(required_layers)),
                            y = available_layers$name)

  if (length(missing_layers) > 0) {
    stop(paste("The geodatabase does not contain the following required layer(s):",
               paste(missing_layers,
                     collapse = ", ")))
  }

  # Read in only the relevant data and only once (in case some ask for the
  # same table as others).
  data_list <- lapply(X = setNames(object = unique(unlist(required_layers)),
                                   nm = unique(unlist(required_layers))),
                      dsn = dsn,
                      verbose = verbose,
                      FUN = function(X, dsn, verbose){
                        if (verbose) {
                          message(paste("Reading in",
                                        X))
                        }
                        sf::st_read(dsn = dsn,
                                    layer = X) |>
                          # So it doesn't complain about a lack of geometry
                          suppressWarnings()
                      })

  # These are for running through check_uniqueness()
  uid_variables_list <- list(tblLPIHeader = c("PrimaryKey",
                                              "RecKey"),
                             tblLPIDetail = c("PrimaryKey",
                                              "RecKey",
                                              "PointNbr"),
                             tblGapHeader = c("PrimaryKey",
                                              "RecKey"),
                             tblGapDetail = c("PrimaryKey",
                                              "RecKey",
                                              "RecType",
                                              "GapStart",
                                              "GapEnd",
                                              "Gap"),
                             tblSoilStabHeader = c("PrimaryKey",
                                                   "RecKey"),
                             tblSoilStabDetail = c("PrimaryKey",
                                                   "RecKey"))

  # These are for running through check_orphaned_records()
  joining_variables_list <- list(tblLPIHeader = c("PrimaryKey",
                                                  "RecKey"),
                                 tblLPIDetail = c("PrimaryKey",
                                                  "RecKey"),
                                 tblGapHeader = c("PrimaryKey",
                                                  "RecKey"),
                                 tblGapDetail = c("PrimaryKey",
                                                  "RecKey"),
                                 tblSoilStabHeader = c("PrimaryKey",
                                                       "RecKey"),
                                 tblSoilStabDetail = c("PrimaryKey",
                                                       "RecKey"))

  # For each of the layers we're working with, find the nonunique records.
  nonuniques_list <- lapply(X = required_layers[!duplicated(required_layers)],
                            data_list = data_list,
                            uid_variables_list = uid_variables_list,
                            verbose = verbose,
                            FUN = function(X, data_list, uid_variables_list, verbose){
                              lapply(X = setNames(object = X,
                                                  nm = X),
                                     data_list = data_list,
                                     uid_variables_list,
                                     verbose = verbose,
                                     FUN = function(X, data_list, uid_variables_list, verbose){
                                       if (verbose) {
                                         message(paste("Checking for nonunique records in",
                                                       X))
                                       }
                                       check_uniqueness(data = data_list[[X]],
                                                        uid_variables = uid_variables_list[[X]])
                                     })

                            })

  # For each of the layers we're working with, find the orphaned records.
  orphaned_list <- lapply(X = required_layers[!duplicated(required_layers)],
                          data_list = data_list,
                          joining_variables_list = joining_variables_list,
                          verbose = verbose,
                          FUN = function(X, data_list, joining_variables_list, verbose){
                            if (verbose) {
                              message(paste("Comparing",
                                            paste(X,
                                                  collapse = " and "),
                                            "to find orphaned records."))
                            }
                            output <- check_orphaned_records(x = data_list[[X[1]]],
                                                             y = data_list[[X[2]]],
                                                             joining_variables = joining_variables_list[c(X[1], X[2])],
                                                             symmetric = TRUE)
                            names(output) <- X
                            output
                          })

  # Compile and return all those results.
  # This looks a bit complicated, but it's unnesting the two lists of lists and
  # making sure that the data frame names correspond to the source tables'.
  output <- list(nonunique_records = unlist(setNames(object = nonuniques_list,
                                                     nm = NULL),
                                            recursive = FALSE),
                 orphaned_records = unlist(setNames(object = orphaned_list,
                                                    nm = NULL),
                                           recursive = FALSE))

  if (!is.null(output_path)) {
    for (current_table in names(output$nonunique_records)) {
      if (nrow(output$nonunique_records[[current_table]]) > 0) {
        if (verbose) {
          message(paste("Writing nonunique records from", current_table, "to CSV."))
        }
        write.csv(x = output$nonunique_records[[current_table]],
                  file = paste0(output_path, "/",
                                current_table, "_nonuniques.csv"),
                  row.names = FALSE)
      } else {
        if (verbose) {
          message(paste("No nonunique records found in", current_table, "so no records will be written to CSV."))
        }
      }
    }
    for (current_table in names(output$orphaned_records)) {
      if (nrow(output$orphaned_records[[current_table]]) > 0) {
        if (verbose) {
          message(paste("Writing orphaned records from", current_table, "to CSV."))
        }
        write.csv(x = output$orphaned_records[[current_table]],
                  file = paste0(output_path, "/",
                                current_table, "_orphaned.csv"),
                  row.names = FALSE)
      } else {
        if (verbose) {
          message(paste("No orphaned values found in", current_table, "so no records will be written to CSV."))
        }
      }
    }
  }

  output
}

# This one's a wrapper for check_uniqueness() and check_orphaned_records() so
# we can quickly and easily generate warnings.
# This will only fire off a warning if there's anything to report.
#' @export
auto_qc_warning <- function(header_data,
                            detail_data,
                            uid_variables,
                            joining_variables) {
    warning_strings <- c(base_warning = "The following data issues will almost certainly produce erroneous or unexpected data in the function output.")
    header_nonuniques <- check_uniqueness(data = header_data,
                                          uid_variables = uid_variables[["header"]])
    if (nrow(header_nonuniques) > 0) {
      warning_strings["header_nonuniques"] <- paste("There are", length(unique(header_nonuniques$id_nonunique_group)),
                                                    "instances of duplicated header records.")
    }
    detail_nonuniques <- check_uniqueness(data = detail_data,
                                          uid_variables = uid_variables[["detail"]])
    if (nrow(detail_nonuniques) > 0) {
      warning_strings["detail_nonuniques"] <- paste("There are", length(unique(detail_nonuniques$id_nonunique_group)),
                                                    "instances of duplicated detail records.")
    }
    orphaned_records_list <- check_orphaned_records(x = detail_data,
                                                    y = header_data,
                                                    joining_variables = joining_variables,
                                                    symmetric = TRUE)
    if (nrow(orphaned_records_list[["y"]]) > 0) {
      warning_strings["header_orphaned"] <- paste("There are", nrow(orphaned_records_list[["y"]]),
                                                  "header records which do not correspond to any detail records.")
    }
    if (nrow(orphaned_records_list[["x"]]) > 0) {
      warning_strings["detail_orphaned"] <- paste("There are", nrow(orphaned_records_list[["x"]]),
                                                  "detail records with no corresponding header records.")
    }

    if (length(warning_strings) > 1) {
      warning(paste(paste(warning_strings,
                          collapse = " "),
                    "Strongly consider cleaning your data (the functions check_uniqueness() and check_orphaned_records() can help) and rerunning this function."))
    }
}
