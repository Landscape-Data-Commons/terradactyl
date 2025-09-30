gather_header_terradat <- function(source = NULL,
                                   dsn = NULL,
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

  # Ordering them like this is effectively setting priority. The first non-NULL
  # will be used as the input source, so hopefully that's source.
  # This lets us keep the old arguments for backwards compatibility, including
  # if the arguments were unnamed in the old code.
  input <- select_source(possible_inputs = list("source" = source,
                                                "tblPlots" = tblPlots,
                                                "dsn" = dsn),
                         valid_input_classes = c("character",
                                                 "data.frame"),
                         valid_file_extensions = c("gdb",
                                                   "csv",
                                                   "rdata"))

  # And read!
  if (length(input) < 1) {
    stop("An input must be provided as the source argument: a character string specifying the filepath to a geodatabase containing a tblPlots table, a data frame containing the data from tblPlots, or the filepath to an RData file containing the data frame. Alternatively, use the legacy arguments dsn or tblPlots.")
  } else {
    # if (verbose) {
    #   message(paste0("Attempting to read from the ", names(input), " argument."))
    # }
    header <- read_whatever(input = input,
                            # This argument only ends up mattering if
                            layer = "tblPlots$",
                            regex = TRUE,
                            verbose = verbose) |>
      sf::st_drop_geometry(x = _)
  }

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
  # them from the GDB pointed to by the input.
  # This needs to look at date_tables to see if it's a list of data frames, a
  # filepath to a geodatabase, or a vector of table names to try to read from
  # the path that was provided as source or dsn if possible

  if (is.data.frame(date_tables)) {
    date_tables <- list(date_tables)
  } else if (is.list(date_tables)) {
    if (all(sapply(X = date_tables, FUN = is.data.frame))) {
      stop("When providing a list as date_tables, it must contain only data frames.")
    }
  } else if (is.character(date_tables)) {
    if (length(date_tables) == 1) {
      current_date_tables_extension <- tools::file_ext(x = date_tables)
      if (tolower(current_date_tables_extension) %in% c("rdata", "gdb")) {
        date_table_source <- date_tables
      } else if (is.character(input)) {
        current_input_extension <- tools::file_ext(x = input[[1]])
        if (tolower(current_input_extension) %in% c("gdb")) {
          date_tables_source <- input
        }
      } else {
        stop("date_tables must be a data frame; a list of data frames; the filepath to a geodatabase containing one or more of the tables tblLPIHeader, tblGapHeader, or tblSpecRichnessHeader; a vector of names of tables in the geodatabase specified as a filepath with the argument source or dsn; or the filepath to an Rdata file containing the relevant tables.")
      }

      if (tolower(tools::file_ext(date_tables_source)) %in% c("gdb")) {

      }
      # Trawl through the available feature classes with regex to accommodate
      # whatever prefixes the data team is using in the geodatabase these days.
      available_layers <- sf::st_layers(dsn = date_tables_source)$name

      date_tables_names <- available_layers[stringr::str_detect(string = available_layers,
                                                                pattern = "tbl((LPI)|(Gap)|(SpecRich))Header$")]

      if (length(date_tables_names) > 0) {
        date_tables <- lapply(X = date_tables_names,
                              source = date_tables_gdb,
                              FUN = function(X, source){
                                read_whatever(source = source,
                                              layer = X) |>
                                  sf::st_drop_geometry() |>
                                  # This'll happen again in a bit, but just to
                                  # save on memory we'll pare it down here.
                                  dplyr::select(.data = _,
                                                PrimaryKey,
                                                tidyselect::any_of(c(Date = "FormDate",
                                                                     Date = "CollectDate"))) |>
                                  dplyr::distinct()
                              })
      }
    }
  }



}


if(!is.null(dsn) & is.null(date_tables)){
  available_layers <- sf::st_layers(dsn = dsn)$name

  desired_date_tables <- c("tblLPIHeader",
                           "tblGapHeader",
                           "tblSpecRichHeader")

  if (length(base::intersect(x = desired_date_tables,
                             y = available_layers)) > 0) {
    date_tables <- lapply(X = base::intersect(x = desired_date_tables,
                                              y = available_layers),
                          dsn = dsn,
                          FUN = function(X, dsn){
                            message(paste("Reading dates from",
                                          X))
                            sf::st_read(dsn = dsn,
                                        layer = X,
                                        stringsAsFactors = FALSE,
                                        quiet = TRUE) |>
                              sf::st_drop_geometry() |>
                              # This'll happen again in a bit, but just to
                              # save on memory we'll pare it down here.
                              dplyr::select(.data = _,
                                            PrimaryKey,
                                            tidyselect::any_of(c(Date = "FormDate",
                                                                 Date = "CollectDate"))) |>
                              dplyr::distinct()
                          })
  }
}

if(is.null(date_tables)){
  stop("date_tables must be provided if dsn is not. Provide a list of tables containing FormDate or collectDate")
}

if(class(date_tables) != "list"){
  stop("date_tables must be a list of minimum length 1")
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
