#### HEADERS ###################################################################
#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @param header Dataframe. Plot header containing plot metadata
#' @param source String. Specifies data source, \code{"AIM", "LMF"}
#' @param ... Query in grepl format that subsets plots.
#' @return A \code{tbl} of indicators of either tall or wide format.


# Build the header portion of the terradat table
#' @export gather_header_terradat
#' @rdname aim_gdb
gather_header_terradat <- function(dsn = NULL, tblPlots = NULL,
                                   date_tables = NULL, ...) {
  # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # tblPlots provides the link between species tables
  if(!is.null(tblPlots)){
    header <- tblPlots
  } else if (!is.null(dsn)){
    # (LPI, Height, Species Richness) and tblStateSpecies
    header <- sf::st_read(
      dsn = dsn, layer = "tblPlots",
      stringsAsFactors = FALSE
    )
  } else {
    stop("Provide either tblPlots or a path to a GDB containing it")
  }

  header <- header %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs)

  # data from different sources / years capitalize this differently.
  if("DateLoadedInDB" %in% colnames(header) | !("DateLoadedInDb" %in% colnames(header))) {
    header$DateLoadedInDb <- header$DateLoadedInDB
  }

  # add these fields if missing
  if(!("Design" %in% colnames(header))) header$Design <- NA
  if(!("DesignFlag" %in% colnames(header))) header$DesignFlag <- NA
  if(!("Purpose" %in% colnames(header))) header$Purpose <- NA
  if(!("PurposeFlag" %in% colnames(header))) header$PurposeFlag <- NA
  if(!("ProjectName" %in% colnames(header))) header$ProjectName <- NA

  header <- header %>%
    # Select the field names we need in the final feature class
    dplyr::select(PrimaryKey, SpeciesState, PlotID, PlotKey,
                  # DBKey,
                  EcologicalSiteId = EcolSite, Latitude_NAD83 = Latitude, Longitude_NAD83 = Longitude, State,
                  Elevation,
                  County, DateEstablished = EstablishDate, DateLoadedInDb,
                  Design, DesignFlag, Purpose, PurposeFlag,
                  ProjectName
    ) %>%

    # If there are any Sites with no PrimaryKeys, delete them
    subset(!is.na(PrimaryKey))

  ## get date from all tables provided to date_tables
  # if date_tables is not provided, load all of lpi gap and species richness headers, as present in the geodatabase
  if(!is.null(dsn) & is.null(date_tables)){
    layernames <- sf::st_layers(dsn)$name
    if("tblLPIHeader" %in% layernames){
      print("Reading dates from tblLPIHeader")
      tblLPIHeader <- sf::st_read(dsn, "tblLPIHeader")
    }
    if("tblGapHeader" %in% layernames){
      print("Reading dates from tblGapHeader")
      tblGapHeader <- sf::st_read(dsn, "tblGapHeader")
    }
    if("tblSpecRichHeader" %in% layernames){
      print("Reading dates from tblSpecRichHeader")
      tblSpecRichHeader <- sf::st_read(dsn, "tblSpecRichHeader")
    }

    date_tables <- list(tblLPIHeader, tblGapHeader, tblSpecRichHeader)
  }

  if(is.null(date_tables) & is.null(dsn)){
    stop("date_tables must be provided if dsn is not. Provide a list of tables containing FormDate or collectDate")
  }

  if(class(date_tables) != "list"){
    stop("date_tables must be a list of minimum length 1")
  }


  # tblHorizontalFlux uses collectDate, most other tables use FormDate
  tblDate <- lapply(date_tables, function(date_table){
    if("FormDate" %in% colnames(date_table)) {
      out <- date_table %>% dplyr::select(PrimaryKey, Date = FormDate)
    } else if("collectDate" %in% colnames(date_table)) {
      out <- date_table %>% dplyr::select(PrimaryKey, Date = collectDate)
    } else {
      out <- data.frame()
    }

    return(out)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(DateVisited = dplyr::first(na.omit(Date), order_by = na.omit(Date)))

  header <- header %>% dplyr::left_join(tblDate, by = c("PrimaryKey"))

  # Return the header file
  return(header)
}

# Build the header portion of the LMF table
#' @export gather_header_lmf
#' @rdname aim_gdb
gather_header_lmf <- function(dsn = NULL,  ...) {
  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  point <- sf::read_sf(dsn = dsn,
                       layer = "POINT") |>
    sf::st_drop_geometry(onj = _) |>
    # Filter using the filtering expression specified by user
    dplyr::filter(.data = _,
                  !!!filter_exprs) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(c("PrimaryKey",
                                       "SpeciesState",
                                       "COUNTY",
                                       "STATE")))

  # County and State are referred to by number codes, let's use the name
  point <- sf::st_read(dsn = dsn,
                       layer = "COUNTYNM",
                       stringsAsFactors = FALSE) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(c("COUNTY",
                                       "COUNTYNM",
                                       "STATE"))) |>
    dplyr::distinct() |>
    dplyr::left_join(x = point,
                     y = _,
                     relationship = "many-to-one",
                     by = c("COUNTY",
                            "STATE")) |>
    # Add state
    dplyr::left_join(x = _,
                     y= sf::st_read(dsn = dsn,
                                    layer = "STATENM",
                                    stringsAsFactors = FALSE) |>
                       dplyr::select(.data = _,
                                     STATE,
                                     STABBR),
                     relationship = "many-to-one",
                     by = "STATE") |>
    dplyr::rename(.data = _,
                  County = COUNTYNM,
                  State = STABBR) |>
    # Pare down to needed fields
    dplyr::select(.data = _,
                  tidyselect::all_of(c("PrimaryKey",
                                       "SpeciesState",
                                       "County",
                                       "State"))) |>
    dplyr::mutate(.data = _,
                  PlotKey = PrimaryKey) |>
    dplyr::distinct()

  # Get the field coordinates
  point_coordinate <- sf::st_read(dsn = dsn,
                                  layer = "POINTCOORDINATES",
                                  stringsAsFactors = FALSE) |>
    sf::st_drop_geometry() |>
    dplyr::select(.data = _,
                  PrimaryKey,
                  Latitude_NAD83 = REPORT_LATITUDE,
                  Longitude_NAD83 = REPORT_LONGITUDE,
                  LocationType) |>
    dplyr::left_join(x = point,
                     y = _,
                     relationship = "one-to-one",
                     by = "PrimaryKey")

  # Add elevation data
  point_elevation <- sf::read_sf(
    dsn = dsn,
    layer = "GPS"
  ) %>%
    dplyr::select(PrimaryKey,
                  DateVisited = CAPDATE, # The GPS capture date is the best approx
                  Elevation = ELEVATION
    ) %>%
    dplyr::left_join(point_coordinate, .,
                     by = "PrimaryKey"
    ) %>%

    # Convert elevation to meters
    dplyr::mutate(Elevation = Elevation * 0.3048)

  # Add Ecological Site Id
  point_ESD_raw <- sf::st_read(dsn,
                               layer = "ESFSG",
                               stringsAsFactors = FALSE
  )

  # add in ESFSG_PREFIX column to old data in order to keep up with LMF schema changes
  if(!"ESFSG_PREFIX" %in% colnames(point_ESD_raw)) point_ESD_raw$ESFSG_PREFIX <- ""

  point_ESD <- point_ESD_raw %>%
    dplyr::left_join(point_elevation, ., by = "PrimaryKey") %>%

    # If the ESD coverage !=all, figure what portion of the plot the dominant ESD
    # is on the plot by taking the End_Mark-Start_Mark and dividng by the line length
    dplyr::mutate(
      ESD_coverage =
        dplyr::if_else(
          condition = COVERAGE == "all",
          true = as.integer(300),
          false = (END_MARK - START_MARK)
        ),
      # LMF schema is being updated to include F/R prefix under the column ESFSG_PREFIX
      # replace NA's with "" in ESFSG_PREFIX
      ESFSG_PREFIX = tidyr::replace_na(ESFSG_PREFIX, ""),
      EcologicalSiteId = trimws(paste(ESFSG_PREFIX, ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE, sep = "")),
      MLRA = ESFSG_MLRA %>% gsub("^$", NA, .)
    ) %>%

    # Add up the coverage on each plot and get the percent coverage
    dplyr::group_by(PrimaryKey, EcologicalSiteId) %>%
    dplyr::summarise(PercentCoveredByEcoSite = 100 * sum(ESD_coverage) / 300) %>%

    # Arrange by ESD_coverage and find the dominant ecological site
    dplyr::ungroup() %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::arrange(dplyr::desc(PercentCoveredByEcoSite), .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

    # Join to point.elevation to build the final header
    dplyr::left_join(point_elevation, ., by = "PrimaryKey")

  # Return the point_ESD as the header file
  point_ESD <- point_ESD %>% dplyr::mutate(PlotID = PrimaryKey)

  return(point_ESD)
}

# Build the header portion of the LMF table
#' @export gather_header_nri
#' @rdname aim_gdb
gather_header_nri <- function(dsn = NULL, speciesstate, ...) {
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

# Build the header portion of the Survey123 table
#' export gather_header_survey123
#' rdname aim_gdb
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
#
# Build the header wrapper
#' @export gather_header
#' @rdname aim_gdb
# Header build wrapper function
gather_header <- function(dsn = NULL, source, tblPlots = NULL, date_tables = NULL, #PlotChar_0 = NULL,
                          speciesstate = NULL, ..., autoQC = TRUE) {
  # Error check
  # Check for a valid source
  try(if (!toupper(source) %in% c("AIM", "TERRADAT", "DIMA", "LMF", "NRI")) {
    stop("No valid source provided")
  })

  # Apply appropriate header function

  header <- switch(toupper(source),
                   "LMF" = gather_header_lmf(dsn = dsn, ...),
                   "NRI" = gather_header_nri(dsn = dsn, speciesstate = speciesstate, ...),
                   "TERRADAT" = gather_header_terradat(dsn = dsn, tblPlots = tblPlots, date_tables = date_tables, ...),
                   "AIM" = gather_header_terradat(dsn = dsn, tblPlots = tblPlots, date_tables = date_tables, ...),
                   "DIMA" = gather_header_terradat(dsn = dsn, tblPlots = tblPlots, date_tables = date_tables, ...)#,
                   # "SURVEY123" = gather_header_survey123(PlotChar = PlotChar_0, speciesstate = speciesstate)
  )

  header$source <- source

  if("sf" %in% class(header)) header <- sf::st_drop_geometry(header)

  # Apply QC helper functions to remove duplicates
  if(autoQC){
    message("Checking for duplicated rows. Disable by adding the parameter 'autoQC = FALSE'")
    header <- tdact_remove_duplicates(header)
  }

  return(header)
}


#### LINE-POINT INTERCEPT ######################################################
#' Convert line-point intercept (LPI) data into a tall, tidy data frame
#'
#' @description Given wide format line-point intercept data, create a tall
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblLPIDetail and
#' tblLPIHeader (AIM/DIMA/TerrADat) or PINTERCEPT (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblLPIHeader Dataframe of the data structure tblLPIHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblLPIDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblLPIDetail Dataframe of the data structure tblLPIDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblLPIHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param PINTERCEPT Dataframe of the data structure PINTERCEPT from the LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when source
#' is LMF or NRI; alternately provide dsn.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if PINTERCEPT is provided.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_lpi
#' @family <gather>
#' @return A tall data frame containing the data from the LPI pin intercepts
#' @examples
#' gather_lpi(dsn = "Path/To/AIM_Geodatabase.gdb",
#'            source = "AIM")
#' gather_lpi(dsn = "Path/To/LMF_Geodatabase.gdb",
#'            source = "LMF")
#'
#' aim_lpidetail <- read.csv("Path/To/tblLPIDetail.csv")
#' aim_lpiheader <- read.csv("Path/To/tblLPIHeader.csv")
#' gather_lpi(source = "AIM",
#'            tblLPIDetail = aim_lpidetail,
#'            tblLPIHeader = aim_lpiheader)
#'
#' lmf_pintercept <- read.csv("Path/To/PINTERCEPT.csv")
#' gather_lpi(source = "LMF",
#'            RANGEHEALTH = lmf_pintercept)

## Function to make tall format of LPI data from TerrADat
#' @export gather_lpi_terradat
#' @rdname gather_lpi
gather_lpi_terradat <- function(dsn = NULL,
                                tblLPIDetail = NULL,
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
                         "DBKey",
                         "rid",
                         "DataErrorChecking",
                         "DataEntry",
                         "DateModified",
                         "FormType")

  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(tblLPIDetail) & !is.null(tblLPIHeader)) {
    if (verbose) {
      if (!is.null(dsn)) {
        message("Using the provided data frames. The provided dsn value is being ignored.")
      }
    }
    detail <- tblLPIDetail
    header <- tblLPIHeader
  } else if(!is.null(dsn)){
    if (verbose) {
      message("Attempting to use the provided dsn value.")
    }
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
    }
    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblLPIDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))
    header <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblLPIHeader",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))
  } else {
    stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing tables with those names.")
  }

  # Clean these up!
  detail <- dplyr::select(.data = detail,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

  header <- dplyr::select(.data = header,
                          -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

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
                  !code %in% c(NA, "N", "None", "")) |>
    dplyr::distinct(.data = _)

  # test <- dplyr::full_join(x = dplyr::mutate(lpi_hits_tall,
  #                                            original = TRUE),
  #                          y = dplyr::mutate(lpi_hits_tall_test,
  #                                            test = TRUE))
  # any(is.na(test$original))
  # any(is.na(test$test))

  # Make a tall data frame the checkbox status by layer.
  lpi_chkbox_tall <- dplyr::mutate(.data = detail,
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
    # Remove the records with invalid checkbox values.
    dplyr::filter(.data = _,
                  chckbox %in% c(1, 0)) |>
    # And adjusting so that the non-numbered layers match those values we expect
    dplyr::mutate(.data = _,
                  layer = dplyr::case_when(layer == "Top" ~ "TopCanopy",
                                           layer == "Soil" ~ "SoilSurface",
                                           .default = layer)) |>
    dplyr::distinct(.data = _)

  # test <- dplyr::full_join(x = dplyr::mutate(lpi_chkbox_tall,
  #                                            original = TRUE),
  #                          y = dplyr::mutate(lpi_chkbox_tall_test,
  #                                            test = TRUE))
  # any(is.na(test$original))
  # any(is.na(test$test))


  # Print update because this function can take a while
  if (verbose) {
    message("Merging the header and detail tables")
  }

  # Join the header information to the hit and checkbox data.
  # The suppressWarnings() and lack of defined relationships in the joins are to
  # allow the user to run this with data that have not been adequately cleaned.
  lpi_tall <- suppressWarnings(dplyr::left_join(x = lpi_hits_tall,
                                                y = lpi_chkbox_tall,
                                                # relationship = "one-to-one",
                                                by = c("PrimaryKey", "RecKey",
                                                       "PointLoc", "PointNbr",
                                                       "layer")) |>
                                 dplyr::left_join(x = dplyr::select(.data = header,
                                                                    LineKey:CheckboxLabel,
                                                                    PrimaryKey),
                                                  y = _,
                                                  # relationship = "one-to-many",
                                                  by = c("PrimaryKey", "RecKey")))

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

#' @export gather_lpi_lmf
#' @rdname gather_lpi

gather_lpi_lmf <- function(dsn = NULL,
                           file_type = "gdb",
                           PINTERCEPT = NULL) {
  #### Reading and cleanup #####################################################
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(PINTERCEPT)) {
    pintercept <- PINTERCEPT
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a database containing PINTERCEPT")
    }
    # Read  PINTERCEPT table in .txt or .gdb or from a preformatted csv
    pintercept <- switch(file_type,
                         "gdb" = {
                           sf::st_read(dsn = dsn,
                                       layer = "PINTERCEPT",
                                       stringsAsFactors = FALSE,
                                       quiet = TRUE) |>
                             suppressWarnings()
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

      colnames <- colnames[1:ncol(pintercept)] %>% subset(!is.na(.))
      names(pintercept) <- colnames
    }
  } else {
    stop("Supply either PINTERCEPT or the path to a GDB containing that table")
  }

  # Sometimes NAs might be introduced as variable names, but we can make sure to
  # drop those.
  pintercept <- pintercept[, !is.na(colnames(pintercept))]

  ##### Making sure these are distinct records ---------------------------------
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

  pintercept <- dplyr::select(.data = pintercept,
                              -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

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
gather_lpi_nps <- function(dsn) {
  lpi_raw <- read.csv(dsn)

  # add plot metadata
  lpi_raw <- lpi_raw %>% dplyr::mutate(
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

#' export gather_lpi_survey123
#' rdname gather_lpi
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



#' @export gather_lpi
#' @rdname gather_lpi

# Wrapper gather.lpi function
gather_lpi <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblLPIDetail = NULL,
                       tblLPIHeader = NULL,
                       PINTERCEPT = NULL,
                       autoQC = TRUE
                       # LPI_0 = NULL,
                       # LPIDetail_1 = NULL
) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    lpi <- gather_lpi_terradat(dsn = dsn,
                               tblLPIDetail = tblLPIDetail,
                               tblLPIHeader = tblLPIHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    lpi <- gather_lpi_lmf(dsn = dsn,
                          file_type = file_type,
                          PINTERCEPT = PINTERCEPT)
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
  lpi <- lpi %>%
    dplyr::select(PrimaryKey, LineKey, tidyselect::everything())

  # Drop rows with no data
  lpi <- lpi %>%
    dplyr::filter(!(is.na(LineKey) &
                      is.na(layer) &
                      is.na(code) &
                      is.na(ShrubShape) &
                      is.na(PointNbr)))

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    lpi <- lpi %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "lpi")
  }

  return(lpi)
}

#### HEIGHT ####################################################################
#' Convert height data into a tall, tidy data frame
#'
#' @description Given wide format line-point intercept data, create a tall
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblLPIDetail and
#' tblLPIHeader (AIM/DIMA/TerrADat) or PASTUREHEIGHTS (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblLPIHeader Dataframe of the data structure tblLPIHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblLPIDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblLPIDetail Dataframe of the data structure tblLPIDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblLPIHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param PASTUREHEIGHTS Dataframe of the data structure PASTUREHEIGHTS from the
#' LMF/NRI database; alternately provide \code{dsn}.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if \code{PASTUREHEIGHTS} is provided.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_height
#' @family <gather>
#' @return A tall data frame containing the data from the height measurements.
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

## Gather Height Data
#' @export gather_height_terradat
#' @rdname gather_height
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

  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(tblLPIDetail) & !is.null(tblLPIHeader)) {
    if (verbose) {
      if (!is.null(dsn)) {
        message("Using the provided data frames. The provided dsn value is being ignored.")
      }
    }
    detail <- tblLPIDetail
    header <- tblLPIHeader
  } else if(!is.null(dsn)){
    if (verbose) {
      message("Attempting to use the provided dsn value.")
    }
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
    }

    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblLPIDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))
    header <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblLPIHeader",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))
  } else {
    stop("Supply either tblLPIDetail and tblLPIHeader, or the path to a GDB containing tables with those names.")
  }

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

  # Warn about introducing NAs by coercion.
  nas_by_coercion <- sapply(X = c(woody = "Woody",
                                  herbaceous = "Herbaceous",
                                  'lower herbaceous' = "LowerHerb"),
                            detail = detail,
                            FUN = function(X, detail){
                              if (class(detail[[paste0("Height", X)]]) %in% c("integer",
                                                                              "numeric")) {
                                0
                              } else {
                                tidyr::replace_na(data = detail[[paste0("Height", X)]],
                                                  replace = "placeholder") |>
                                  as.numeric() |>
                                  suppressWarnings() |>
                                  is.na() |>
                                  sum()
                              }
                            })
  nas_by_coercion <- nas_by_coercion[nas_by_coercion > 0]
  if (length(nas_by_coercion) > 0) {
    warning(paste0("There are non-numeric values in at least one height variable in tblLPIDetail. There will be ",
                   sum(nas_by_coercion),
                   " invalid height values replaced with NA across the following height types: ",
                   paste(names(nas_by_coercion),
                         collapse = ", "),
                   ". Any records with a height value of NA will be dropped from the output during processing."))
  }



  # There are three height types that we're going to be working with here, so
  # we'll deal with each independently and then mash them together with
  # dplyr::bind_rows().
  # This approach with the lapply() and renaming makes more sense than doing
  # pivoting with tidyr functions because the value types are mixed (e.g., the
  # species are stored as character strings but heights are numeric) so we can't
  # put them all in one data frame variable when we pivot the data to a long
  # format.
  lpi_heights_tall <- lapply(X = c(woody = "Woody",
                                   herbaceous = "Herbaceous",
                                   lower_herbaceous = "LowerHerb"),
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

#' @export gather_height_lmf
#' @rdname gather_height

# Gather Height for LMF/NRI
gather_height_lmf <- function(dsn = NULL,
                              file_type = "gdb",
                              PASTUREHEIGHTS = NULL) {
  #### Reading and cleanup #####################################################
  if(!is.null(PASTUREHEIGHTS)){
    vegheight <- PASTUREHEIGHTS
  } else if (!is.null(dsn)) {
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing PASTUREHEIGHTS")
    }

    # Read in the data as .txt or .gdb
    vegheight <- switch(file_type,
                        "gdb" = {
                          suppressWarnings(sf::st_read(dsn,
                                                       layer = "PASTUREHEIGHTS",
                                                       stringsAsFactors = FALSE,
                                                       quiet = T
                          ))
                        },
                        "txt" = {
                          read.table(paste(dsn, "pastureheights.txt", sep = ""),
                                     stringsAsFactors = FALSE,
                                     header = FALSE,
                                     sep = "|",
                                     strip.white = TRUE
                          )
                        },
                        "csv" = {
                          read.csv(dsn)
                        }
    )

    if (file_type == "txt") {
      # if it is in a text file, there are no field names assigned.
      colnames <- subset(
        terradactyl::nri.data.column.explanations,
        TABLE.NAME == "PASTUREHEIGHTS"
      ) |>
        dplyr::pull(FIELD.NAME) |>
        unique()

      vegheight <- vegheight[seq_len(length(colnames))]
      names(vegheight) <- colnames

      # We need to establish and/or fix the PLOTKEY so it exists in a single field.
      vegheight$PrimaryKey <- paste(vegheight$SURVEY,
                                    vegheight$STATE,
                                    vegheight$COUNTY,
                                    vegheight$PSU,
                                    vegheight$POINT,
                                    sep = ""
      )

      # Assign DBKey
      vegheight$DBKey <- vegheight$SURVEY
    }




  } else {
    stop("Supply either PASTUREHEIGHTS or a path to a gdb containing that table")
  }

  ##### Making sure these are distinct records ---------------------------------
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

  vegheight <- dplyr::select(.data = vegheight,
                             -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

  ##### Dealing with intersecting transects ------------------------------------
  # The arrangement of the transects for an LMF point crosses in the middle of
  # the transects (point 75) and so that intersection gets recorded twice, once
  # per transect. We'll drop the 75th record on the northeast-southwest transect
  # but we also want to warn the user that it's happening to any situations
  # where the assumption that they're identical is violated.
  duplicated_75mark_indices <- dplyr::filter(.data = vegheight,
                                             DISTANCE == 75) |>
    dplyr::select(.data = _,
                  -TRANSECT) |>
    duplicated(x = _)
  vegheight_75mark <- dplyr::filter(.data = vegheight,
                                    DISTANCE == 75)
  vegheight_75mark[["duplicated"]] <- duplicated_75mark_indices
  vegheight_75mark_summary <- dplyr::summarize(.data = vegheight_75mark,
                                               .by = PrimaryKey,
                                               n_records = dplyr::n(),
                                               has_duplicate = any(duplicated))
  if (any(!vegheight_75mark_summary$has_duplicate)) {
    warning(paste0("There are ", sum(!vegheight_75mark_summary$has_duplicate),
                   " plots where the height records at the 75th sampling locations on the two transects are not identical to each other despite being the intersection of those transects. The records associated with the 'nesw' transects will still be dropped for these plots."))
  }

  vegheight <- dplyr::filter(.data = vegheight,
                             !(TRANSECT == "nesw" & DISTANCE == 75))

  #### Reformatting and harmonizing ############################################
  # Most of the heavy lifting!
  # This will get us to the point where there's a separate record for each type
  # of measurement (woody and nonwoody) but there's a little more to adjust
  # after that.
  data_long <- dplyr::select(.data = vegheight,
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

#' export gather_height_survey123
#' rdname gather_height
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

#' Gather Height wrapper for all data types
#' @export gather_height
#' @rdname gather_height

gather_height <- function(dsn = NULL,
                          file_type = "gdb",
                          source,
                          tblLPIDetail = NULL,
                          tblLPIHeader = NULL,
                          PASTUREHEIGHTS = NULL,
                          autoQC = TRUE#,
                          # LPI_0 = NULL,
                          # LPIDetail_1 = NULL
) {
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    height <- gather_height_terradat(
      dsn = dsn,
      tblLPIHeader = tblLPIHeader,
      tblLPIDetail = tblLPIDetail
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    height <- gather_height_lmf(
      dsn = dsn, file_type = file_type,
      PASTUREHEIGHTS = PASTUREHEIGHTS
    )
    # } else if(toupper(source) %in% c("SURVEY123")){
    #   height <- gather_height_survey123(
    #     LPI_0 = LPI_0,
    #     LPIDetail_1 = LPIDetail_1
    #   )
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
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
                          PrimaryKey, LineKey,
                          tidyselect::everything())

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    height <- height %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "height")
  }

  # Output height
  return(height)
}


#### GAP #######################################################################
#' Convert gap data into a tall, tidy data frame
#'
#' @description Given wide format gap data, create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblGapDetail and
#' tblGapHeader (AIM/DIMA/TerrADat) or both of GINTERCEPT and POINT (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblGapHeader Dataframe of the data structure tblGapHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblGapDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblGapDetail Dataframe of the data structure tblGapDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblGapHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param GINTERCEPT Dataframe of the data structure GINTERCEPT from the LMF/NRI
#' database. Use with POINT when data source is LMF or NRI; alternately provide
#' dsn.
#' @param POINT Dataframe of the data structure POINT from the LMF database.
#' Use with GINTERCEPT when data source if LMF or NRI; alternately provide dsn.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if GINTERCEPT and POINT are provided.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_gap
#' @family <gather>
#' @return A tall data frame containing the data from the gap measurements.
#' @examples
#' gather_gap(dsn = "Path/To/AIM_Geodatabase.gdb",
#'            source = "AIM")
#' gather_gap(dsn = "Path/To/LMF_Geodatabase.gdb",
#'            source = "LMF")
#'
#' aim_gapdetail <- read.csv("Path/To/tblGapDetail.csv")
#' aim_gapheader <- read.csv("Path/To/tblGapHeader.csv")
#' gather_gap(source = "AIM",
#'            tblGapDetail = aim_gapdetail,
#'            tblGapHeader = aim_gapheader)
#'
#' lmf_gintercept <- read.csv("Path/To/GINTERCEPT.csv")
#' lmf_point <- read.csv("Path/To/POINT.csv")
#' gather_gap(source = "LMF",
#'            GINTERCEPT = lmf_gintercept,
#'            POINT = lmf_point)

## gather gap data
#' @export gather_gap_terradat
#' @rdname gather_gap
gather_gap_terradat <- function(dsn = NULL,
                                tblGapDetail = NULL,
                                tblGapHeader = NULL,
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

  ### switch by input types
  if(!is.null(tblGapDetail) & !is.null(tblGapHeader)){
    if (verbose) {
      if (!is.null(dsn)) {
        message("Using the provided data frames. The provided dsn value is being ignored.")
      }
    }
    detail <- tblGapDetail
    header <- tblGapHeader
  } else if(!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblGapDetail and tblGapHeader")
    }
    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    # Read tblGapDetail
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblGapDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))

    # Read tblGapHeader
    header <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblGapHeader",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))

  } else {
    stop("Provide both tblGapDetail and tblGapHeader or the path to a GDB containing those tables.")
  }

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
                                                                    .default  = .x)))



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
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            RecType = dplyr::case_when(PerennialsCanopy == 1 &
                                                         AnnualForbsCanopy == 0 &
                                                         AnnualGrassesCanopy == 0 &
                                                         OtherCanopy == 0 ~ "P",
                                                       .default = RecType))

  gap_tall
}

#' @export gather_gap_lmf
#' @rdname gather_gap
gather_gap_lmf <- function(dsn = NULL,
                           file_type = NULL,
                           GINTERCEPT = NULL,
                           POINT = NULL) {

  #### Reading and cleanup #####################################################
  # if file type is NULL, define it by checking the extension of dsn
  valid_file_types <- c("csv", "gdb", "txt")

  # if file type is NULL, define it by checking the extension of dsn
  if(is.null(file_type)){
    extension <- substr(dsn, nchar(dsn)-2, nchar(dsn))
    if(extension == "csv") {
      file_type <- "csv"
    } else if(extension == "gdb") {
      file_type <- "gdb"
    } else {
      file_type <- "txt"
    }
  }

  if(!is.null(GINTERCEPT) & !is.null(POINT)){
    gintercept <- GINTERCEPT
    point <- POINT
  } else if(!is.null(dsn) & file_type %in% c("gdb", "csv", "txt")){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a database containing GINTERCEPT and POINT")
    }
    gintercept <- switch(file_type,
                         "gdb" = {
                           suppressWarnings(sf::st_read(
                             dsn = dsn,
                             layer = "GINTERCEPT",
                             stringsAsFactors = FALSE,
                             quiet = T
                           )) %>%
                             dplyr::select_if(!names(.) %in% c(
                               'GlobalID',
                               'created_user',
                               'created_date',
                               'last_edited_user',
                               'last_edited_date'
                             ))
                         },
                         "txt" = {
                           read.table(paste(dsn, "gintercept.txt", sep = ""),
                                      stringsAsFactors = FALSE, strip.white = TRUE,
                                      header = FALSE, sep = "|"
                           )
                         },
                         "csv" = {
                           read.csv(dsn)
                         }
    )
    # Read in point file for other plot level information
    point <- switch(file_type,
                    "gdb" = {
                      suppressWarnings(sf::st_read(
                        dsn = dsn,
                        layer = "POINT",
                        stringsAsFactors = FALSE,
                        quiet = T
                      )) %>%
                        dplyr::select_if(!names(.) %in% c(
                          'GlobalID',
                          'created_user',
                          'created_date',
                          'last_edited_user',
                          'last_edited_date'
                        ))
                    },
                    "txt" = {
                      read.table(paste(dsn, "point.txt", sep = ""),
                                 stringsAsFactors = FALSE,
                                 strip.white = TRUE,
                                 header = FALSE, sep = "|"
                      )
                    },
                    "csv" = {
                      stop("csv not currently supported for gap data")
                      read.csv(point_dsn)
                    }
    )


    if (file_type == "txt") {
      # Add meaningful column names
      gintercept <- name_variables_nri(
        data = gintercept,
        table_name = "GINTERCEPT"
      )
      point <- name_variables_nri(
        data = gintercept,
        table_name = "POINT"
      )
    }
  }
  else {
    stop("Supply either GINTERCEPT and POINT, or the path to a GDB containing those tables")
  }

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

  gintercept <- dplyr::select(.data = gintercept,
                              -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()
  point <- dplyr::select(.data = point,
                         -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

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
                                               y = gintercept,
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
      warning("There are transects where it was indicated that perennial-only and all-plant canopy gaps were identical but only perennial records exist. The all-plant records will be inferred from the perennial-only records.")
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

#' @export gather_gap
#' @rdname gather_gap
gather_gap <- function(dsn = NULL,
                       file_type = "gdb",
                       source,
                       tblGapHeader = NULL,
                       tblGapDetail = NULL,
                       POINT = NULL,
                       GINTERCEPT = NULL,
                       autoQC = TRUE#,
                       # Gap_0 = NULL,
                       # GapDetail_1 = NULL
) {

  # Gather gap using the appropriate method
  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    gap <- gather_gap_terradat(dsn = dsn,
                               tblGapDetail = tblGapDetail,
                               tblGapHeader = tblGapHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    gap <- gather_gap_lmf(dsn = dsn,
                          file_type = file_type,
                          POINT = POINT,
                          GINTERCEPT = GINTERCEPT)
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
  gap <- gap %>%
    dplyr::select(PrimaryKey, LineKey, tidyselect::everything())

  # Drop rows with no data
  gap <- gap %>%
    dplyr::filter(!(is.na(Gap) &
                      is.na(GapEnd) &
                      is.na(GapMin) &
                      is.na(GapStart) &
                      is.na(LineKey) &
                      is.na(LineLengthAmount) &
                      is.na(Measure) &
                      is.na(RecType) &
                      is.na(SeqNo)))

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    gap <- gap %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "gap")
  }

  dplyr::distinct(gap)
}


#### SOIL STABILITY ############################################################
#' Convert soil stability data into tall, tidy data frame
#'
#' @description Given soil stability create a tall format data frame usable by
#' other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase containing the table of interest. This field
#' is unnecessary if you supply either both of tblSoilStabDetail and
#' tblSoilStabHeader (AIM/DIMA/TerrADat) or SOILHORIZON (LMF/NRI).
#' @param source Character string. The data source format, can be \code{"AIM"},
#' \code{"TerrADat"}, \code{"DIMA"}, \code{"LMF"}, or \code{"NRI"} (case independent).
#' @param tblSoilStabDetail Dataframe of the data structure tblSoilStabDetail
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param tblSoilStabHeader Dataframe of the data structure tblSoilStabHeader
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when data
#' source is LMF or NRI; alternately provide dsn.
#' @param auto_qc Logical. If \code{TRUE} then AIM/DIMA/TerrADat data will be
#' checked for non-unique and orphaned records before doing processing. If any
#' are found, a warning will be triggered but the gather will still be carried
#' out. It is strongly recommended that any identified issues be addressed to
#' avoid incorrect records in the output. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will report back
#' diagnostic information as console messages while it works. Defaults to
#' \code{FALSE}.
#' @importFrom magrittr %>%
#' @name gather_soil_stability
#' @family <gather>
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
#'                       SOILHORIZON = lmf_horizons)

#' @export gather_soil_stability_terradat
#' @rdname gather_soil_stability
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

  if(!is.null(tblSoilStabDetail) & !is.null(tblSoilStabHeader)){
    detail <- tblSoilStabDetail
    header <- tblSoilStabHeader
  } else if (!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblSoilStabDetail and tblSoilStabHeader")
    }
    # The suppressWarnings() here are so that it doesn't complain about pulling
    # tables without geometry. We know that's what should be happening.
    detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                           layer = "tblSoilStabDetail",
                                           stringsAsFactors = FALSE,
                                           quiet = TRUE))

    header <-suppressWarnings(sf::st_read(dsn = dsn,
                                          layer = "tblSoilStabHeader",
                                          stringsAsFactors = FALSE,
                                          quiet = TRUE))
  } else {
    stop("Supply either both tblSoilStabDetail and tblSoilStabHeader or a path to a GDB containing those tables.")
  }

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
                                                    "BoxNum")),
                    joining_variables = c("PrimaryKey",
                                          "RecKey"))
  }

  # Convert to tall format
  gathered <- tidyr::gather(detail,
                            key = variable,
                            value = value,
                            -PrimaryKey, -BoxNum, -RecKey,
                            na.rm = TRUE) |>
    dplyr::filter(.data = _,
                  value != "")

  detail_tall <- tidyr::pivot_longer(data = detail,
                                     cols = -tidyselect::all_of(c("PrimaryKey",
                                                                  "RecKey",
                                                                  "BoxNum")),
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
                         "Position",
                         "BoxNum")) |>
    #However, there are likely going to be records in the source data where
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

#' @export gather_soil_stability_lmf
#' @rdname gather_soil_stability
gather_soil_stability_lmf <- function(dsn = NULL,
                                      file_type = "gdb",
                                      SOILDISAG = NULL) {
  #### Reading and cleanup #####################################################
  if(!is.null(SOILDISAG)) {
    soildisag <- SOILDISAG
  } else if(!is.null(dsn)){
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing SOILDISAG")
    }

    soildisag <- switch(file_type,
                        "gdb" = {
                          suppressWarnings(sf::st_read(
                            dsn = dsn, layer = "SOILDISAG",
                            stringsAsFactors = FALSE, quiet = T
                          ))
                        },
                        "txt" = {
                          read.table(paste(dsn, "soildisag.txt", sep = ""),
                                     stringsAsFactors = FALSE,
                                     strip.white = TRUE, header = FALSE, sep = "|"
                          )
                        },
                        "csv" = {
                          read.csv(dsn)
                        }
    )

    # Add column names
    if (file_type == "txt") {
      soildisag <- name_variables_nri(
        data = soildisag,
        table_name = "SOILDISAG"
      )
    }

  } else {
    stop("Supply either SOILDISAG or a path to a gdb containing that table")
  }

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

  soildisag <- dplyr::select(.data = soildisag,
                             -tidyselect::any_of(internal_gdb_vars)) |>
    dplyr::distinct()

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

#' export gather_soil_stability_survey123
#' rdname gather_soil_stability
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
#' @export gather_soil_stability
#' @rdname gather_soil_stability
gather_soil_stability <- function(dsn = NULL,
                                  source,
                                  file_type = "gdb",
                                  tblSoilStabDetail = NULL,
                                  tblSoilStabHeader = NULL,
                                  SOILDISAG = NULL,
                                  autoQC = TRUE
) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    soil_stability <- gather_soil_stability_terradat(
      dsn = dsn,
      tblSoilStabDetail = tblSoilStabDetail,
      tblSoilStabHeader = tblSoilStabHeader)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    soil_stability <- gather_soil_stability_lmf(
      dsn = dsn,
      file_type = file_type,
      SOILDISAG = SOILDISAG)

  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  soil_stability$source <- source

  if("sf" %in% class(soil_stability)) soil_stability <- sf::st_drop_geometry(soil_stability)

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
  soil_stability <- soil_stability %>%
    dplyr::filter(!(
      is.na(Position) &
        is.na(Rating) &
        is.na(Veg)
    ))

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    soil_stability <- soil_stability %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "soilstab")
  }

  return(soil_stability)
}

#### INTERPRETING INDICATORS OF RANGELAND HEALTH ###############################
#' Convert Interpreting Indicators of Rangeland Health (IIRH) data into a tall,
#' tidy data frame
#'
#' @description Given wide format IIRH data, create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either both of tblQualHeader and
#' tblQualDetail (AIM/DIMA/TerrADat) or RANGEHEALTH (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblQualHeader Dataframe of the data structure tblQualHeader from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblQualDetail when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param tblQualDetail Dataframe of the data structure tblQualDetail from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use with
#' tblQualHeader when data source is AIM, DIMA, or TerrADat; alternately provide
#' dsn.
#' @param RANGEHEALTH Dataframe of the data structure RANGEHEALTH from the
#' LMF/NRI database. Use when data source if LMF or NRI; alternately provide
#' dsn.
#' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if RANGEHEALT is provided.
#' @importFrom magrittr %>%
#' @name gather_rangeland_health
#' @family <gather>
#' @return A tall data frame containing the data from the rangeland health
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

#' @export gather_rangeland_health_terradat
#' @rdname IIRH
gather_rangeland_health_terradat <- function(dsn = NULL,
                                             tblQualHeader = NULL,
                                             tblQualDetail = NULL) {

  if(!is.null(tblQualHeader) & !is.null(tblQualDetail)){
    IIRH_header <- tblQualHeader
    IIRH_detail <- tblQualDetail
  } else if(!is.null(dsn)) {
    # check file
    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing tblQualDetail and tblQualHeader")
    }

    # Read in tblQualHeader
    IIRH_header <- suppressWarnings(sf::st_read(dsn, layer = "tblQualHeader", stringsAsFactors = FALSE, quiet = T))

    # Read in tblQualDetail
    IIRH_detail <- suppressWarnings(sf::st_read(dsn, layer = "tblQualDetail", quiet = T))

  } else {
    stop("Provide either tblQualHeader and tblQualDetail or a path to a geodatabase containing those tables")
  }

  # Clean up the Indicators Table
  rangeland_health_indicators <- IIRH_detail %>%
    dplyr::mutate(
      indicator = Seq %>%
        as.character() %>%
        # Rename Seq from a number to an Indicator name
        stringr::str_replace_all(c(
          "\\b1\\b" = "RH_Rills",
          "\\b2\\b" = "RH_WaterFlowPatterns",
          "\\b3\\b" = "RH_PedestalsTerracettes",
          "\\b4\\b" = "RH_BareGround",
          "\\b5\\b" = "RH_Gullies",
          "\\b6\\b" = "RH_WindScouredAreas", #
          "\\b7\\b" = "RH_LitterMovement", #
          "\\b8\\b" = "RH_SoilSurfResisErosion", #
          "\\b9\\b" = "RH_SoilSurfLossDeg", #
          "\\b10\\b" = "RH_PlantCommunityComp", #
          "\\b11\\b" = "RH_Compaction", #
          "\\b12\\b" = "RH_FuncSructGroup", #
          "\\b13\\b" = "RH_DeadDyingPlantParts", #
          "\\b14\\b" = "RH_LitterAmount", #
          "\\b15\\b" = "RH_AnnualProd", #
          "\\b16\\b" = "RH_InvasivePlants", #
          "\\b17\\b" = "RH_ReprodCapabilityPeren"
        )),
      Rating = Rating %>%
        as.character() %>%
        stringr::str_replace_all(c(
          "1" = "NS",
          "2" = "SM",
          "3" = "M",
          "4" = "ME",
          "5" = "ET",
          "0" = NA
        ))
    ) %>%
    subset(!is.na(Rating)) %>%
    dplyr::select(RecKey, indicator, Rating) %>%
    dplyr::distinct() %>%
    tidyr::spread(key = indicator, value = Rating)

  # Attributes and then joined to Indicators
  IIRH <- dplyr::select(IIRH_header, DBKey, PrimaryKey, RecKey, DateLoadedInDb,
                        RH_HydrologicFunction = HFVxWRatingFinal,
                        RH_BioticIntegrity = BIVxWRatingFinal,
                        RH_SoilSiteStability = SSSVxWRatingFinal,
                        RH_CommentsBI = CommentBI,
                        RH_CommentsHF = CommentHF,
                        RH_CommentsSS = CommentSSS#,
                        # Observer,
                        # Recorder
  ) %>%

    # Add the indicators
    dplyr::left_join(rangeland_health_indicators, by = "RecKey")

  ## last drop
  IIRH <- IIRH %>% dplyr::select(
    -c(DateLoadedInDb)
  )

  return(IIRH)
}

#' @export gather_rangeland_health_lmf
#' @rdname IIRH
gather_rangeland_health_lmf <- function(dsn = NULL,
                                        file_type = NULL,
                                        RANGEHEALTH = NULL) {

  if(!is.null(RANGEHEALTH)){
    IIRH <- RANGEHEALTH
  } else if(!is.null(dsn)){


    if (!file.exists(dsn)) {
      stop("dsn must be a valid filepath to a geodatabase containing RHSUMMARY or the filepath to a text file containing RHSUMMARY")
    }

    # if file type is NULL, define it by checking the extension of dsn
    if(is.null(file_type)){
      extension <- substr(dsn, nchar(dsn)-2, nchar(dsn))
      if(extension == "csv") {
        file_type <- "csv"
      } else if(extension == "gdb") {
        file_type <- "gdb"
      } else {
        file_type <- "txt"
      }
    }

    # Read in the data as .txt or .gdb
    IIRH <- switch(file_type,
                   "gdb" = {
                     suppressWarnings(sf::st_read(dsn,
                                                  layer = "RANGEHEALTH",
                                                  stringsAsFactors = FALSE, quiet = T
                     ))
                   },
                   "txt" = {
                     read.table(paste(dsn, "rangehealth.txt", sep = ""),
                                stringsAsFactors = FALSE,
                                header = FALSE,
                                sep = "|",
                                strip.white = TRUE
                     )
                   },
                   "csv" = {
                     read.csv(dsn)
                   }
    )

    # if it is in a text file, there are no field names assigned.
    if (file_type == "txt") {
      IIRH <- name_variables_nri(
        data = IIRH,
        table_name = "RHSUMMARY"
      )
    }
  } else {
    stop("Provide RANGEHEALTH or a path to a geodatabase containing that table")
  }

  # Clean up the field names so they are human readable and match TerrAdat names
  IIRH_clean <- IIRH %>%
    dplyr::select(PrimaryKey, DBKey,
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
                  RH_HydrologicFunction = "HYDROLOGIC_FUNCTION",
    )

  return(IIRH_clean)
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




#' @export gather_rangeland_health
#' @rdname IIRH
#'
gather_rangeland_health <- function(dsn = NULL,
                                    source,
                                    file_type = NULL,
                                    tblQualHeader = NULL,
                                    tblQualDetail = NULL,
                                    RANGEHEALTH = NULL,
                                    autoQC = TRUE) {


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
  IIRH <- IIRH %>%
    dplyr::select(PrimaryKey, DBKey, tidyselect::everything())

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    IIRH <- IIRH %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "rh")
  }

  return(IIRH)
}


#### SPECIES INVENTORY #########################################################
#' Convert species inventory data into tall, tidy data frame
#'
#' @description Given species inventory data create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase containing the table of interest. This field
#' is unnecessary if you supply either both of tblSpecRichDetail and
#' tblSpecRichHeader (AIM/DIMA/TerrADat) or PLANTCENSUS (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblSpecRichDetail Dataframe of the data structure tblSpecRichDetail
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use with tblSpecRichHeader when data source is AIM, DIMA, or TerrADat;
#' alternately provide dsn.
#' @param tblSpecRichHeader Dataframe of the data structure tblSpecRichHeader
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use with tblSpecRichDetail when data source is AIM, DIMA, or TerrADat;
#' alternately provide dsn.
#' @param PLANTCENSUS Dataframe of the data structure PLANTCENSUS from LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when data
#' source is LMF or NRI; alternately provide dsn.
#' @importFrom magrittr %>%
#' @name gather_species_inventory
#' @family <gather>
#' @return A tall data frame containing species inventory data.
#' @examples
#' gather_species_inventory(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                          source = "AIM")
#' gather_species_inventory(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                          source = "LMF")
#'
#' aim_specrichdetail <- read.csv("Path/To/tblSpecRichDetail.csv")
#' aim_specrichheader <- read.csv("Path/To/tblSpecRichHeader.csv")
#' gather_species_inventory(source = "AIM",
#'                          tblSpecRichDetail = aim_specrichdetail,
#'                          tblSpecRichHeader = aim_specrichheader)
#'
#' lmf_census <- read.csv("Path/To/PLANTCENSUS.csv")
#' gather_species_inventory(source = "LMF",
#'                          PLANTCENSUS = lmf_census)

#' @export gather_species_inventory_terradat
#' @rdname gather_species_inventory
gather_species_inventory_terradat <- function(dsn = NULL,
                                              tblSpecRichDetail = NULL,
                                              tblSpecRichHeader = NULL) {

  if(!is.null(tblSpecRichDetail) & !is.null(tblSpecRichHeader)) {
    species_inventory_detail <- tblSpecRichDetail
    species_inventory_header <- tblSpecRichHeader
  } else if (!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblSpecRichDetail and tblSpecRichHeader")
    }


    # load raw tables
    species_inventory_detail <- suppressWarnings(sf::st_read(dsn,
                                                             layer = "tblSpecRichDetail",
                                                             stringsAsFactors = FALSE, quiet = T
    ))
    species_inventory_header <- suppressWarnings(sf::st_read(dsn,
                                                             layer = "tblSpecRichHeader",
                                                             stringsAsFactors = FALSE, quiet = T
    ))

  } else {
    stop("Supply either tblSpecRichDetail and tblSpecRichHeader, or the path to a GDB containing those tables")
  }

  # Add null DBKey column if not present
  if(!("DBKey" %in% colnames(species_inventory_header))) species_inventory_header$DBKey <- NA
  if(!("DBKey" %in% colnames(species_inventory_detail))) species_inventory_detail$DBKey <- NA

  # Make Species Inventory Detail  a tall dataframe
  species_detail_tall <- tall_species(species_inventory_detail = species_inventory_detail)

  # Join with header data and strip out NA codes
  species_inventory_tall <- dplyr::left_join(
    x = species_inventory_header,
    y = species_detail_tall#,
    # by = c("RecKey", "PrimaryKey")
  ) %>%
    subset(!is.na(Species)) %>%
    dplyr::select_if(!names(.) %in%
                       c("DateModified", "FormType", "DataEntry",
                         "DataErrorChecking", "DateLoadedInDb", "created_user", "created_date", "last_edited_user", "last_edited_date", "GlobalID")
    )

  return(species_inventory_tall)
}
#' @export species_count
#' @rdname gather_species_inventory
species_count <- function(species_inventory_tall, ...) {
  grouping_variables <- rlang::quos(...)

  if ("DBKey" %in% colnames(species_inventory_tall)) {
    levels <- rlang::quos(DBKey, PrimaryKey)
  } else {
    levels <- rlang::quos(PrimaryKey)
  }

  # make sure that there are a unique set of species for each grouping level
  species_inventory_tall <- species_inventory_tall %>%
    dplyr::select(
      !!!grouping_variables,
      !!!levels,
      Species
    ) %>%
    unique()

  species_count <- species_inventory_tall %>%
    dplyr::count(!!!levels, !!!grouping_variables) %>%
    tidyr::unite(indicator, !!!grouping_variables, sep = ".") %>%
    dplyr::filter(!grepl(indicator, pattern = "^NA$|\\.NA|NA\\.|\\.NA\\."))



  return(species_count)
}

#' @export tall_species
#' @rdname gather_species_inventory
tall_species <- function(species_inventory_detail) {
  tall_list <- lapply(1:nrow(species_inventory_detail), FUN = function(X, df) {
    # split species strings concatenated in a single field
    codes <- stringr::str_split(df[X, "SpeciesList"], pattern = ";")[[1]]

    # Format output
    output <- data.frame(
      "PrimaryKey" = df$PrimaryKey[X],
      "RecKey" = df$RecKey[X],
      "Species" = codes
    )
    return(output)
  }, df = species_inventory_detail)
  # Combine output
  output <- dplyr::bind_rows(tall_list)

  # Remove NAs and blanks
  output <- dplyr::filter(output, !(Species %in% c("", NA)))

  return(output)
}

# Gather LMF data
#' @export gather_species_inventory_lmf
#' @rdname gather_species_inventory
gather_species_inventory_lmf <- function(dsn = NULL,
                                         file_type = "gdb",
                                         PLANTCENSUS = NULL) {
  if(!is.null(PLANTCENSUS)){
    plantcensus <- PLANTCENSUS
  } else if(!is.null(dsn)){

    plantcensus <- switch(file_type,
                          "gdb" = {
                            suppressWarnings(sf::st_read(dsn,
                                                         layer = "PLANTCENSUS",
                                                         stringsAsFactors = FALSE, quiet = T
                            ))
                          },
                          "txt" = {
                            read.table(paste(dsn, "plantcensus.txt", sep = ""),
                                       stringsAsFactors = FALSE,
                                       header = FALSE, sep = "|",
                                       strip.white = TRUE
                            )
                          },
                          "csv" = {
                            read.csv(dsn,
                                     stringsAsFactors = FALSE
                            )
                          }
    )

    # if it is in a text file, there are no field names assigned.
    if (file_type == "txt") {
      plantcensus <- name_variables_nri(
        data = plantcensus,
        table_name = "PLANTCENSUS"
      )
    }

  } else {
    stop("Supply either PLANTCENSUS or the path to a GDB containing that table")
  }

  # Get species count
  species_inventory <- plantcensus %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::summarize(., SpeciesCount = dplyr::n(), .groups = "drop") %>%
    merge(., plantcensus)

  # rename fields
  species_inventory <- dplyr::rename(species_inventory,
                                     Species = CPLANT
  ) %>% dplyr::select(., -c(SURVEY:SEQNUM)) %>%
    dplyr::select_if(!names(.) %in% c("GlobalID", "created_user",
                                      "created_date", "last_edited_user", "last_edited_date"))

  return(species_inventory)
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
#' @export gather_species_inventory
#' @rdname gather_species_inventory
gather_species_inventory <- function(dsn = NULL,
                                     source,
                                     tblSpecRichDetail = NULL,
                                     tblSpecRichHeader = NULL,
                                     PLANTCENSUS = NULL,
                                     # SpeciesRichness_0 = NULL,
                                     # SpecRichDetail_1 = NULL,
                                     file_type = "gdb",
                                     autoQC = TRUE) {

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    species_inventory <- gather_species_inventory_terradat(
      dsn = dsn,
      tblSpecRichDetail = tblSpecRichDetail,
      tblSpecRichHeader = tblSpecRichHeader
    )
  } else if(toupper(source) %in% c("LMF", "NRI")){
    species_inventory <- gather_species_inventory_lmf(
      dsn = dsn, file_type = file_type,
      PLANTCENSUS = PLANTCENSUS
    )
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

  if("sf" %in% class(species_inventory)) species_inventory <- sf::st_drop_geometry(species_inventory)

  if (any(class(species_inventory) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(species_inventory)[do.call(rbind, vapply(species_inventory,
                                                                  class))[, 1] %in% c("POSIXct", "POSIXt")]
    species_inventory <- dplyr::mutate_at(species_inventory, dplyr::vars(change_vars),
                                          dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  species_inventory <- species_inventory %>%
    dplyr::select(PrimaryKey, tidyselect::everything())

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    species_inventory <- species_inventory %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "specinv")
  }

  return(species_inventory)
}



#### PLOT CHARACTERIZATION #####################################################
#' Convert plot data into a tall, tidy data frame
#'
#' @description Given wide format plot data, create a tall format data frame
#' usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either tblPlots (AIM/DIMA/TerrADat)
#' or POINT (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblPlots Dataframe of the data structure tblPlots from the
#' DIMA database with the addition of PrimaryKey and DBKey fields. Use when data
#' source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param POINT Dataframe of the data structure PINTERCEPT from the LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when source
#' is LMF or NRI; alternately provide dsn.
#' @param POINTCOORDINATES Dataframe of the data structure POINTCOORDINATES from the LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when source
#' is LMF or NRI; alternately provide dsn.
#' @param GPS Dataframe of the data structure GPS from the LMF/NRI
#' database with the addition of PrimaryKey and DBKey fields. Use when source
#' is LMF or NRI; alternately provide dsn.
#' #' @param file_type Character string that denotes the source file type of the
#' LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
#' AIM/DIMA/TerrADat, or if POINT, POINTCOORDINATES, and GPS are provided.
#' @importFrom magrittr %>%
#' @name gather_plot_characterization
#' @family <gather>
#' @return A tall data frame containing plot characterization data
#' @examples
#' gather_plot_characterization(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                              source = "AIM")
#' gather_plot_characterization(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                              source = "LMF")
#'
#' aim_plots <- read.csv("Path/To/tblPlots.csv")
#' gather_plot_characterization(source = "AIM",
#'                              tblPlots = aim_plots)
#'
#' lmf_pintercept <- read.csv("Path/To/PINTERCEPT.csv")
#' lmf_pointcoords <- read.csv("Path/To/POINTCOORDINATES.csv")
#' lmf_gps <- read.csv("Path/To/GPS.csv")
#' gather_plot_characterization(source = "LMF",
#'                              PINTERCEPT = lmf_pintercept,
#'                              POINTCOORDINATES = lmf_pointcoords,
#'                              GPS = lmf_gps)

#' @export gather_plot_characterization_terradat
#' @rdname gather_plot_characterization
gather_plot_characterization_terradat <- function(dsn = NULL,
                                                  tblPlots = NULL){
  if(!is.null(tblPlots)){
    plot_raw <- tblPlots
  } else if(!is.null(dsn)){
    plot_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblPlots", stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("Supply either tblPlots or the path to a GDB containing that table")
  }
  plot_tall <- plot_raw %>%
    dplyr::select_if(names(.) %in% c(
      'PrimaryKey', 'DBKey', 'ProjectKey',
      'Latitude', 'Longitude',
      'State', 'County',
      'EcolSite', 'ParentMaterial', 'Slope', 'Elevation', 'Aspect', 'ESD_SlopeShape',
      'LandscapeType', 'LandscapeTypeSecondary', #'HillslopeType',
      'ESD_Series',
      # 'Observer', 'Recorder',
      'EstablishDate'
      # 'ESD_Investigators'
    )) %>%
    dplyr::rename(
      Latitude_NAD83 = Latitude,
      Longitude_NAD83 = Longitude,
      SlopeShape = ESD_SlopeShape,
      SoilSeries = ESD_Series,
    ) %>%
    dplyr::mutate(
      SlopeShapeVertical = dplyr::case_when(
        SlopeShape %in% c("CC", "CV", "CL", "concave concave", "concave convex", "concave linear") ~ "concave",
        SlopeShape %in% c("LC", "LV", "LL", "linear concave", "linear convex", "linear linear") ~ "linear",
        SlopeShape %in% c("VC", "VV", "VL", "convex concave", "convex convex", "convex linear") ~ "convex"
      ),
      SlopeShapeHorizontal = dplyr::case_when(
        SlopeShape %in% c("CC", "LC", "VC", "concave concave", "linear concave", "convex concave") ~ "concave",
        SlopeShape %in% c("CL", "LL", "VL", "concave linear", "linear linear", "convex linear") ~ "linear",
        SlopeShape %in% c("CV", "LV", "VV", "concave convex", "linear convex", "convex convex") ~ "convex"
      ),
      Aspect = suppressWarnings(as.numeric(Aspect)),
      Slope = suppressWarnings(as.numeric(Slope)),
      Latitude_NAD83 = suppressWarnings(as.numeric(Latitude_NAD83)),
      Longitude_NAD83 = suppressWarnings(as.numeric(Longitude_NAD83)),
      PrimaryKey = as.character(PrimaryKey),
      MLRA = substr(EcolSite, 2, 5) %>% gsub("NKNO", NA, .)) %>%
    dplyr::select(-SlopeShape)

  return(plot_tall)
}

#' LMF plot characterization function
#' @export gather_plot_characterization_lmf
#' @rdname gather_plot_characterization
gather_plot_characterization_lmf <-   function(dsn = NULL,
                                               POINT = NULL,
                                               POINTCOORDINATES = NULL,
                                               GPS = NULL,
                                               ESFSG = NULL,
                                               file_type = NULL
) {
  ### input ####
  if (!is.null(POINT) & !is.null(POINTCOORDINATES) & !is.null(GPS) & !is.null(ESFSG)){
    point_lmf_raw <- POINT
    coord_lmf_raw <- POINTCOORDINATES
    gps_lmf_raw   <- GPS
    esfsg_lmf_raw <- ESFSG
  } else if(!is.null(dsn)){
    point_lmf_raw <-
      sf::st_read(dsn = dsn, layer = "POINT", stringsAsFactors = FALSE, quiet = T)

    coord_lmf_raw <-
      sf::st_read(dsn = dsn, layer = "POINTCOORDINATES", stringsAsFactors = FALSE, quiet = T)

    gps_lmf_raw <-
      sf::st_read(dsn = dsn, layer = "GPS", stringsAsFactors = FALSE, quiet = T)

    esfsg_lmf_raw <-
      sf::st_read(dsn = dsn, layer = "ESFSG", stringsAsFactors = FALSE, quiet = T)


  } else{
    stop("Supply either POINT, POINTCOORDINATES, ESFSG, and GPS, or the path to a GDB containing those tables")
  }

  # get slope shape from POINT
  point_lmf <- point_lmf_raw %>% dplyr::select(
    DBKey, PrimaryKey,
    SlopeShapeVertical = VERTICAL_SLOPE_SHAPE,
    SlopeShapeHorizontal = HORIZONTAL_SLOPE_SHAPE,
    Slope = SLOPE_PERCENT, Aspect = SLOPE_ASPECT
  ) %>% dplyr::mutate(
    # reclass aspect into degrees
    Aspect = suppressWarnings(as.numeric(dplyr::recode(Aspect,
                                                       "N" = "0",
                                                       "NE" = "45",
                                                       "E" = "90",
                                                       "SE" = "135",
                                                       "S" = "180",
                                                       "SW" = "225",
                                                       "W" = "270",
                                                       "NW" = "315")))
    # get MLRA from ecological site id
  )

  # get gis data from POINTCOORDINATES
  coord_lmf <- coord_lmf_raw %>% dplyr::select(
    PrimaryKey, DBKey,
    Latitude_NAD83 = REPORT_LATITUDE,
    Longitude_NAD83 = REPORT_LONGITUDE,
  )

  # get gis from GPS
  gps_lmf <- gps_lmf_raw %>% dplyr::select(
    PrimaryKey, DBKey,
    Elevation = ELEVATION
  )

  # get ecological site and mlra from ESFSG
  esfsg_lmf <- esfsg_lmf_raw %>% dplyr::mutate(
    EcolSite = paste0(ESFSG_MLRA, ESFSG_SITE, ESFSG_STATE),
    MLRA = ESFSG_MLRA %>% gsub("^$", NA, .)
  ) %>% dplyr::select(
    PrimaryKey, DBKey,
    EcolSite, MLRA
  )

  # join GIS
  gis_lmf  <- dplyr::full_join(coord_lmf, gps_lmf, by = c("PrimaryKey", "DBKey"))

  # join gis and plot data
  plot_lmf <- dplyr::left_join(point_lmf, gis_lmf, by = c("PrimaryKey", "DBKey")) %>%
    # and ecolsite data
    dplyr::left_join(esfsg_lmf, by = c("PrimaryKey", "DBKey"))


  # last drop
  plot_lmf <- plot_lmf %>% dplyr::select_if(!names(.) %in% c(
    "Shape", "StateNo", "CountyNo")
  )

  return(plot_lmf)
}

# export gather_plot_characterization_survey123
# rdname gather_plot_characterization
# gather_plot_characterization_survey123 <- function(dsn = NULL,
#                                                    PlotChar_0 = NULL){
#
#   if(!is.null(PlotChar_0)){
#     plot_raw <- PlotChar_0
#   } else if(!is.null(dsn)){
#     plot_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblPlots", stringsAsFactors = FALSE, quiet = T))
#   } else {
#     stop("Supply either tblPlots or the path to a GDB containing that table")
#   }
#
#   # Rename plotkey to primarykey
#   plot_raw$PrimaryKey <- plot_raw$PlotKey
#
#   # Add null DBKey
#   plot_raw$DBKey <- NA
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- plot_raw$PrimaryKey[duplicated(plot_raw$PrimaryKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   plot_tall <- plot_raw %>%
#     dplyr::select(
#       PrimaryKey, DBKey, # ProjectKey,
#       Latitude_NAD83 = y, Longitude_NAD83 = x,
#       # State, County,
#       EcolSite = Ecolsite, ParentMaterial, Slope, Elevation, Aspect, #ESD_SlopeShape,
#       SLopeShapeVertical = vertshape, SlopeShapeHorizontal = horizshape,
#       LandscapeType, LandscapeTypeSecondary, #HillslopeType,
#       SoilSeries = ESD_Series,
#       # Observer, Recorder,
#       EstablishDate = EstabDate
#       # ESD_Investigators
#     ) %>%
#     dplyr::mutate(
#       Aspect = suppressWarnings(as.numeric(Aspect)),
#       Slope = suppressWarnings(as.numeric(Slope)),
#       Latitude_NAD83 = suppressWarnings(as.numeric(Latitude_NAD83)),
#       Longitude_NAD83 = suppressWarnings(as.numeric(Longitude_NAD83)),
#       PrimaryKey = as.character(PrimaryKey),
#       MLRA = substr(EcolSite, 2, 5) %>% gsub("NKNO", NA, .))
#
#   return(plot_tall)
# }



#' Wrapper function
#' @export gather_plot_characterization
#' @rdname gather_plot_characterization
gather_plot_characterization <- function(dsn = NULL,
                                         source,
                                         tblPlots = NULL,
                                         POINT = NULL,
                                         POINTCOORDINATES = NULL,
                                         GPS = NULL,
                                         ESFSG = NULL,
                                         # PlotChar_0 = NULL,
                                         file_type = "gdb"){

  if(toupper(source) %in% c("AIM", "TERRADAT", "DIMA")){
    plotchar <- gather_plot_characterization_terradat(dsn = dsn,
                                                      tblPlots = tblPlots)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    plotchar <- gather_plot_characterization_lmf(dsn = dsn,
                                                 file_type = file_type,
                                                 POINT = POINT,
                                                 POINTCOORDINATES = POINTCOORDINATES,
                                                 GPS = GPS,
                                                 ESFSG = ESFSG)
    # } else if(toupper(source) == "SURVEY123"){
    # plotchar <- gather_plot_characterization_survey123(dsn = dsn,
    # PlotChar_0 = PlotChar_0)
  } else {
    stop("source must be AIM, TerrADat, DIMA, LMF, or NRI (all case independent)")
  }

  # plotchar$source <- toupper(source)
  plotchar$source <- source

  if("sf" %in% class(plotchar)) plotchar <- sf::st_drop_geometry(plotchar)

  if (any(class(plotchar) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(plotchar)[do.call(rbind, vapply(plotchar,
                                                         class))[, 1] %in% c("POSIXct", "POSIXt")]
    plotchar <- dplyr::mutate_at(plotchar, dplyr::vars(change_vars),
                                 dplyr::funs(as.character))
  }

  # reorder so that primary key is leftmost column
  plotchar <- plotchar %>%
    dplyr::select(PrimaryKey, DBKey, tidyselect::everything())

  return(plotchar)
}


#### SOIL HORIZONS #############################################################
#' Convert horizon data into a tall, tidy data frame
#'
#' @description Given wide format soil horizon data, create a tall
#' format data frame usable by other terradactyl functions.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing the table of interest.
#' This field is unnecessary if you provide either tblSoilPitHorizons
#' (AIM/DIMA/TerrADat) or SOILHORIZON (LMF/NRI).
#' @param source Character string. The data source format,
#' \code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).
#' @param tblSoilPitHorizons Dataframe of the data structure tblSoilPitHorizons
#' from the DIMA database with the addition of PrimaryKey and DBKey fields.
#' Use when data source is AIM, DIMA, or TerrADat; alternately provide dsn.
#' @param SOILHORIZON Dataframe of the data structure SOILHORIZON from the
#' LMF/NRI database with the addition of PrimaryKey and DBKey fields;
#' alternately provide dsn.
#' @importFrom magrittr %>%
#' @name gather_soil_horizon
#' @family <gather>
#' @return A tall data frame containing soil horzon data.
#' @examples
#' gather_soil_horizon(dsn = "Path/To/AIM_Geodatabase.gdb",
#'                     source = "AIM")
#' gather_soil_horizon(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                     source = "LMF")
#'
#' aim_horizons <- read.csv("Path/To/tblSoilPitHorizons.csv")
#' gather_soil_horizon(source = "AIM",
#'                     tblSoilPitHorizons = aim_horizons)
#'
#' lmf_horizons <- read.csv("Path/To/SOILHORIZON.csv")
#' gather_soil_horizon(source = "LMF",
#'                     SOILHORIZON = lmf_horizons)

#' @export gather_soil_horizon_terradat
#' @rdname gather_soil_horizon
gather_soil_horizon_terradat <- function(dsn = NULL,
                                         tblSoilPitHorizons = NULL){

  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(tblSoilPitHorizons)) {
    hz_aim_raw <- tblSoilPitHorizons
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing tblSoilPitHorizons")
    }

    hz_aim_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
                                               stringsAsFactors = FALSE, quiet = T))
  } else {
    stop("Supply either tblSoilPitHorizons, or the path to a GDB containing those tables")
  }

  horizons_aim <- hz_aim_raw %>%
    ### select ###
    dplyr::select(
      PrimaryKey, DBKey, HorizonKey, HorizonDepthUpper, HorizonDepthLower,
      DepthUOM = DepthMeasure, HorizonName = ESD_Horizon,
      Texture, TextureModifier = ESD_HorizonModifier,
      pH = ESD_pH, EC = ESD_EC, Effervescence = Effer,
      ClayPct = ESD_PctClay, SandPct = ESD_PctSand,

      StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure,
      # StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2,
      StructureQuality = ESD_StructQual,

      # PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic,
      # ClayFilm = ESD_ClayFilm,
      Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color,
      # RootSize = ESD_RootSize, RootQty = ESD_RootQty,

      Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
      Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
      Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,

      HorizonNotes = ESD_Notes

      ### cleaning ###
    ) %>%
    dplyr::mutate_all(
      stringr::str_trim # defensive, early qc seems to catch this well
    ) %>%
    ### recode class data###
    dplyr::mutate(
      StructureGrade = dplyr::recode(StructureGrade,
                                     "0" = "Structureless",
                                     "1" = "Weak",
                                     "2" = "Moderate",
                                     "3" = "Strong"),
      # StructureGrade2 = dplyr::recode(StructureGrade2,
      #                          "0" = "Structureless",
      #                          "1" = "Weak",
      #                          "2" = "Moderate",
      #                          "3" = "Strong"),
      StructureSize = dplyr::recode(StructureSize %>% tolower(),
                                    "vf" = "Very fine",
                                    "vn" = "Very thin",
                                    "f"  = "Fine",
                                    "tn" = "Thin",
                                    "m"  = "Medium",
                                    "co" = "Coarse",
                                    "tk" = "Thick",
                                    "vc" = "Very coarse",
                                    "vk" = "Very thick",
                                    "ec" = "Extremely coarse"),
      # StructureSize2 = dplyr::recode(StructureSize2 %>% tolower(),
      #                         "vf" = "Very fine",
      #                         "vn" = "Very thin",
      #                         "f"  = "Fine",
      #                         "tn" = "Thin",
      #                         "m"  = "Medium",
      #                         "co" = "Coarse",
      #                         "tk" = "Thick",
      #                         "vc" = "Very coarse",
      #                         "vk" = "Very thick",
      #                         "ec" = "Extremely coarse"),
      StructureType = dplyr::recode(StructureType %>% tolower(),
                                    "gr"  = "Granular",
                                    "abk" = "Angular blocky",
                                    "sbk" = "Subangular blocky",
                                    "pl"  = "Platy",
                                    "weg" = "Wedge",
                                    "pr"  = "Prismatic",
                                    "col" = "Columnar",
                                    "sg"  = "Single grain",
                                    "ma"  = "Massive",
                                    "cdy" = "Cloddy",
                                    "other" = "Other"),
      # StructureType2 = dplyr::recode(StructureType2 %>% tolower(),
      #                         "gr"  = "Granular",
      #                         "abk" = "Angular blocky",
      #                         "sbk" = "Subangular blocky",
      #                         "pl"  = "Platy",
      #                         "weg" = "Wedge",
      #                         "pr"  = "Prismatic",
      #                         "col" = "Columnar",
      #                         "sg"  = "Single grain",
      #                         "ma"  = "Massive",
      #                         "cdy" = "Cloddy",
      #                         "other" = "Other"),
    ) %>%
    ### complex mutates that depend on >1 var ###
    dplyr::mutate(
      SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
      FragVolGravel = dplyr::case_when(
        Fragment1Type %in% c("GR", "Gravel", "1") ~ Fragment1VolPct,
        Fragment2Type %in% c("GR", "Gravel", "1") ~ Fragment2VolPct,
        Fragment3Type %in% c("GR", "Gravel", "1") ~ Fragment3VolPct
      ),
      FragVolCobble = dplyr::case_when(
        Fragment1Type %in% c("CB", "Cobble", "2") ~ Fragment1VolPct,
        Fragment2Type %in% c("CB", "Cobble", "2") ~ Fragment2VolPct,
        Fragment3Type %in% c("CB", "Cobble", "2") ~ Fragment3VolPct
      ),
      FragVolStone = dplyr::case_when(
        Fragment1Type %in% c("ST", "Stone", "6") ~ Fragment1VolPct,
        Fragment2Type %in% c("ST", "Stone", "6") ~ Fragment2VolPct,
        Fragment3Type %in% c("ST", "Stone", "6") ~ Fragment3VolPct
      ),
      FragVolNodule = dplyr::case_when(
        Fragment1Type %in% c("8", "Nodule") ~ Fragment1VolPct,
        Fragment2Type %in% c("8", "Nodule") ~ Fragment2VolPct,
        Fragment3Type %in% c("8", "Nodule") ~ Fragment3VolPct
      ),
      FragVolDurinode = dplyr::case_when(
        Fragment1Type %in% c("9", "Durinode") ~ Fragment1VolPct,
        Fragment2Type %in% c("9", "Durinode") ~ Fragment2VolPct,
        Fragment3Type %in% c("9", "Durinode") ~ Fragment3VolPct
      ),
      HorizonDepthLower = dplyr::case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
      HorizonDepthUpper = dplyr::case_when(
        DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54,
        DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
      DepthUOM = "cm"
    ) %>%
    ### drop vars that are no longer relevant ###
    dplyr::select(
      -Fragment1Type,
      -Fragment2Type,
      -Fragment3Type,
      -Fragment1VolPct,
      -Fragment2VolPct,
      -Fragment3VolPct,
    )   %>%
    dplyr::arrange(PrimaryKey, HorizonDepthUpper) %>%
    dplyr::group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
      PrimaryKey
    ) %>%

    dplyr::mutate(HorizonNumber = as.character(dplyr::row_number()),
                  # across(c(RockFragments), ~ suppressWarnings(as.integer(.x))),
                  across(c(pH,
                           EC, ClayPct, SandPct, SiltPct, # poreqty,
                           FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule,
                           FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
                  ), ~ suppressWarnings(as.double(.x))),
                  # across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
    )
  horizons_aim <- as.data.frame(horizons_aim)

  return(horizons_aim)
}

#' @export gather_soil_horizon_lmf
#' @rdname gather_soil_horizon
gather_soil_horizon_lmf <- function(dsn = NULL,
                                    SOILHORIZON = NULL){
  # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
  if (!is.null(SOILHORIZON)){
    hz_lmf_raw <- SOILHORIZON
  } else if(!is.null(dsn)){
    if(!file.exists(dsn)){
      stop("dsn must be a valid filepath to a geodatabase containing SOILHORIZON")
    }
    hz_lmf_raw <- sf::st_read(dsn = dsn, layer = "SOILHORIZON", stringsAsFactors = FALSE, quiet = T)
  } else {
    stop("Supply either SOILHORIZON or the path to a GDB containing that table")
  }

  horizons_lmf <- hz_lmf_raw %>%
    dplyr::select(
      PrimaryKey, DBKey, HorizonNumber = SEQNUM,
      HorizonDepthLower = DEPTH, Effervescence = EFFERVESCENCE_CLASS,
      Texture = HORIZON_TEXTURE, TextureModifier = TEXTURE_MODIFIER,
      HorizonNotes = UNUSUAL_FEATURES
    )

  horizons_lmf <- horizons_lmf %>% # have to have already created horizons_lmf before the HorizonDepthUpper parsing below, as it refers to the df by name
    dplyr::mutate(
      DepthUOM = "cm",
      HorizonNumber = as.character(HorizonNumber),
      HorizonDepthUpper = sapply(unique(PrimaryKey), function(x) {
        lower <- horizons_lmf$HorizonDepthLower[horizons_lmf$PrimaryKey == x]
        upper <- c(0, lower[1:length(lower) - 1])
        return(upper)}
      ) %>% unlist(),
      ### ARE THEY ALWAYS INCHES? No measure type recorded, though they may use decifeet sometimes
      HorizonDepthLower = suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
      HorizonDepthUpper = suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54
    )

  return(horizons_lmf)
}
#' export gather_soil_horizon_survey123
#' rdname gather_soil_horizon
# gather_soil_horizon_survey123 <- function(dsn = NULL,
#                                           PlotChar_0 = NULL,
#                                           SoilPitHorizons_1 = NULL){
#
#   # INPUT DATA, prefer tables if provided. If one or more are missing, load from dsn
#   if (!is.null(SoilPitHorizons_1) & !is.null(PlotChar_0)) {
#     hz_raw <- SoilPitHorizons_1
#     plotchar_raw <- PlotChar_0
#   } else if(!is.null(dsn)){
#     if(!file.exists(dsn)){
#       stop("dsn must be a valid filepath to a geodatabase containing tblSoilPitHorizons")
#     }
#
#     hz_raw <- suppressWarnings(sf::st_read(dsn = dsn, layer = "tblSoilPitHorizons",
#                                                stringsAsFactors = FALSE, quiet = T))
#   } else {
#     stop("Supply either tblSoilPitHorizons, or the path to a GDB containing those tables")
#   }
#
#   # Survey123 data uses PlotKey instead of PrimaryKey
#   hz_raw <- dplyr::left_join(hz_raw, plotchar_raw %>% dplyr::select(PrimaryKey = PlotKey, GlobalID), by = c("ParentGlobalID" = "GlobalID"))
#
#   # Check for duplicate PrimaryKeys
#   dupkeys <- hz_raw$PrimaryKey[duplicated(hz_raw$PrimaryKey)]
#   if(length(dupkeys) > 0){
#     dupnames <- paste(unique(dupkeys), collapse = ", ")
#     warning(paste("Duplicate PrimaryKeys found. Change PlotKey in the original data:", dupnames))
#   }
#
#   horizons <- hz_raw %>%
#     ### select ###
#     dplyr::select(
#       PrimaryKey, DBKey, HorizonKey, HorizonDepthUpper, HorizonDepthLower,
#       DepthUOM = DepthMeasure, HorizonName = ESD_Horizon,
#       Texture, TextureModifier = ESD_HorizonModifier,
#       pH = ESD_pH, EC = ESD_EC, Effervescence = Effer,
#       ClayPct = ESD_PctClay, SandPct = ESD_PctSand,
#
#       StructureGrade = ESD_Grade, StructureSize = ESD_Size, StructureType = ESD_Structure,
#       # StructureGrade2 = ESD_Grade2, StructureSize2 = ESD_Size2, StructureType2 = ESD_Structure2,
#       StructureQuality = ESD_StructQual,
#
#       # PetrocalcicRubble = ESD_PetrocalcicRubble, Gypsic = ESD_Gypsic,
#       # ClayFilm = ESD_ClayFilm,
#       Hue = ESD_Hue, Value = ESD_Value, Chroma = ESD_Chroma, ColorMoistDry = ESD_Color,
#       # RootSize = ESD_RootSize, RootQty = ESD_RootQty,
#
#       Fragment1VolPct = ESD_FragVolPct,  Fragment1Type = ESD_FragmentType,
#       Fragment2VolPct = ESD_FragVolPct2, Fragment2Type = ESD_FragmentType2,
#       Fragment3VolPct = ESD_FragVolPct3, Fragment3Type = ESD_FragmentType3,
#
#       HorizonNotes = ESD_Notes
#
#       ### cleaning ###
#     ) %>%
#     dplyr::mutate_all(
#       stringr::str_trim # defensive, early qc seems to catch this well
#     ) %>%
#     ### recode class data###
#     dplyr::mutate(
#       StructureGrade = dplyr::recode(StructureGrade,
#                                      "0" = "Structureless",
#                                      "1" = "Weak",
#                                      "2" = "Moderate",
#                                      "3" = "Strong"),
#       # StructureGrade2 = dplyr::recode(StructureGrade2,
#       #                          "0" = "Structureless",
#       #                          "1" = "Weak",
#       #                          "2" = "Moderate",
#       #                          "3" = "Strong"),
#       StructureSize = dplyr::recode(StructureSize %>% tolower(),
#                                     "vf" = "Very fine",
#                                     "vn" = "Very thin",
#                                     "f"  = "Fine",
#                                     "tn" = "Thin",
#                                     "m"  = "Medium",
#                                     "co" = "Coarse",
#                                     "tk" = "Thick",
#                                     "vc" = "Very coarse",
#                                     "vk" = "Very thick",
#                                     "ec" = "Extremely coarse"),
#       # StructureSize2 = dplyr::recode(StructureSize2 %>% tolower(),
#       #                         "vf" = "Very fine",
#       #                         "vn" = "Very thin",
#       #                         "f"  = "Fine",
#       #                         "tn" = "Thin",
#       #                         "m"  = "Medium",
#       #                         "co" = "Coarse",
#       #                         "tk" = "Thick",
#       #                         "vc" = "Very coarse",
#       #                         "vk" = "Very thick",
#       #                         "ec" = "Extremely coarse"),
#       StructureType = dplyr::recode(StructureType %>% tolower(),
#                                     "gr"  = "Granular",
#                                     "abk" = "Angular blocky",
#                                     "sbk" = "Subangular blocky",
#                                     "pl"  = "Platy",
#                                     "weg" = "Wedge",
#                                     "pr"  = "Prismatic",
#                                     "col" = "Columnar",
#                                     "sg"  = "Single grain",
#                                     "ma"  = "Massive",
#                                     "cdy" = "Cloddy",
#                                     "other" = "Other"),
#       # StructureType2 = dplyr::recode(StructureType2 %>% tolower(),
#       #                         "gr"  = "Granular",
#       #                         "abk" = "Angular blocky",
#       #                         "sbk" = "Subangular blocky",
#       #                         "pl"  = "Platy",
#       #                         "weg" = "Wedge",
#       #                         "pr"  = "Prismatic",
#       #                         "col" = "Columnar",
#       #                         "sg"  = "Single grain",
#       #                         "ma"  = "Massive",
#       #                         "cdy" = "Cloddy",
#       #                         "other" = "Other"),
#     ) %>%
#     ### complex mutates that depend on >1 var ###
#     dplyr::mutate(
#       SiltPct = 100 - (as.numeric(SandPct) + as.numeric(ClayPct)),
#       FragVolGravel = dplyr::case_when(
#         Fragment1Type %in% c("GR", "Gravel", "1") ~ Fragment1VolPct,
#         Fragment2Type %in% c("GR", "Gravel", "1") ~ Fragment2VolPct,
#         Fragment3Type %in% c("GR", "Gravel", "1") ~ Fragment3VolPct
#       ),
#       FragVolCobble = dplyr::case_when(
#         Fragment1Type %in% c("CB", "Cobble", "2") ~ Fragment1VolPct,
#         Fragment2Type %in% c("CB", "Cobble", "2") ~ Fragment2VolPct,
#         Fragment3Type %in% c("CB", "Cobble", "2") ~ Fragment3VolPct
#       ),
#       FragVolStone = dplyr::case_when(
#         Fragment1Type %in% c("ST", "Stone", "6") ~ Fragment1VolPct,
#         Fragment2Type %in% c("ST", "Stone", "6") ~ Fragment2VolPct,
#         Fragment3Type %in% c("ST", "Stone", "6") ~ Fragment3VolPct
#       ),
#       FragVolNodule = dplyr::case_when(
#         Fragment1Type %in% c("8", "Nodule") ~ Fragment1VolPct,
#         Fragment2Type %in% c("8", "Nodule") ~ Fragment2VolPct,
#         Fragment3Type %in% c("8", "Nodule") ~ Fragment3VolPct
#       ),
#       FragVolDurinode = dplyr::case_when(
#         Fragment1Type %in% c("9", "Durinode") ~ Fragment1VolPct,
#         Fragment2Type %in% c("9", "Durinode") ~ Fragment2VolPct,
#         Fragment3Type %in% c("9", "Durinode") ~ Fragment3VolPct
#       ),
#       HorizonDepthLower = dplyr::case_when(
#         DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthLower)) * 2.54,
#         DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthLower))),
#       HorizonDepthUpper = dplyr::case_when(
#         DepthUOM == "in" ~ suppressWarnings(as.numeric(HorizonDepthUpper)) * 2.54,
#         DepthUOM == "cm" ~ suppressWarnings(as.numeric(HorizonDepthUpper))),
#       DepthUOM = "cm"
#     ) %>%
#     ### drop vars that are no longer relevant ###
#     dplyr::select(
#       -Fragment1Type,
#       -Fragment2Type,
#       -Fragment3Type,
#       -Fragment1VolPct,
#       -Fragment2VolPct,
#       -Fragment3VolPct,
#     )   %>%
#     dplyr::arrange(PrimaryKey, HorizonDepthUpper) %>%
#     dplyr::group_by( # group to add horizon number columnm. if this reduces nrows, theres a mistake
#       PrimaryKey
#     ) %>%
#
#     dplyr::mutate(HorizonNumber = as.character(dplyr::row_number()),
#                   # across(c(RockFragments), ~ suppressWarnings(as.integer(.x))),
#                   across(c(pH,
#                            EC, ClayPct, SandPct, SiltPct, # poreqty,
#                            FragVolGravel, FragVolCobble, FragVolStone, FragVolNodule,
#                            FragVolDurinode, HorizonDepthUpper, HorizonDepthLower,
#                   ), ~ suppressWarnings(as.double(.x))),
#                   # across(c(ClayFilm, PetrocalcicRubble, Gypsic), ~ suppressWarnings(as.logical(as.integer(.x))))
#     )
#   horizons <- as.data.frame(horizons)
#
#   return(horizons)
# }


#' @export gather_soil_horizon
#' @rdname gather_soil_horizon
gather_soil_horizon <- function(dsn = NULL,
                                source,
                                SOILHORIZON = NULL,
                                tblSoilPitHorizons = NULL,
                                autoQC = TRUE) {

  if(toupper(source) %in% c("AIM", "TERRADAT")) {
    soil <- gather_soil_horizon_terradat(dsn = dsn, tblSoilPitHorizons = tblSoilPitHorizons)
  } else if(toupper(source) %in% c("LMF", "NRI")){
    soil <- gather_soil_horizon_lmf(dsn = dsn, SOILHORIZON = SOILHORIZON)
  } else {
    stop("source must be AIM, TerraDat, DIMA, LMF, or NRI (all case independent)")
  }

  soil$source <- source

  if("sf" %in% class(soil)) soil <- sf::st_drop_geometry(soil)

  if (any(class(soil) %in% c("POSIXct", "POSIXt"))) {
    change_vars <- names(soil)[do.call(rbind, vapply(soil,
                                                     class))[, 1] %in% c("POSIXct", "POSIXt")]
    soil <- dplyr::mutate_at(soil, dplyr::vars(change_vars),
                             dplyr::funs(as.character))
  }

  # change from tibble to data frame
  soil <- as.data.frame(soil) %>%

    # reorder so that primary key is leftmost column
    dplyr::select(PrimaryKey, DBKey, tidyselect::everything())

  # remove duplicates and empty rows
  if(autoQC){
    message("Removing duplicated rows and rows with no essential data. Disable by adding the parameter 'autoQC = FALSE'")
    soil <- soil %>% tdact_remove_duplicates() %>% tdact_remove_empty(datatype = "soilhz")
  }

  return(soil)
}

#### GATHER ALL ################################################################
#' Gather tall tables for gap, vegetation height, LPI, plot characterization,
#' IIRH, soil horizon, soil pit summary, soil stability, and species inventory.
#'
#' @description Given wide format AIM/LMF data, gather gap,
#' vegetation height, LPI, header, IIRH, soil horizon,
#' soil pit summary, soil stability, and species inventory data. Missing
#' tables will be skipped. AIM-type and LMF-type data will both be processed.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing thes table of interest.
#' This field is unnecessary if you provide dflist.
#' @param dflist Named list of data frames containing monitoring data. Tables
#' must be named as expected by the individual gather_functions.
#' @param outfolder Character string. Name of a folder to save all output to.
#' If the specified folder does not exist, the function will create it.
#' @param outtype Vector specifying output format, accepting "csv" and "rdata".
#' Defaults to writing both.
#' @param verbose True/False. When true, displays progress information, and
#' reports missing input data.
#' @param doLPI True/False. When false, LPI data will not be gathered. LPI data
#' is large and the gather process is RAM-intensive. This function will function
#' with fewer resources if LPI is run in batches, external to this wrapper.
#' @importFrom magrittr %>%
#' @name gather_all
#' @family <gather>
#' @return A list of tall data frames containing reformatted input data.
#' @examples
#' gather_all(dsn = "Path/To/AIM-LMF_Geodatabase.gdb", outfolder = "output")
#'
#' names <- sf::st_layers(dsn = "Path/To/AIM-LMF_Geodatabase.gdb")$name
#' all_data <- sapply(names, function(n){## Gather Height Data
#'   sf::st_read(dsn = "Path/To/AIM-LMF_Geodatabase.gdb",
#'   layer = n, quiet = T)
#' })
#' gather_all(dflist = all_data, outfolder = "output")


## Gather All Data
#' @export gather_all
#' @rdname gather_all

gather_all <- function(dsn = NULL,
                       dflist = NULL,
                       outfolder,
                       outtype = c("csv", "rdata"),
                       verbose = TRUE,
                       doLPI = TRUE) {
  # prep ####
  outtype <- tolower(outtype)

  if(substr(outfolder, nchar(outfolder), nchar(outfolder)) != "/") {
    outfolder <- paste0(outfolder, "/")
  }

  if(!dir.exists(outfolder)) dir.create(outfolder)

  # if neither dsn or dflist are provided, stop
  if(is.null(dflist) & is.null(dsn)) stop("Provide either dsn or dflist")

  # if both dsn and dflist are provided, drop dsn
  if(!is.null(dflist) & !is.null(dsn)){
    dsn <- NULL
    if(verbose) print("Both dsn and dflist were provided. Dsn will be ignored")
  }

  # extract names, check against these before trying to load data
  if(is.null(dflist)){
    names_rda <- NULL
  } else {
    names_rda <- names(dflist)
  }
  if(is.null(dsn)){
    names_gdb <-NULL
  } else {
    names_gdb <- sf::st_layers(dsn) %>% unlist()
  }

  # pull tables out of dflist if supplied, so the lack of NULL inputs dont mess up the functions
  # if dflist is NULL, all of these should be NULL
  tblGapDetail <- dflist[["tblGapDetail"]]
  tblGapHeader <- dflist[["tblGapHeader"]]
  tblLPIDetail <- dflist[["tblLPIDetail"]]
  tblLPIHeader <- dflist[["tblLPIHeader"]]
  tblSoilStabDetail <- dflist[["tblSoilStabDetail"]]
  tblSoilStabHeader <- dflist[["tblSoilStabHeader"]]
  tblQualDetail <- dflist[["tblQualDetail"]]
  tblQualHeader <- dflist[["tblQualHeader"]]
  tblSoilPitHorizons <- dflist[["tblSoilPitHorizons"]]
  tblSoilPits <- dflist[["tblSoilPits"]]
  tblSpecRichDetail <- dflist[["tblSpecRichDetail"]]
  tblSpecRichHeader <- dflist[["tblSpecRichHeader"]]
  tblPlots <- dflist[["tblPlots"]]

  GINTERCEPT <- dflist[["GINTERCEPT"]]
  POINT <- dflist[["POINT"]]
  PASTUREHEIGHTS <- dflist[["PASTUREHEIGHTS"]]
  RANGEHEALTH <- dflist[["RANGEHEALTH"]]
  PINTERCEPT <- dflist[["PINTERCEPT"]]
  SOILDISAG <- dflist[["SOILDISAG"]]
  PLANTCENSUS <- dflist[["PLANTCENSUS"]]
  SOILHORIZON <- dflist[["SOILHORIZON"]]
  POINTCOORDINATES <- dflist[["POINTCOORDINATES"]]
  GPS <- dflist[["GPS"]]

  rm(dflist)

  # Gap ####
  if(("tblGapDetail" %in% names_rda & "tblGapHeader" %in% names_rda) |
     ("tblGapDetail" %in% names_gdb & "tblGapHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM gap")
    gap_aim <- gather_gap(dsn = dsn, source = "AIM",
                          tblGapDetail = tblGapDetail,
                          tblGapHeader = tblGapHeader)
  } else {
    gap_aim <- NULL
    if(verbose) print("tblGapDetail and/or tblGapHeader not found. Skipping AIM Gap.")
  }

  if(("GINTERCEPT" %in% names_rda & "POINT" %in% names_rda) |
     ("GINTERCEPT" %in% names_gdb & "POINT" %in% names_gdb)){
    if(verbose) print("Gathering LMF gap")
    gap_lmf <- gather_gap(dsn = dsn, source = "LMF",
                          GINTERCEPT = GINTERCEPT,
                          POINT = POINT)
  } else {
    gap_lmf <- NULL
    if(verbose) print("GINTERCEPT and/or POINT not found. Skipping LMF Gap.")
  }

  gap_tall <- dplyr::bind_rows(gap_aim, gap_lmf)
  if(1 <= nrow(gap_tall)){
    if("csv" %in% outtype){
      write.csv(gap_tall,
                file = paste(outfolder, "gap_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(gap_tall,
              file = paste0(outfolder, "gap_tall.rdata"))
    }

  }
  rm(gap_aim, gap_lmf)
  invisible(gc())

  # Soil stability ####
  if(("tblSoilStabDetail" %in% names_rda & "tblSoilStabHeader" %in% names_rda) |
     ("tblSoilStabDetail" %in% names_gdb & "tblSoilStabHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM soil stability")
    soilstab_aim <- gather_soil_stability(dsn = dsn, source = "AIM",
                                          tblSoilStabDetail = tblSoilStabDetail,
                                          tblSoilStabHeader = tblSoilStabHeader)
  } else {
    soilstab_aim <- NULL
    if(verbose) print("tblSoilStabDetail and/or tblSoilStabHeader not found. Skipping AIM Soil Stability.")
  }

  if(("SOILDISAG" %in% names_rda) |
     ("SOILDISAG" %in% names_gdb)){
    if(verbose) print("Gathering LMF soil stability")
    soilstab_lmf <- gather_soil_stability(dsn = dsn, source = "LMF",
                                          SOILDISAG = SOILDISAG)
  } else {
    soilstab_lmf <- NULL
    if(verbose) print("SOILDISAG not found. Skipping LMF Soil Stability.")
  }

  soilstab_tall <- dplyr::bind_rows(soilstab_aim, soilstab_lmf)
  if(1 <= nrow(soilstab_tall)){

    if("csv" %in% outtype){
      write.csv(soilstab_tall,
                file = paste(outfolder, "soil_stability_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(soilstab_tall,
              file = paste0(outfolder, "soil_stability_tall.rdata"))
    }
  }
  rm(soilstab_aim, soilstab_lmf)
  invisible(gc())

  # LPI ####
  if(doLPI == T){
    if(("tblLPIDetail" %in% names_rda & "tblLPIHeader" %in% names_rda) |
       ("tblLPIDetail" %in% names_gdb & "tblLPIHeader" %in% names_gdb)){
      if(verbose) print("Gathering AIM LPI")
      lpi_aim <- gather_lpi(dsn = dsn, file_type = "gdb", source = "AIM",
                            tblLPIDetail = tblLPIDetail, tblLPIHeader = tblLPIHeader)} else {
                              lpi_aim <- NULL
                              if(verbose) print("tblLPIDetail and/or tblLPIHeader not found. Skipping AIM LPI.")
                            }

    if(("PINTERCEPT" %in% names_rda) |
       ("PINTERCEPT" %in% names_gdb)){
      if(verbose) print("Gathering LMF LPI")
      lpi_lmf <- gather_lpi(dsn = dsn, file_type = "gdb", source = "LMF",
                            PINTERCEPT = PINTERCEPT)
    } else {
      lpi_lmf <- NULL
      if(verbose) print("PINTERCEPT not found. Skipping LMF LPI.")
    }

    lpi_tall <- dplyr::bind_rows(lpi_aim, lpi_lmf)
    if(1 <= nrow(lpi_tall)){
      if("csv" %in% outtype){
        write.csv(lpi_tall,
                  file = paste(outfolder, "lpi_tall.csv", sep = ""), row.names = F)
      }
      if("rdata" %in% outtype){
        saveRDS(lpi_tall,
                file = paste0(outfolder, "lpi_tall.rdata"))
      }
    }
    rm(lpi_aim, lpi_lmf)
    invisible(gc())

    # Height ####
    if(("tblLPIDetail" %in% names_rda & "tblLPIHeader" %in% names_rda) |
       ("tblLPIDetail" %in% names_gdb & "tblLPIHeader" %in% names_gdb)){
      if(verbose) print("Gathering AIM Height")
      height_aim <- gather_height(dsn = dsn, file_type = "gdb", source = "AIM",
                                  tblLPIDetail = tblLPIDetail, tblLPIHeader = tblLPIHeader)
    } else {
      height_aim <- NULL
      if(verbose) print("tblLPIDetail and/or tblLPIHeader not found. Skipping AIM Height.")
    }

    if(("PASTUREHEIGHTS" %in% names_rda) |
       ("PASTUREHEIGHTS" %in% names_gdb)){
      if(verbose) print("Gathering LMF Height")
      height_lmf <- gather_height(dsn = dsn, file_type = "gdb", source = "LMF",
                                  PASTUREHEIGHTS = PASTUREHEIGHTS)
    } else {
      height_lmf <- NULL
      if(verbose) print("PASTUREHEIGHTS not found. Skipping LMF Height.")
    }

    height_tall <- dplyr::bind_rows(height_aim, height_lmf)
    if(1 <= nrow(height_tall)){
      if("csv" %in% outtype){
        write.csv(height_tall,
                  file = paste(outfolder, "height_tall.csv", sep = ""), row.names = F)
      }
      if("rdata" %in% outtype){
        saveRDS(height_tall,
                file = paste0(outfolder, "height_tall.rdata"))
      }
    }
    rm(height_lmf, height_aim)
    invisible(gc())




  } else {
    print("doLPI is false, skipping all lpi")
  }

  ##### Species inventory ####
  if(("tblSpecRichDetail" %in% names_rda & "tblSpecRichHeader" %in% names_rda) |
     ("tblSpecRichDetail" %in% names_gdb & "tblSpecRichHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM species inventory")
    spp_inventory_aim <- gather_species_inventory(dsn = dsn, source = "AIM",
                                                  tblSpecRichDetail = tblSpecRichDetail,
                                                  tblSpecRichHeader = tblSpecRichHeader)

  } else {
    spp_inventory_aim <- NULL
    if(verbose) print("tblSpecRichDetail and/or tblSpecRichHeader not found. Skipping AIM Species Inventory.")
  }
  if(("PLANTCENSUS" %in% names_rda) |
     ("PLANTCENSUS" %in% names_gdb)){
    if(verbose) print("Gathering LMF species inventory")
    spp_inventory_lmf <- gather_species_inventory(dsn = dsn, source = "LMF",
                                                  PLANTCENSUS = PLANTCENSUS,
                                                  file_type = "gdb")
  } else {
    spp_inventory_lmf <- NULL
    if(verbose) print("PLANTCENSUS not found. Skipping LMF Species Inventory.")
  }

  spp_inventory_tall <- dplyr::bind_rows(spp_inventory_aim, spp_inventory_lmf)
  if(1 <= nrow(spp_inventory_tall)){

    if("csv" %in% outtype){
      write.csv(spp_inventory_tall,
                file = paste(outfolder, "species_inventory_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(spp_inventory_tall,
              file = paste0(outfolder, "spp_inventory_tall.rdata"))
    }
  }
  rm(spp_inventory_aim, spp_inventory_lmf)
  invisible(gc())

  # soil horizons ####
  if(("tblSoilPitHorizons" %in% names_rda) |
     ("tblSoilPitHorizons" %in% names_gdb)){
    if(verbose) print("Gathering AIM soil horizon data")
    hz_aim <- gather_soil_horizon(dsn = dsn, source = "AIM",
                                  tblSoilPitHorizons = tblSoilPitHorizons)
  } else {
    hz_aim <- NULL
    if(verbose) print("tblSoilPitHorizons not found. Skipping AIM Horizons.")
  }
  if(("SOILHORIZON" %in% names_rda) |
     ("SOILHORIZON" %in% names_gdb)){
    if(verbose) print("Gathering LMF soil horizon data")
    hz_lmf <- gather_soil_horizon(dsn = dsn, source = "LMF", SOILHORIZON = SOILHORIZON)
  } else {
    hz_lmf <- NULL
    if(verbose) print("SOILHORIZON not found. Skipping LMF Horizons.")
  }
  hz_tall <- dplyr::bind_rows(hz_aim, hz_lmf)
  if(1 <= nrow(hz_tall)){

    if("csv" %in% outtype){
      write.csv(hz_tall,
                file = paste(outfolder, "soil_horizons_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(hz_tall,
              file = paste0(outfolder, "soil_horizons_tall.rdata"))
    }

  }
  rm(hz_aim, hz_lmf)
  invisible(gc())

  # soil summary ####
  if(("tblSoilPitHorizons" %in% names_rda & "tblSoilPits" %in% names_rda) |
     ("tblSoilPitHorizons" %in% names_gdb & "tblSoilPits" %in% names_gdb)){
    if(verbose) print("Gathering AIM soil summary")
    pit_aim <- gather_soil_summary(dsn = dsn, source = "AIM",
                                   tblSoilPitHorizons = tblSoilPitHorizons,
                                   tblSoilPits = tblSoilPits)
  } else {
    pit_aim <- NULL
    if(verbose) print("tblSoilPitHorizons and/or tblSoilPits not found. Skipping AIM Soil Summary.")
  }
  if(("SOILHORIZON" %in% names_rda) |
     ("SOILHORIZON" %in% names_gdb)){
    if(verbose) print("Gathering LMF soil summary")
    pit_lmf <- gather_soil_summary(dsn = dsn, source = "LMF", SOILHORIZON = SOILHORIZON)
  } else {
    pit_lmf <- NULL
    if(verbose) print("SOILHORIZON not found. Skipping LMF soil Summary.")
  }
  pit_tall <- dplyr::bind_rows(pit_aim, pit_lmf)
  if(1 <= nrow(pit_tall)){
    if("csv" %in% outtype){
      write.csv(pit_tall,
                file = paste(outfolder, "pit_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(pit_tall,
              file = paste0(outfolder, "pit_tall.rdata"))
    }
  }

  rm(pit_aim, pit_lmf)
  invisible(gc())

  # iirh ####
  if(("tblQualDetail" %in% names_rda & "tblQualHeader" %in% names_rda) |
     ("tblQualDetail" %in% names_gdb & "tblQualHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM IIRH data")
    iirh_aim <- gather_rangeland_health(dsn = dsn, source = "AIM",
                                        tblQualDetail = tblQualDetail,
                                        tblQualHeader = tblQualHeader)
  } else {
    iirh_aim <- NULL
    if(verbose) print("tblQualDetail and/or tblQualHeader not found. Skipping AIM Rangeland Health.")
  }
  if(("RANGEHEALTH" %in% names_rda) |
     ("RANGEHEALTH" %in% names_gdb)){
    if(verbose) print("Gathering LMF IIRH data")
    iirh_lmf <- gather_rangeland_health(dsn = dsn, source = "LMF",
                                        RANGEHEALTH = RANGEHEALTH)

  } else {
    iirh_lmf <- NULL
    if(verbose) print("RANGEHEALTH not found. Skipping LMF Rangeland Health.")
  }
  iirh_tall <- dplyr::bind_rows(iirh_aim, iirh_lmf)
  if(1 <= nrow(iirh_tall)){
    if("csv" %in% outtype){
      write.csv(iirh_tall,
                file = paste(outfolder, "rangeland_health_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(iirh_tall,
              file = paste0(outfolder, "rangeland_health_tall.rdata"))
    }  }
  rm(iirh_aim, iirh_lmf)
  invisible(gc())

  # header ####
  if(("tblPlots" %in% names_rda & "tblLPIHeader" %in% names_rda) |
     ("tblPlots" %in% names_gdb & "tblLPIHeader" %in% names_rda)){
    if(verbose) print("Gathering AIM Header")
    header_aim <- gather_header(dsn = dsn, source = "AIM",
                                tblPlots = tblPlots,
                                tblLPIHeader = tblLPIHeader)

    #
    #     if(verbose) print("Gathering AIM plot characterization")
    #     plotchar_aim <- gather_plot_characterization(dsn = dsn,
    #                                                  source = "AIM",
    #                                                  tblPlots = tblPlots)

  } else {
    header_aim <- NULL

    if(verbose) print("tblPlots not found. Skipping AIM header.")
  }
  if(("POINT" %in% names_rda) |
     ("POINT" %in% names_gdb)){
    if(verbose) print("Gathering LMF header")
    header_lmf <- gather_header(dsn = dsn,
                                source = "LMF")
  } else {
    header_lmf <- NULL
    if(verbose) print("POINT not found. Skipping LMF header.")
  }

  header_tall <- dplyr::bind_rows(header_aim, header_lmf)
  if(1 <= nrow(header_tall)){
    if("csv" %in% outtype){
      write.csv(header_tall,
                file = paste(outfolder, "header.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(header_tall,
              file = paste0(outfolder, "header.rdata"))
    }  }
  rm(header_aim, header_lmf)
  invisible(gc())

  # # output ####
  # if(doLPI == T){
  #
  # list_out <- list(
  #   gap_tall, height_tall, hz_tall, lpi_tall, pit_tall, header_tall,
  #   soilstab_tall, spp_inventory_tall
  # )
  #
  # names(list_out) <- c("Gap", "VegHeight", "SoilHorizons", "LPI", "SoilPitSummary",
  #                      "Header",
  #                      "SoilStability", "SpeciesInventory")
  #
  # } else {
  #   list_out <- list(
  #     gap_tall, hz_tall, #pit_tall,
  #     header_tall,
  #     soilstab_tall, spp_inventory_tall
  #   )
  #
  #   names(list_out) <- c("Gap", "SoilHorizons", "SoilPitSummary",
  #                        "Header", "SoilStability", "SpeciesInventory")
  # }

  # return(list_out)
}

