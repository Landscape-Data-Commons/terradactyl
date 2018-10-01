
gather.coordinates <- function(dsn) {

  ## Read plot data in
  # Try the TerrADat format
  try(plots <- sf::st_read(
    dsn = dsn,
    layer = "tblPlots",
    quiet = TRUE
  ),
  silent = TRUE
  )
  # Try the LMF Format
  if (!exists("plots")) {
    try(plots <- sf::st_read(
      dsn = dsn,
      layer = "POINTCOORDINATES"
    ),
    silent = TRUE
    )
  }
  # Try the NRI format
  if (!exists("plots")) {
    try(plots <- read.csv(paste(dsn, "pointcoordinates.txt"), sep = "|"),
      silent = TRUE
    )
  }
  # If no valid plot file provided, send error
  if (!exists("plots")) {
    stop("No valid plot coordinate filepath provided")
  }

  ## Clean Up LMF and NRI data

  if (!("PrimaryKey" %in% colnames(plots))) {
    # Build PrimaryKey
    plots <- terradactyl::build.PK(plots)
    # Correct Longitude
    plots$FIELD_LONGITUDE <- plots$FIELD_LONGITUDE * -1
    # Select and Rename relevant fields
    plots <- dplyr::select(plots,
      PrimaryKey,
      Latitude = FIELD_LATITUDE,
      Longitude = FIELD_LONGITUDE
    )
  } else {
    # extract just PrimaryKey and coordinates
    plots <- dplyr::select(plots, PrimaryKey, Latitude, Longitude)
  }

  ## Make the layer spatial
  plots.sp <- sf::st_as_sf(plots,
    coords = c("Longitude", "Latitude"),
    crs = ("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  )

  return(plots.sp)
}

#' Create valid PrimaryKey values
#' @description The PrimaryKey values in TerrADat are strings created by pasting/concatenating several other values in the \code{tblPlots} table.
#' @param plots Data frame. The contents of \code{tblPlots} in TerrADat or an equivalent with variables named "SURVEY", "STATE", "COUNTY", "PSU", and "POINT".
#' @return The data frame \code{plots} with a new variable "PrimaryKey" added containing the PrimaryKey value for each plot in the data frame.
#' @export

build.PK <- function(plots) {
  if (class(plots) != "data.frame") {
    stop("The plots argument must be a data frame")
  }
  required.vars <- c("SURVEY", "STATE", "COUNTY", "PSU", "POINT")
  missing.vars <- required.vars[!(required.vars %in% names(plots))]

  if (length(missing.vars) > 0) {
    stop(paste("The following variables are missing from the plots data frame:", paste(missing.vars, collapse = ", ")))
  }

  plots$PrimaryKey <- paste0(plots$SURVEY, plots$STATE, plots$COUNTY, plots$PSU, plots$POINT)
  return(plots)
}

#' Select plots using spatial extent
#' @description Subset plots using the PrimaryKey values. This can either be using the values that appear in the spatial data frame \code{extent} or in the spatial points data frame \code{plots} restricted to their intersection with \code{extent}.
#' @param gathered.data Data frame. The plot data containing at minimum a variable called "PrimaryKey" containing values matching those in \code{extent} or \code{plots}.
#' @param extent Spatial points/polygons data frame. Spatial restriction for the results. If \code{plots} is not provided, then this must contain a variable called "PrimaryKey" matching the values in \code{gathered.data}.
#' @param plots Optional spatial points data frame. Must have a variable called "PrimaryKey" containing values matching those in \code{gathered.data}. Will be restricted to the intersection with \code{extent}.
#' @export plot.query

plot.query <- function(gathered.data,
                       extent,
                       plots = NULL) {
  if (class(gathered.data) != "data.frame") {
    stop("gathered.data must be a data frame")
  }
  if (!("PrimaryKey" %in% names(gathered.data))) {
    stop("gathered.data must contain the variable 'PrimaryKey'")
  }

  if (!grepl(class(extent)[1], pattern = "^Spatial.+DataFrame$")) {
    stop("extent must be a spatial data frame.")
  }
  if (!("PrimaryKey" %in% names(extent@data))) {
    stop("extent must contain the variable 'PrimaryKey'")
  }

  # Treat the query differently if it is spatial or aspatial
  if (!is.null(plots)) {
    if (class(plots)[1] != "SpatialPointsDataFrame") {
      stop("Plots must be a spatial points data frame.")
    }
    if (!("PrimaryKey" %in% names(plots@data))) {
      stop("plots must contain the variable 'PrimaryKey'")
    }

    # Get everything on same projection
    extent <- sf::st_transform(extent, crs = sf::st_crs(plots))
    # Subset all plots by extent
    plot.subset <- sf::st_intersection(plots, extent)
    # Subset gathered data by plots subset
    data.subset <- subset(
      gathered.data,
      PrimaryKey %in% plot.subset$PrimaryKey
    )
  } else {
    if (!is.null(plots) & is.null(extent)) {
      message("No extent provided. Treating query as aspatial.")
    }
    if (is.null(plots) & !is.null(extent)) {
      message("No plots provided. Treating query as aspatial.")
    }
    # Aspatial subset
    data.subset <- subset(
      gathered.data,
      PrimaryKey %in% extent$PrimaryKey
    )
  }

  return(data.subset)
}
