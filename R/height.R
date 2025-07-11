#' Calculate the vegetation height
#' @param height_tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param omit_zero Logical. If \code{TRUE} the results omit height measurements of \code{0}. Defaults to \code{TRUE}.
#' @param method Character string. Indicates the type of indicator, \code{"max"}, which returns the mean of the maximum heights at each pin drop on the plot or \code{"mean"} which yields the mean height by functional group (woody/herbaceous).
#' @param by_line Logical. If \code{TRUE} then the results will be calculated on a per-line basis. If \code{FALSE} then the results will be calculated on a per-plot basis. Defaults to \code{FALSE}.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param ... Optional bare variable names. One or more variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @examples
#' # Gather height data into tall format
#' height_tall <- gather_height(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                     source = "LMF")
#' # Calculate woody and herbaceous height (specified in "type" field)
#' # All arguments must be named explicitly for this function to work correctly
#' height <- mean_height(height_tall,
#'                       method = "mean",
#'                       omit_zero = FALSE,
#'                       by_line = FALSE,
#'                       tall = FALSE,
#'                       type)
#'
#'
#' @export mean_height
mean_height <- function(height_tall,
                        method = "mean",
                        omit_zero = TRUE,
                        by_line = FALSE,
                        tall = FALSE,
                        ...,
                        indicator_variables = NULL,
                        missing_fill = 0,
                        digits = 1,
                        verbose = FALSE) {
  ##### Indicator variables -----------------------------------------------------
  # Get a list of the variables the user wants to group data by for calculations.
  # There's a grouping_variables argument that takes the names of variables as
  # character strings, so we'll handle that.
  if (!is.null(indicator_variables)) {
    if (!is.character(indicator_variables)) {
      stop("indicator_variables must be a character string or vector of character strings")
    }
  }
  # Clean this up!
  indicator_variables <- unique(indicator_variables)

  # This here because we're trying to support the legacy decision to originally
  # allow for bare variables as the indicator-defining variables.
  # Now it can be bare variable names, character strings, vectors of character
  # strings or some combination of the three.
  # BUT! You can't create a vector, store it in the environment, and then pass
  # it in by name because then you end up with just the name of the vector.
  indicator_variables <- c(indicator_variables,
                           rlang::quos(...) |>
                             as.character() |>
                             # This does the cleanup that removes the prefixed ~ from everything as well
                             # as any quotation marks or bits of the definition of a vector.
                             stringr::str_replace_all(string = _,
                                                      pattern = "(^~)|(\\\")|(c\\()|(\\)$)",
                                                      replacement = "") |>
                             stringr::str_split(string = _,
                                                pattern = ",[ ]*",
                                                simplify = TRUE) |>
                             as.vector()) |>
    unique()
  indicator_variables <- indicator_variables[!(indicator_variables %in% c(""))]

  if (!is.data.frame(height_tall)) {
    stop("height_tall must be a data frame.")
  }

  if (!(method %in% c("mean", "max"))) {
    stop("method must be either 'mean' or 'max'.")
  }

  # For grouping by line
  grouping_vars <- c("PrimaryKey")
  if (by_line) {
    grouping_vars <- c("PrimaryKey",
                       "LineKey")
  }

  # Make sure the Height field is numeric and filter out NAs
  height_tall <- dplyr::mutate(.data = height_tall,
                               Height = as.numeric(Height)) |>
    dplyr::filter(.data = _,
                  !is.na(Height))

  # If height of zero is dropped by the calculation, filter out zeros.
  # Note that this has been dropping only 0s where a species wasn't recorded!
  # If that's unintended, we should fix it.
  if (omit_zero) {
    height_tall <- dplyr::filter(.data = height_tall,
      # !(Height == 0 & Species %in% c("", "None", "N", NA))
      Height != 0)
  }

  # Calculate mean height by grouping variable, if method == "mean"
  if (method == "mean") {
    output <- dplyr::summarize(.data = height_tall,
                                .by = c(grouping_vars,
                                        indicator_variables),
                                mean_height = mean(Height) |>
                                 round(x = _,
                                       digits = digits))

    if (!is.null(indicator_variables)) {
      output <- tidyr::unite(data = output,
                               col = "indicator",
                               tidyselect::all_of(indicator_variables),
                               sep = ".")
    } else {
      output <- mutate(.data = output,
                        indicator = "mean_height")
    }

    output <- dplyr::select(.data = output,
                            tidyselect::any_of(x = c("PrimaryKey",
                                                     "LineKey")),
                            tidyselect::all_of(x = c("indicator",
                                                     "mean_height"))) |>
      # Remove invalid indicators, i.e., those for which not all indicator
      # variables were populated.
      dplyr::filter(.data = _,
                    !stringr::str_detect(string = indicator,
                                         pattern = "^NA\\.|\\.NA$|\\.NA\\."))

    # if (length(grouping_variables) > 0) {
    #   summary <- height_tall %>%
    #     dplyr::filter(!is.na(Height)) %>%
    #     dplyr::group_by(
    #       !!!level,
    #       !!!grouping_variables
    #     ) %>%
    #     dplyr::summarize(mean_height = mean(as.numeric(Height))) %>%
    #     tidyr::unite(indicator,
    #                  !!!grouping_variables,
    #                  sep = "."
    #     )
    # } else {
    #   summary <- height_tall %>%
    #     dplyr::filter(!is.na(Height)) %>%
    #     dplyr::group_by(!!!level) %>%
    #     dplyr::summarize(mean_height = mean(as.numeric(Height))) %>%
    #     dplyr::mutate(indicator = "mean_height")
    #
    #   # Reorder variables
    #   present_key_vars <- c("PrimaryKey", "LineKey")[c("PrimaryKey", "LineKey") %in% names(summary)]
    #   summary <- summary[, c(present_key_vars, "indicator", "mean_height")]
    # }
    #
    # summary <- summary[!grepl(summary$indicator, pattern = "^NA\\.|\\.NA$|\\.NA\\."), ]

    # quarantined by joe brehm 8/26, with all other rounding code. Need to make these optional parameters
    # summary$mean_height <- round(summary$mean_height, digits = 2)

    # Convert to wide format
    if (!tall) {
      # summary <- summary %>% tidyr::pivot_wider(names_from = indicator, values_from = mean_height, values_fill = missing_fill)
      output <- tidyr::pivot_wider(data = output,
                                   names_from = indicator,
                                   values_from = mean_height,
                                   values_fill = missing_fill)
    }
  }
  # Calculate the max height by grouping variable, if method =="max"
  # Note 2025-07-08: This looks like it's going to spit out some deeply weird
  # values and I wouldn't trust it at all, but I'm also leaving it as-is for
  # now and will try to revisit it while praying no one uses it in the meantime.
  # I think it was written assuming that this would always be done when Species
  # was included in indicator_variables????
  if (method == "max") {
    output <- dplyr::summarize(.data = height_tall,
                               .by = c("PrimaryKey",
                                       "LineKey",
                                       "PointNbr",
                                       indicator_variables),
                               max = max(Height)) |>
      dplyr::summarize(.data = _,
                       .by = c(grouping_vars,
                               indicator_variables),
                       max_height = mean(max) |>
                         round(x = _,
                               digits = digits)) |>
      dplyr::filter(.data = _,
                    !stringr::str_detect(string = max_height,
                                         pattern = "^NA$|\\.NA|NA\\.|\\.NA\\."))
    # summary <- height_tall %>%
    #   # dplyr::group_by(PrimaryKey, LineKey, PointNbr) %>%
    #   dplyr::group_by(PrimaryKey, LineKey, PointNbr, !!!grouping_variables) %>%
    #   dplyr::summarise(max = max(Height))
    #
    # summary <- summary %>%
    #   dplyr::group_by(!!!level, !!!grouping_variables) %>%
    #   dplyr::summarize(max_height = mean(max)) %>%
    #   dplyr::filter(!grepl(max_height, pattern = "^NA$|\\.NA|NA\\.|\\.NA\\."))

    # quarantined by joe brehm 8/26, with all other rounding code. Need to make these optional parameters
    # summary$max_height <- round(summary$max_height, digits = 2)

    # Convert to wide format
    if (!tall) {
      output <- tidyr::pivot_longer(data = output,
                                    names_from = Species,
                                    values_from = max_height,
                                    values_fill = missing_fill)
      # summary <- summary %>% tidyr::pivot_wider(names_from = Species, values_from = max_height, values_fill = missing_fill)
    }
  }

  output
}
