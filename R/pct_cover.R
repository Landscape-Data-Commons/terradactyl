#' Percent cover
#' @description Calculate the percent cover by plot for variables or combinations of variables. Percent cover will be calculated for every combination of the variables requested, so if the variables are \code{GrowthHabitSub} and \code{Duration} then the output will contain fields like \code{Graminoid.Perennial}, \code{Graminoid.Annual}, \code{Shrub.Perennial}, etc. whereas using just the variable \code{code} will produce one column per species code. Any number of indicator variables can be used. These are calculated as cover from anywhere in the canopy column or as only the first hit in the canopy column. Any groupings where all the variable values were \code{NA} will be dropped.
#' @param lpi_tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param hit Character string. If \code{"any"} then percent cover will be calculated using any hit in the canopy column (so a single pin drop record may be counted more than once if it had hits that corresponded to different groups). If \code{"first"} then only the first canopy hit at a pin drop will be used to calculate cover.  If \code{"basal"}, then only the soil surfacy hit will be used to calculate cover. Defaults to \code{"any"}.
#' @param by_line Logical. If \code{TRUE} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param ... Optional character strings. One or more variable name from \code{lpi_tall} to calculate percent cover for, e.g. \code{"GrowthHabitSub"} to calculate percent cover by growth habits or \code{"GrowthHabitSub", "Duration"} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @examples
#' # Gather header and LPI files
#' dsn = "Path/To/LMF_Geodatabase.gdb" # also contains species list
#' header <- gather_header(dsn = dsn,
#'                     source = "LMF")
#'
#' lpi_tall <- gather_lpi(dsn = "Path/To/LMF_Geodatabase.gdb",
#'                     source = "LMF")
#'
#' # Join lpi_tall and header to get the SpeciesState value associated with LPI data
#' lpi_tall <- dplyr::left_join(lpi_tall, header)
#'
#' # Join species list attributes
#' species_join(data = lpi_tall,species_file = dsn)
#'
#' # Calculate percent cover of individual species and cover values (S, R, etc) in lpi_tall
#'  pct_cover(lpi_tall = lpi_tall,
#'     tall = FALSE,
#'     hit = "any",
#'     by_line = FALSE,
#'     code)
#'
#'  Calculate percent cover of Duration and Growth Habit Sub (e.g., to produce Annual Forb cover)
#'  pct_cover(lpi_tall = lpi_tall,
#'     tall = FALSE,
#'     hit = "any",
#'     by_line = FALSE,
#'     Duration, GrowthHabitSub)
#' @export

pct_cover <- function(lpi_tall,
                      tall = FALSE,
                      hit = "any",
                      by_line = FALSE,
                      ...,
                      indicator_variables = NULL) {
  #### SETUP ###################################################################
  ##### Grouping variables -----------------------------------------------------
  # Get a list of the variables the user wants to group data by for calculations.
  # There's a grouping_variables argument that takes the names of variables as
  # character strings, so we'll handle that.
  if (!is.null(indicator_variables)) {
    if (!is.character(indicator_variables)) {
      stop("indicator_variables must be a character string or vector of character strings")
    }
  }
  # Renaming, basically just for clarity going forward.
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

  ##### Argument validity checks -----------------------------------------------
  if (!is.data.frame(lpi_tall)) {
    stop("lpi_tall must be a data frame.")
  }

  valid_hit_values <- c("any",
                        "first",
                        "basal")

  if (length(hit) > 1) {
    stop(paste("hit must be a single chatacter string. Valid values are 'any', 'first', or 'basal'."))
  }

  if (!(hit %in% valid_hit_values)) {
    stop(paste("The current value of hit is", hit, "which is not a valid value. Valid values are 'any', 'first', or 'basal'."))
  }

  # This will always calculate values per-plot via PrimaryKey if the user asks
  # for it to be by line within a plot this will add LineKey to the grouping
  # variables.
  internal_grouping_vars <- c("PrimaryKey")
  if (by_line) {
    internal_grouping_vars <- c(internal_grouping_vars,
                                "LineKey")
  }

  ##### Data cleanup -----------------------------------------------------------
  ###### Removing non-records --------------------------------------------------
  # Remove values that represent empty records. These shouldn't be in here at
  # this point if the data were cleaned, but better safe than sorry.
  # Should this happen after pin drop record counts to make sure that we don't
  # accidentally undercount if a pin drop had no qualifying records?
  non_codes <- c(NA,
                 "None",
                 "N",
                 "<NA>",
                 "")

  # Drop data where there is no code value, i.e., layers where there was no
  # recorded hit but the tall data includes them for some reason anyway.
  lpi_tall <- dplyr::filter(.data = lpi_tall,
                            !(code %in% non_codes))

  ###### Integrity check -------------------------------------------------------
  # The incoming data are assumed to have a SoilSurface record for every pin
  # drop on every plot. If that's not the case, we need to at the very least
  # warn the user!
  pin_drop_summary <- dplyr::summarize(.data = lpi_tall,
                                       .by = tidyselect::all_of(c("PrimaryKey",
                                                                  "LineKey",
                                                                  "PointNbr")),
                                       has_required_layers = all(c("SoilSurface") %in% layer))

  if (any(!pin_drop_summary$has_required_layers)) {
    warning(paste("There are", sum(!pin_drop_summary$has_required_layers), "pin drops that are missing the expected layer variable value 'SoilSurface'. This may cause issues with the validity of the calculations, particularly basal calculations."))
  }


  ###### Setup for basal hits --------------------------------------------------
  # If the user has asked for basal, we'll handle things the same as if they
  # asked for any hit indicators, but throw out all the records not associated
  # with SoilSurface layers.
  if (hit == "basal") {
    hit <- "any"
    lpi_tall <- dplyr::filter(.data = lpi_tall,
                              layer %in% c("SoilSurface"))
  }

  ###### Ordering layers -------------------------------------------------------
  # Our lives will be easier if we can just assume later that the order of the
  # records for a given pin drop is in order from the first layer down to the
  # surface. This makes sure that's the case.

  unique_layer_values <- unique(lpi_tall$layer)

  bad_layer_value_indices <- which(!stringr::str_detect(string = unique_layer_values,
                                                        pattern = "^TopCanopy|SoilSurface|(Lower\\d+)$"))

  if (length(bad_layer_value_indices) > 0) {
    warning(paste("The layer variable has unexpected values and the associated records will be dropped. The unexpected values are:",
                  paste(unique_layer_values[bad_layer_value_indices],
                        collapse = ", ")))
    lpi_tall <- dplyr::filter(.data = lpi_tall,
                              !(layer %in% unique_layer_values[bad_layer_value_indices]))
    unique_layer_values <- unique_layer_values[-bad_layer_value_indices]
  }

  # Find the lower layer numbers in this particular data set.
  lower_layer_numbers <- stringr::str_extract(string = unique_layer_values,
                                              pattern = "(?<=^Lower)\\d+$") |>
    unique() |>
    na.omit() |>
    as.numeric()

  # Now we can make the levels vector with the layer codes in the correct order.
  layer_levels <- c("TopCanopy",
                    # We only try to add lower codes here if we were able to
                    # find any above.
                    if (length(lower_layer_numbers) > 0) {
                      paste0("Lower",
                             lower_layer_numbers[order(lower_layer_numbers)])
                    },
                    "SoilSurface")

  # Now we apply the levels and use them to order the data!
  lpi_tall <- dplyr::mutate(.data = lpi_tall,
                            layer = factor(layer,
                                           levels = layer_levels)) |>
    dplyr::arrange(.data = _,
                   layer)

  ###### Capitalization? -------------------------------------------------------
  # This converts all grouping variables and the code variable to uppercase,
  # which seems wrong? Sure this should happen as data cleaning before it
  # reaches this point because case might matter for differentiating values?
  # lpi_tall <- dplyr::mutate(.data = lpi_tall,
  #                           code = toupper(code),
  #                           dplyr::across(.cols = tidyselect::all_of(user_grouping_variables),
  #                                         .fns = toupper))

  #### CALCULATING #############################################################
  ##### Pin drop counts --------------------------------------------------------
  # We need to know just how many pin drops actually occurred for each unique
  # grouping. Because these are LPI data, we assume that there'll be at least
  # two records for every pin drop (a TopCanopy and a SoilSurface) so we should
  # be safe to just count the number of unique PointNbr values.
  # We also assume that PointNbr values are only unique *within* a unique
  # PrimaryKey/LineKey combination.
  point_totals <- dplyr::select(.data = lpi_tall,
                                tidyselect::all_of(c("PrimaryKey",
                                                     "LineKey",
                                                     "PointNbr"))) |>
    dplyr::distinct(.data = _) |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(internal_grouping_vars),
                     point_count = dplyr::n())

  ##### Make an indicator variable ---------------------------------------------
  # Create the indicator names from the variables the user specified.
  lpi_tall <- tidyr::unite(data = lpi_tall,
                           col = "indicator",
                           tidyselect::all_of(indicator_variables),
                           sep = ".",
                           remove = FALSE,
                           na.rm = TRUE)

  # In the case that an indicator doesn't have a value for every indicator
  # variable, it shouldn't be kept in the output. This vector will be used to
  # screen those out *after* calculation so that first hit calculations aren't
  # affected.
  # This is important to hold off on actually using to filter!!!
  # If we screen these out before the calculation is done then there will be an
  # overestimation of first hit indicators where the first hit with a full set
  # of values in the indicator variables wasn't the first actual hit at the pin
  # drop because the upper hits would've been stripped away by a filtering step.
  keeping_indicators <- dplyr::select(.data = lpi_tall,
                                      tidyselect::all_of(indicator_variables),
                                      indicator) |>
    dplyr::distinct() |>
    dplyr::filter(.data = _,
                                      dplyr::if_all(.cols = tidyselect::all_of(indicator_variables),
                                                    .fns = ~ !is.na(.x))) |>
    dplyr::pull(.data = _,
                indicator)

  ##### Removing records without user_grouping_var values ----------------------
  # Just check all the user specific grouping variables and keep only records
  # where those variables are not NA.
  # There used to be some string manipulation later to account for NAs that were
  # then part of indicator names, but this ought to avoid needing that.
  # This *MUST* be done AFTER the pin drop counting because otherwise we won't
  # be able to be sure that we haven't missed counting entire pin drops for lack
  # of qualifying data.
  lpi_tall <- dplyr::filter(.data = lpi_tall,
                            dplyr::if_all(.cols = tidyselect::all_of(indicator_variables),
                                          .fns = ~ !is.na(.x)))

  ##### Summarize data ---------------------------------------------------------
  # The first summarization is just to figure out the first layer that each
  # indicator appears in (if at all) at a pin drop.
  summary <- dplyr::summarize(.data = lpi_tall,
                              .by = tidyselect::all_of(c("PrimaryKey",
                                                         "LineKey",
                                                         "PointNbr",
                                                         "indicator")),
                              first_occurrence_layer = dplyr::first(layer)) |>
    dplyr::arrange(.data = _,
                   first_occurrence_layer)

  # In case the user has asked for first hits only, we'll go ahead and further
  # summarize to get only the first hit record for each pin drop.
  if (hit == "first") {
    summary <- dplyr::summarize(.data = summary,
                                .by = tidyselect::all_of(c("PrimaryKey",
                                                           "LineKey",
                                                           "PointNbr")),
                                # This was just as a check during development
                                # and shouldn't be necessary.
                                # first_occurrence_layer = dplyr::first(first_occurrence_layer),
                                indicator = dplyr::first(indicator))
  }

  ##### Calculate indicators ---------------------------------------------------
  indicators <- dplyr::summarize(.data = summary,
                                 .by = tidyselect::all_of(c(internal_grouping_vars,
                                                            "indicator")),
                                 record_count = dplyr::n()) |>
    dplyr::left_join(x = _,
                     y = point_totals,
                     relationship = "many-to-one",
                     by = internal_grouping_vars) |>
    dplyr::mutate(.data = _,
                  percent = round(x = record_count / point_count * 100,
                                  digits = 2))

  ##### Remove non-qualifying indicators ---------------------------------------
  # This is the point where keeping_indicators is used to remove indicators
  # which represent incomplete sets of indicator variable values. As noted
  # above when creating keeping_indicators, this is important to hold off on
  # doing until after indicators are calculated specifically because of how
  # first hits are handled.
  # If the actual first hit recorded at a pin drop is tied to a non-qualifying
  # indicator and that record is dropped *before* the calculations, then if
  # the pin drop had a qualifying record in a lower layer that record would be
  # counted towards an indicator despite not actually being the first hit at the
  # pin drop.
  indicators <- dplyr::filter(.data = indicators,
                              indicator %in% keeping_indicator)

  ##### Add 0s where no qualifying data occurred -------------------------------
  # The absence of data is taken to mean that the indicator in question wasn't
  # present, not that it was missed. Therefore, we need to make sure that we add
  # 0s to represent that.
  # This is efficient-looking in terms of code for making sure that we have a 0
  # for every indicator on every plot that had no qualifying records for that
  # indicator, but might not be computationally cheap. Consider targeting this
  # for a refactor.

  # This will make a placeholder record for every unique combination of
  # PrimaryKey that made it to the point counting stage but didn't have data
  # qualifying for any indicators and the indicators that were calculated.
  zero_records <- expand.grid(PrimaryKey = setdiff(unique(point_totals$PrimaryKey),
                                                   unique(indicators$PrimaryKey)),
                              indicator = unique(indicators$indicator)) |>
    dplyr::mutate(.data = _,
                  percent = 0)

  # We'll combine the calculated indicators with the zero values then pivot
  # wider, putting 0s anywhere that didn't have a value (so, indicators which
  # did not occur on a plot that did have other indicators on it). Pivoting is
  # the easiest way to do that.
  output <- dplyr::bind_rows(list(indicators,
                                zero_records)) |>
    tidyr::pivot_wider(data = _,
                       id_cols = tidyselect::all_of(internal_grouping_vars),
                       names_from = "indicator",
                       values_from = "percent",
                       values_fill = 0)

  # If the user wanted the output to be tall, we'll pivot it back.
  if (tall) {
   output <- tidyr::pivot_longer(data = output,
                                 cols = -tidyselect::all_of(internal_grouping_vars),
                                 names_to = "indicator",
                                 values_to = "percent")
  }

  output
}
