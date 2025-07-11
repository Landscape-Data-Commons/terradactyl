#' Calculate the number, length, and percent of gaps
#' @description Calculate the number, length, and percent of gaps by plot or line.
#' @param gap_tall Data frame. The format must match the output from
#' \code{\link{gather_gap}()} which includes at minimum the variables PrimaryKey,
#' LineKey, LineLengthAmount, RecType, and Gap.
#' @param tall Logical. If \code{FALSE} then the output will be a named list of
#' data frames, one for each type of indicator calculated: length of transects
#' in gaps, number of gaps, and percent of transects in gaps. If \code{TRUE}
#' then the output will be a single data frame with one record for each indicator
#' type for each gap size class for each plot (or transect on a plot depending
#' on the value of \code{by_line}). Defaults to \code{FALSE}.
#' @param breaks Numeric vector. These are the breaks between gap size classes
#' in centimeters. The smallest size class is inclusive on both sides, e.g. if
#' the lowest two values in \code{breaks} are 25 and 50 then gaps of 25 or 50 cm
#' would be included in the 25-50 class. Larger classes are inclusive only on the
#' high end, e.g. if \code{breaks} is \code{c(25, 50, 100, 200)} then a 50 cm gap
#' would be included in the 25-50 class and a 100 cm gap would be included in the
#' 51-100 class. Defaults to \code{c(20, 24, 50, 100, 200)} which produces the classes
#' 20-24, 25-50, 51-100, 101-201, and 200-Inf (with \code{no_max_cutoff = TRUE} which is the default).
#' @param type Character string. The type of measurement (perennial-only canopy,
#' all-plant canopy, or basal) to use. For perennial-only calculations, use
#' \code{"P"}, \code{"perennial"}, or \code{"perennial canopy"}. For all-plant use
#' \code{"C"} or \code{"canopy"}. For basal use \code{"B"} or \code{"basal"}.
#' Defaults to \code{"canopy"}.
#' @param no_max_cutoff Logical. If \code{TRUE} then the largest size class will
#' be defined from the largest finite value in \code{breaks} to infinity. If your
#' calculations need to be restricted to a maximum gap size, set this to
#' \code{FALSE} and make sure that \code{breaks} does not include \code{Inf}.
#' Defaults to \code{TRUE}.
#' @param by_line Logical. If \code{FALSE} then results will be calculated for
#' each value in the PrimaryKey variable. If \code{TRUE} then the results will
#' be calculated for each unique combination of values in PrimaryKey and LineKey.
#' Defaults to \code{FALSE}.
#' @returns If \code{tall} is \code{TRUE} then the output is a named list
#' (percent, n, and length) of data frames containing values for each type of
#' indicator. The data frames contain the identifying variable(s) and one variable
#' for each gap size class containing the relevant indicator value. if \code{tall}
#' is \code{FALSE} then the output is a single data frame containing the identifying
#' variable(s), a variable with the indicator type, a variable with the gap
#' size class, and a variable with the indicator value. In every case, if there
#' were gap size classes for which no data qualified the values will be 0.
#' @export

# Percent Gap
#' @export gap_cover
#' @rdname gap_cover
gap_cover <- function(gap_tall,
                      tall = FALSE,
                      breaks = c(20, 24, 50, 100, 200),
                      type = "canopy",
                      no_max_cutoff = TRUE,
                      by_line = FALSE) {

  #### SETUP ###################################################################
  ##### Data checks ------------------------------------------------------------
  if (!("data.frame" %in% class(gap_tall))) {
    stop("gap_tall must be a data frame.")
  }

  gap_types <- c(C = "C",
                 c = "C",
                 canopy = "C",
                 B = "B",
                 b = "B",
                 basal = "B",
                 P = "P",
                 p = "P",
                 perennial = "P",
                 "perennial canopy" = "P")

  if (length(type) != 1) {
    stop(paste0("The value of type must be ONE of the following character strings: ",
                paste(names(gap_types),
                      collapse = ", ")))
  }

  if (!(tolower(type) %in% names(gap_types))) {
    stop(paste0("The value '", type, "' is not a valid option. Please use one of the following options instead: ",
                paste(names(gap_types),
                      collapse = ", ")))
  }

  # This will always calculate values per-plot via PrimaryKey if the user asks
  # for it to be by line within a plot this will add LineKey to the grouping
  # variables.
  grouping_vars <- c("PrimaryKey")
  if (by_line) {
    grouping_vars <- c(grouping_vars,
                       "LineKey")
  }

  # TODO: Add in checks for required variables

  ##### Getting breaks ready ---------------------------------------------------
  # If the user has specified that there's no maximum gap size, make sure that
  # Inf is included in the breaks vector. If it's already there, we don't
  # duplicate it.
  if (no_max_cutoff) {
    breaks <- c(breaks,
                Inf)
  }

  # Make sure that the breaks are in ascending order
  breaks <- unique(breaks)[order(unique(breaks))]

  # Create the size class strings from the breaks. For the smallest class, this
  # is inclusive on the left and right, so we just use the first two break
  # values as-is.
  classes <- paste0(breaks[1],
                    "-",
                    breaks[2])

  # If there are enough breaks, we need classes that are only inclusive on the
  # right, so we'll take the lower bounds and add 1 then use the upper bounds
  # without making changes.
  if (length(breaks > 2)) {
    classes <- c(classes,
                 paste(breaks[2:(length(breaks) - 1)] + 1,
                       breaks[3:length(breaks)],
                       sep = "-"))
  }


  ##### Gap data ---------------------------------------------------------------
  ###### Unit harmonization ----------------------------------------------------
  # Convert the line lengths to the same units as the gaps.
  # When Measure == 1 then the LineLengthAmount is in meters and just needs to
  # be multiplied by 100 to convert it to centimeters. If Measure == 2, then the
  # LineLengthAmount is in feet and also needs to be converted to centimeters.
  # If somehow Measure is neither 1 nor 2, then we won't touch the
  # LineLengthAmount value.
  # If Measure == 2, then the gaps and the gap minimum will be in inches and
  # need to be converted to centimeters as well.
  # The current (2024) state of the function that gathers gap does the
  # conversion from imperial to metric, so Measure == 2 is unlikely.
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            LineLengthAmount = dplyr::case_when(Measure == 1 ~ LineLengthAmount * 100,
                                                                Measure == 2 ~ LineLengthAmount * 12 * 2.54,
                                                                .default = LineLengthAmount),
                            Gap = dplyr::case_when(Measure == 2 ~ Gap * 2.54,
                                                   .default = Gap))

  ###### Total line lengths ----------------------------------------------------
  # We need to know the total length of the transects for each plot (or just for
  # each transect if the user specified by_line = TRUE)
  line_lengths <- dplyr::select(.data = gap_tall,
                                tidyselect::all_of(c("PrimaryKey",
                                                     "LineKey",
                                                     "LineLengthAmount"))) |>
    dplyr::distinct(.data = _) |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(grouping_vars),
                     total_line_length = sum(LineLengthAmount))

  ###### Making 0 records for no-gap plots -------------------------------------
  # There are plots with no qualifying gaps but which we need to put 0 values
  # into all the gap classes for.
  # We do this by identifying the PrimaryKey (or PrimaryKey/LineKey combos)
  # where we don't have any qualifying gaps. Those then get a variable for each
  # of the size classes where all the values are 0.
  zero_gaps <- dplyr::filter(.data = gap_tall,
                             RecType %in% gap_types[tolower(type)]) |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(grouping_vars),
                     no_qualifying_gaps = !any(Gap >= min(breaks) & Gap <= max(breaks))) |>
    dplyr::filter(.data = _,
                  no_qualifying_gaps) |>
    dplyr::select(.data = _,
                  -no_qualifying_gaps) |>
    dplyr::left_join(x = _,
                     y = line_lengths,
                     relationship = "one-to-one",
                     by = grouping_vars)

  if (nrow(zero_gaps) > 0) {
    for (current_class in classes) {
      zero_gaps[[current_class]] <- 0
    }
  }

  ###### Data subsetting -------------------------------------------------------
  # No sense in keeping records for a gap type not being calculated or gaps too
  # small to qualify.
  gap_tall <- dplyr::filter(.data = gap_tall,
                            RecType %in% gap_types[tolower(type)],
                            Gap >= min(breaks)) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(grouping_vars),
                  # Need this for the validity check
                  LineLengthAmount,
                  tidyselect::matches("^Gap"))

  ###### Validity checks -------------------------------------------------------
  # These are gaps where the start or end was beyond the end of the transect,
  # according to the metadata.
  # For these, the whole plot will be dropped even if by_line = TRUE.
  impossible_gaps <- dplyr::select(.data = gap_tall,
                                   tidyselect::all_of(grouping_vars),
                                   LineLengthAmount,
                                   tidyselect::matches("^Gap")) |>
    dplyr::filter(.data = _,
                  GapStart > LineLengthAmount | GapEnd > LineLengthAmount)

  if (nrow(impossible_gaps) > 0) {
    warning(paste0("There are ", length(unique(impossible_gaps$PrimaryKey)), " plots with gap records that extend beyond the end of the transect according to the metadata. These plots will be dropped from consideration."))

    gap_tall <- dplyr::filter(.data = gap_tall,
                              !(PrimaryKey %in% impossible_gaps$PrimaryKey))
  }

  # Since we no longer need these variables, drop them.
  gap_tall <- dplyr::select(.data = gap_tall,
                            -LineLengthAmount,
                            -tidyselect::matches("^Gap.+"))

  # Make sure the data don't have impossibly large amounts of gap. This should
  # only happen if gaps overlap because the ones that were longer than the
  # transects should've been caught above.
  total_gap <- dplyr::summarize(.data = gap_tall,
                                .by = tidyselect::all_of(grouping_vars),
                                total_gap = sum(Gap)) |>
    dplyr::left_join(x = _,
                     y = line_lengths,
                     relationship = "one-to-one",
                     by = grouping_vars)

  too_much_gap <- dplyr::filter(.data = total_gap,
                                total_gap > total_line_length) |>
    dplyr::mutate(.data = _,
                  drop_record = TRUE) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(grouping_vars),
                  drop_record) |>
    dplyr::distinct()

  if (nrow(too_much_gap) > 0) {
    warning(paste0("The total gap exceeded the total transect length for ",
                   nrow(too_much_gap),
                   if (length(grouping_vars) < 2) {
                     paste0(" unique values of ", grouping_vars)
                   } else {
                     paste0(" unique combinations of values in ", paste(grouping_vars,
                                                                        collapse = " and "))
                   },
                   ". Gap indicators will not be calculated for these records."))
    gap_tall <- dplyr::left_join(x = gap_tall,
                                 y = too_much_gap,
                                 relationship = "many-to-one",
                                 by = grouping_vars) |>
      dplyr::filter(.data = _,
                    is.na(drop_record)) |>
      dplyr::select(.data = _,
                    -drop_record)
  }

  ###### Classify gaps based on breaks -----------------------------------------
  # OKAY! So this uses cut() which is very nice. However, cut() needs to be told
  # if the breaks are inclusive on the right or left, which for our purposes is
  # going to be on the right EXCEPT in the case of the smallest gap class which
  # is inclusive on both sides. To deal with that, we'll just assign that class
  # to any gap smaller than the second break value and then use cut() on
  # everything else.
  # After we've got those assigned, we can split them into variables for the
  # minimum and maximum values in the class and adjust those to reflect the
  # inclusivity, e.g. if 50 is the minimum, then we'll change that to 51 because
  # it wasn't inclusive on the left.
  #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
  #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
  # NOTE!!!!! The Gap values are being truncated here when classifying them to
  # reproduce the previously existing behavior. They are not truncated when
  # calculating the indicators.
  #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
  #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
  gap_tall <- dplyr::mutate(.data = gap_tall,
                            class = dplyr::case_when(trunc(Gap) < breaks[2] ~ paste0(breaks[1],
                                                                                     ",",
                                                                                     breaks[2]),
                                                     .default = as.character(cut(x = trunc(Gap),
                                                                                 breaks = breaks,
                                                                                 right = TRUE))),
                            # This regex will yank any series of digits (or Inf)
                            # that's followed by a comma, which is the character
                            # that cut() puts between the bounding break values.
                            # This lets us avoid all the messiness of trying to
                            # work with the [, ], (, and ) that cut() uses to
                            # indicate inclusivity.
                            break_min = as.numeric(stringr::str_extract(string = class,
                                                                        pattern = "\\d+|Inf(?=,)")),
                            # This makes sure that the lower bound value gets
                            # bumped up by 1 for all classes where the lower
                            # bound wasn't the lowest break value.
                            break_min = dplyr::case_when(break_min != breaks[1] ~ break_min + 1,
                                                         .default = break_min),
                            # Same as above, but with digits (or Inf) *preceded*
                            # by a comma.
                            break_max = as.numeric(stringr::str_extract(string = class,
                                                                        pattern = "(?<=,)\\d+|Inf")))

  #### CALCULATE ###############################################################
  ##### Calculate the indicators -----------------------------------------------
  indicators <- dplyr::summarize(.data = gap_tall,
                                 .by = tidyselect::all_of(c(grouping_vars,
                                                            "break_min",
                                                            "break_max")),
                                 n = dplyr::n(),
                                 length = sum(Gap)) |>
    dplyr::left_join(x = _,
                     y = line_lengths,
                     relationship = "many-to-one",
                     by = grouping_vars) |>
    dplyr::mutate(.data = _,
                  percent = round(100 * length / total_line_length,
                                  digits = 2))

  ##### Create the output list -------------------------------------------------
  # Right now, the indicators are in a wide-ish format where there's a record
  # for each gap size class for each unique combination of grouping variables
  # and a variable each for percent, gap count, and length of gaps.
  # The output that we actually want is for each of those three types of
  # indicators to have their own data frame where there's a variable for each
  # size class and each record corresponds to a unique combination of grouping
  # variable values.
  # This will make a list of data frames where each of them contains one
  # indicator type.
  output <- lapply(X = c(percent = "percent",
                         n = "n",
                         length = "length"),
                   indicators = indicators,
                   grouping_vars = grouping_vars,
                   classes = classes,
                   zero_gaps = zero_gaps,
                   FUN = function(X, indicators, grouping_vars, classes, zero_gaps){
                     # This grabs just the current indicators (e.g., percent)
                     # and pivots them so that there's one variable for each
                     # size class. Any classes that have no values get a value
                     # of 0 for all records.
                     current_indicators <- dplyr::select(.data = indicators,
                                                         tidyselect::all_of(c(grouping_vars,
                                                                              "total_line_length")),
                                                         tidyselect::matches("^break"),
                                                         tidyselect::all_of(X)) |>
                       tidyr::pivot_wider(data = _,
                                          names_from = c("break_min",
                                                         "break_max"),
                                          names_sep = "-",
                                          values_from = tidyselect::all_of(X),
                                          values_fill = 0)

                     missing_classes <- setdiff(x = classes,
                                                names(current_indicators))

                     for (missing_class in missing_classes) {
                       current_indicators[[missing_class]] <- 0
                     }

                     # This select() lets us reorder the variables to put the
                     # classes in ascending order by size.
                     # Also, we're sticking all those zero-gap plots in here.
                     dplyr::bind_rows(dplyr::select(.data = current_indicators,
                                                    tidyselect::all_of(c(grouping_vars,
                                                                         "total_line_length")),
                                                    dplyr::all_of(classes)),
                                      zero_gaps)
                   })

  ##### Convert to tall (if asked) ---------------------------------------------
  # If the user wants all the data in a tall data frame, we can do that. We just
  # pivot each of the data frames in the list and bind them together!
  if (tall) {
    output <- lapply(X = names(output),
                     output = output,
                     grouping_vars = grouping_vars,
                     FUN = function(X, output, grouping_vars) {
                       tidyr::pivot_longer(data = output[[X]],
                                           cols = -tidyselect::all_of(c(grouping_vars,
                                                                        "total_line_length")),
                                           names_to = "gap_class",
                                           values_to = "value") |>
                         dplyr::mutate(.data = _,
                                       indicator = X) |>
                         dplyr::select(.data = _,
                                       tidyselect::all_of(c(grouping_vars,
                                                            "total_line_length",
                                                            "indicator",
                                                            "gap_class",
                                                            "value")))
                     }) |>
      dplyr::bind_rows()
  }

  output
}
