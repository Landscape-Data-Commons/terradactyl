pct_cover_indicators <- function(lpi_tall,
                                 indicator_families = c("total foliar",
                                                        "ground",
                                                        "between plant",
                                                        "bare soil",
                                                        "litter",
                                                        "live",
                                                        "species"),
                                 tall_output = c("total foliar" = FALSE,
                                                 "ground" = FALSE,
                                                 "between plant" = FALSE,
                                                 "bare soil" = FALSE,
                                                 "litter" = FALSE,
                                                 "live" = FALSE,
                                                 # Special case!
                                                 "species" = TRUE),
                                 by_line = FALSE,
                                 hit_type = c(live = "any",
                                              species = "any"),
                                 indicator_variables = list(`total foliar` = "code",
                                                            ground = "code",
                                                            `between plant` = "code",
                                                            `bare soil` = "code",
                                                            litter = "code",
                                                            live = c("chckbox",
                                                                     "code"),
                                                            species = "code"),
                                 verbose = FALSE,
                                digits = 1) {
  #### Santization and setup ###################################################
  # TODO: ALL THE COMPLIANCE CHECKS

  # We'll use this to make a lookup table for which kind of cover values to
  # calculate based on which sets of indicators are asked for.
  families_list <- list(any = c("litter"),
                        first = c("total foliar",
                                  "between plant",
                                  "bare soil"),
                        basal = "ground")

  # The hit_type argument is used for "live" and per-species calculations
  # because those requests can vary in terms of hit_type.
  needed_hit_types <- intersect(x = c("live",
                                      "species"),
                                y = indicator_families)
  if (length(needed_hit_types) > 0) {
    hit_type <- hit_type[names(hit_type) %in% c("live",
                                                "species")]

    # Default to "any" for unspecified versions
    missing_hit_types <- setdiff(x = c("live",
                                       "species"),
                                 y = names(hit_type))
    for (current_missing_hit_type in missing_hit_types) {
      hit_type[current_missing_hit_type] <- "any"
    }

    # Add this information to the families_list object so we can include it in
    # the lookup table.
    for (current_hit_type in hit_type) {
      families_list[[current_hit_type]] <- c(families_list[[current_hit_type]],
                                             names(hit_type)[hit_type == current_hit_type]) |>
        unique()
    }
  }

  # Make a lookup table with the indicator families and the hit types for easy
  # reference later
  calc_lut <- lapply(X = names(families_list),
                     families_list = families_list,
                     FUN = function(X, families_list){
                       data.frame(hit = X,
                                  cover_type = families_list[[X]])
                     }) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data = _,
                  cover_type %in% indicator_families)

  indicator_variables_lut <- lapply(X = names(indicator_variables),
                                    indicator_variables = indicator_variables,
                                    FUN = function(X, indicator_variables){
                                      data.frame(cover_type = X,
                                                 indicator_variables = paste(indicator_variables[[X]],
                                                                             collapse = "|"))
                                    }) |>
    dplyr::bind_rows()

  calc_lut <- dplyr::left_join(x = calc_lut,
                               y = indicator_variables_lut,
                               relationship = "one-to-one",
                               by = "cover_type")

  #### Calculating the input cover values ######################################
  # This is the minimum set of hit types to calculate for. The lapply() below
  # will deal with the need for idfferent grouping variables.
  iteration_vector <- setNames(object = calc_lut$hit,
                               nm = calc_lut$hit) |>
    unique()

  # Get the data frames that we'll use to calculate the various indicators from.
  # This'll be a list with one index per hit type and at each index a list
  # containing a data frame forr each set of needed grouping variables based on
  # the indicator_variables list provided as an argument.
  values_list <- lapply(X = iteration_vector,
                        calc_lut = calc_lut,
                        indicator_variables = indicator_variables,
                        lpi_tall = lpi_tall,
                        by_line = by_line,
                        FUN = function(X, calc_lut, indicator_variables, lpi_tall, by_line){
                          current_hit_type <- X
                          current_indicator_variables_list <- indicator_variables[dplyr::filter(.data = calc_lut,
                                                                                                hit == current_hit_type)[["cover_type"]]] |>
                            unique()

                          current_indicator_variables_list <- sapply(X = current_indicator_variables_list,
                                                                     collapse = "|",
                                                                     FUN = paste) |>
                            setNames(object = current_indicator_variables_list,
                                     nm = _)

                          hit_values_list <- lapply(X = current_indicator_variables_list,
                                                    current_hit_type = current_hit_type,
                                                    lpi_tall = lpi_tall,
                                                    by_line = by_line,
                                                    FUN = function(X, current_hit_type, lpi_tall, tall, by_line){
                                                      # message(paste(current_hit_type, "by", paste(X,
                                                      #                                             collapse = ", ")))
                                                      pct_cover(lpi_tall = lpi_tall,
                                                                tall = TRUE,
                                                                hit = current_hit_type,
                                                                by_line = by_line,
                                                                indicator_variables = X,
                                                               digits = digits)
                                                    })
                        })
  names(values_list) <- iteration_vector

  #### Calculating outputs #####################################################
  # Use the values_list to calculate the various indicators requested, storing
  # them in outputs_list.
  # These are hardcoded as individual if()s because they're all special.
  # Maybe eventually this can be handled in a way that doesn't duplicate code.
  outputs_list <- list()

  ##### Total foliar cover -----------------------------------------------------
  if ("total foliar" %in% indicator_families) {
    # Calculate total foliar cover by summing the first hits for all plant
    # species on a plot (and by line if requested)
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "total foliar")
    # output <- values_list[[which(names(iteration_vector) == current_lut[["hit"]])]][[current_lut[["indicator_variables"]]]] |>
    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Remove all layer codes that are < 3 codes (i.e., non-species codes)
      dplyr::filter(.data = _,
                    nchar(indicator) >= 3) |>
      # Replace the indicator value with "TotalFoliarCover" because we should only
      # have records with recorded species at this point.
      dplyr::mutate(.data = _,
                    indicator = "TotalFoliarCover") |>
      # Sum the remaining records, grouped by whatever variables aren't holding
      # the percentages because those should be indicator (all the same value)
      # and then whatever grouping variables came through the pct_cover().
      dplyr::summarize(.data = _,
                       .by = -tidyselect::any_of(x = c("percent")),
                       indicator = dplyr::first(indicator),
                       # The na.rm = TRUE should produce 0 when all inputs are
                       # NA and effectively treat NA as 0 when real values are
                       # involved.
                       percent = sum(percent,
                                     na.rm = TRUE))

    # Widen the data frame if the user asked for that.
    if (!tall_output["total foliar"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["total_foliar"]] <- dplyr::distinct(current_output)
  }

  ##### Ground (basal) cover ---------------------------------------------------
  if ("ground" %in% indicator_families) {
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "ground")
    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Remove all layer codes that are >= 3 codes (i.e., species codes)
      # THIS SEEMS REALLY WRONG?????????????
      dplyr::filter(.data = _,
                    nchar(indicator) < 3)

    # Widen the data frame if the user asked for that.
    if (!tall_output["ground"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["ground"]] <- dplyr::distinct(current_output)
  }

  ##### Between-plant cover ----------------------------------------------------
  if ("between plant" %in% indicator_families) {
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "between plant")
    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Remove all layer codes that are >= 3 codes (i.e., species codes)
      dplyr::filter(.data = _,
                    nchar(indicator) < 3)

    # Widen the data frame if the user asked for that.
    if (!tall_output["between plant"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["between_plant"]] <- dplyr::distinct(current_output)
  }

  ##### Bare soil --------------------------------------------------------------
  if ("bare soil" %in% indicator_families) {
    soil_codes <- c("S", "PC", "FG", "AG", "LM")
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "bare soil")

    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Keep only the soil code records
      dplyr::filter(.data = _,
                    indicator %in% soil_codes)

    # Widen the data frame if the user asked for that.
    if (!tall_output["bare soil"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["bare_soil"]] <- dplyr::distinct(current_output)
  }

  ##### Litter -----------------------------------------------------------------
  if ("litter" %in% indicator_families) {
    litter_codes <- c("L", "WL", "NL", "HL", "AM", "DN", "ER")
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "litter")

    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Keep only the litter code records
      dplyr::filter(.data = _,
                    indicator %in% litter_codes)

    # Widen the data frame if the user asked for that.
    if (!tall_output["litter"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["litter"]] <- dplyr::distinct(current_output)
  }

  ##### Live cover -------------------------------------------------------------
  if ("live" %in% indicator_families) {
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "live")

    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Keep only the records with "real" indicators (no NAs in the variables
      # used to define the indicators)
      dplyr::filter(.data = _,
                    !stringr::str_detect(string = indicator,
                                         # pattern = "(((\\.)+)|((\\.NA)+)$)")) |>
                                         pattern = "((\\.)+$)|((\\.NA)+$)")) |>
      # Replace the 1s and 0s with "Dead" and "Live"
      dplyr::mutate(.data = _,
                    indicator = stringr::str_replace_all(string = indicator,
                                                         pattern = c("^1(\\.)?" = "Dead",
                                                                     "^0(\\.)?" = "Live")))

    # Widen the data frame if the user asked for that.
    if (!tall_output["live"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["live"]] <- dplyr::distinct(current_output)
  }

  ##### Species cover ----------------------------------------------------------
  if ("species" %in% indicator_families) {
    current_lut <- dplyr::filter(.data = calc_lut,
                                 cover_type == "species")
    # output <- values_list[[which(names(iteration_vector) == current_lut[["hit"]])]][[current_lut[["indicator_variables"]]]] |>
    current_output <- values_list[[current_lut[["hit"]]]][[current_lut[["indicator_variables"]]]] |>
      # Remove all layer codes that are < 3 codes (i.e., non-species codes)
      dplyr::filter(.data = _,
                    nchar(indicator) >= 3)

    # Widen the data frame if the user asked for that.
    if (!tall_output["species"]) {
      current_output <- tidyr::pivot_wider(data = current_output,
                                           names_from = "indicator",
                                           values_from = "percent",
                                           values_fill = 0)
    }

    # Can't hurt to ask for only distinct records, right?
    outputs_list[["species"]] <- dplyr::distinct(current_output)
  }

  #### Combining and cleanup ###################################################
  outputs_list
}


#' Percent Cover Indicators, wrapper functions of \code{pct_cover}
#' @description Calculate the percent cover  indicators by plot or line for variables or combinations of variables.This is a family of standard indicator variables to examine total foliar cover, bare soil, litter cover, and other ground cover indicators. To compute cover by species, growth habit and duration, or other custom line-point intercept combinations, see \code{pct_cover()}.
#' @param lpi_tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
#' @param by_line Logical. If \code{TRUE} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param tall Logical. If \code{TRUE} then output will be in tall format
#' @param hit String. If \code{"first"} then only top LPI hits are included. If \code{"any"} then any hit values are included.Only used for \code{pct_cover_live} and \code{pct_cover_species}.
#' @param ... Optional bare variable names. Only used for \code{pct_cover_live}. Names of variables to include as part of grouping e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param digits Integer. The number of decimal places that the output values will be rounded to. Values larger than \code{2} are not recommended because they will likely imply false precision. Defaults to \code{1}.
#' @name cover_indicators
#' @return A \code{tbl} of either wide or tall format.
#' @examples
#' # Calculate between plant cover, or non-plant species first hit cover
#' between_plant_cover <- pct_cover_between_plant(lpi_tall,
#'                                                tall = FALSE,
#'                                                by_line = FALSE)
#'
#' # Calculate ground cover, including those under plants
#' between_plant_cover <- pct_cover_all_ground(lpi_tall,
#'                                             tall = FALSE,
#'                                             by_line = FALSE)
#'


#' @export pct_cover_between_plant
#' @rdname cover_indicators

# Percent Cover Between Plants####
# This function assumes that all non-plant codes are <3 characters long
pct_cover_between_plant <- function(lpi_tall,
                                    tall = FALSE,
                                    by_line = FALSE,
                                    verbose = FALSE) {
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("between plant"),
                       tall_output = c("between plant" = tall),
                       by_line = by_line,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # # Calculate between plant cover
  # output <- pct_cover(lpi_tall,
  #                     tall = TRUE,
  #                     hit = "first",
  #                     by_line = by_line,
  #                     indicator_variables = "code") |>
  #   # Remove all layer codes that are >=3 codes (i.e., species codes)
  #   dplyr::filter(.data = _,
  #                 nchar(indicator) < 3)
  #
  # if (!tall) {
  #   summary <- tidyr::spread(summary, key = indicator, value = percent) %>%
  #     ## Replace the NA values with 0s because they represent 0% cover for that indicator
  #     tidyr::replace_na(replace = setNames(
  #       as.list(rep.int(0,
  #                       # Make a list of 0s named with the newly-created field names for replace_na()
  #                       times = length(unique(names(.)[!(names(.) %in% c(
  #                         "PrimaryKey",
  #                         "PlotKey",
  #                         "PlotID",
  #                         "LineKey",
  #                         "LineID"
  #                       ))]))
  #       )),
  #       unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
  #     ))
  # }
  # return(summary)
}
#' @export pct_cover_all_ground
#' @rdname cover_indicators

# Percent Ground Cover####
# This function assumes that all non-plant codes are <3 characters long
pct_cover_all_ground <- function(lpi_tall,
                                 tall = FALSE,
                                 by_line = FALSE,
                                 verbose = FALSE) {
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("ground"),
                       tall_output = c("ground" = tall),
                       by_line = by_line,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # # Calculate between plant cover
  # summary <- pct_cover(lpi_tall,
  #                      tall = TRUE,
  #                      hit = "basal",
  #
  #                      by_line = by_line,
  #                      code
  # ) %>%
  #   # Remove all layer codes that are >=3 codes (i.e., species codes)
  #   subset(., nchar(indicator) < 3)
  # if (!tall) {
  #   summary <- tidyr::spread(summary,
  #                            key = indicator,
  #                            value = percent
  #   ) %>%
  #     ## Replace the NA values with 0s because they represent 0% cover for that indicator
  #     tidyr::replace_na(replace = setNames(
  #       as.list(rep.int(0,
  #                       # Make a list of 0s named with the newly-created field names for replace_na()
  #                       times = length(unique(names(.)[!(names(.) %in% c(
  #                         "PrimaryKey",
  #                         "PlotKey",
  #                         "PlotID",
  #                         "LineKey",
  #                         "LineID"
  #                       ))]))
  #       )),
  #       unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
  #     ))
  # }
  # return(summary)
}

#' @export pct_cover_total_foliar
#' @rdname cover_indicators

# Percent Total Foliar Cover####
pct_cover_total_foliar <- function(lpi_tall,
                                   tall = FALSE,
                                   by_line = FALSE,
                                   verbose = FALSE,
                                  digits = 1) {
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("total foliar"),
                       tall_output = c("total foliar" = tall),
                       by_line = by_line,
                       digits = digits,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # summarization_vars <- "PrimaryKey"
  # if (by_line) {
  #   summarization_vars <- c(summarization_vars,
  #                           "LineKey")
  # }
  #
  # # Calculate total foliar cover by summing the first hits for all plant species
  # # on a plot (and by line if requested)
  # output <- pct_cover(lpi_tall,
  #                     tall = TRUE,
  #                     hit = "first",
  #                     by_line = by_line,
  #                     indicator_variables = "code") |>
  #   # Remove all layer codes that are < 3 codes (i.e., non-species codes)
  #   dplyr::filter(.data = _,
  #                 nchar(indicator) >= 3) |>
  #   # Replace the indicator value with "TotalFoliarCover" because we should only
  #   # have records with recorded species at this point.
  #   dplyr::mutate(.data = _,
  #                 indicator = "TotalFoliarCover") |>
  #   # Sum the remaining records, grouped by whatever variables aren't holding
  #   # the percentages because those should be indicator (all the same value)
  #   # and then whatever grouping variables came through the pct_cover().
  #   dplyr::summarize(.data = _,
  #                    .by = -tidyselect::any_of(x = c("percent")),
  #                    indicator = dplyr::first(indicator),
  #                    percent = sum(percent,
  # na.rm = TRUE))
  #
  # # Widen the data frame if the user asked for that.
  # if (!tall) {
  #   output <- tidyr::pivot_wider(data = output,
  #                                names_from = "indicator",
  #                                values_from = "percent",
  #                                values_fill = 0)
  # }
  #
  # # Can't hurt to ask for only distinct records, right?
  # dplyr::distinct(output)
}

#' @export pct_cover_bare_soil
#' @rdname cover_indicators

# Percent Bare Soil Cover####
pct_cover_bare_soil <- function(lpi_tall,
                                tall = FALSE,
                                by_line = FALSE,
                                verbose = FALSE) {
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("bare soil"),
                       tall_output = c("bare soil" = tall),
                       by_line = by_line,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # soil_codes <- c("S", "PC", "FG", "AG", "LM")
  # # Calculate first hit cover then keep only the ones for soil codes.
  # output <- pct_cover(lpi_tall,
  #                     tall = TRUE,
  #                     hit = "first",
  #                     by_line = by_line,
  #                     indicator_variables = "code") |>
  #   dplyr::filter(.data = _,
  #                 indicator %in% soil_codes)
  #
  #
  # # Widen the data frame if the user asked for that.
  # if (!tall) {
  #   output <- tidyr::pivot_wider(data = output,
  #                                names_from = "indicator",
  #                                values_from = "percent",
  #                                values_fill = 0)
  # }
  # return(output)
}

#' @export pct_cover_litter
#' @rdname cover_indicators

# Percent Litter Cover####
pct_cover_litter <- function(lpi_tall,
                             tall = FALSE,
                             by_line = FALSE,
                             verbose = FALSE) {
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("litter"),
                       tall_output = c("litter" = tall),
                       by_line = by_line,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # litter_codes <- c("L", "WL", "NL", "HL", "AM", "DN", "ER")
  # # Calculate the any-hit cover
  # output <- pct_cover(lpi_tall,
  #                     tall = TRUE,
  #                     hit = "any",
  #                     by_line = by_line,
  #                     code) |>
  #   # Keep only cover by litter codes
  #   dplyr::filter(.data = _,
  #                 indicator %in% litter_codes)
  #
  # if (!tall) {
  #   output <- tidyr::pivot_wider(data = output,
  #                                names_from = "indicator",
  #                                values_from = "percent",
  #                                values_fill = 0)
  # }
  #
  # return(summary)
}

#' @export pct_cover_live
#' @rdname cover_indicators

# Percent Cover Live vs Dead ####
pct_cover_live <- function(lpi_tall,
                           tall = FALSE,
                           by_line = FALSE,
                           hit = "any",
                           ...,
                           verbose = FALSE) {
  grouping_variables <- rlang::quos(...)
  # This here because we're trying to support the legacy decision to originally
  # allow for bare variables as the indicator-defining variables.
  # Now it can be bare variable names, character strings, vectors of character
  # strings or some combination of the three.
  # BUT! You can't create a vector, store it in the environment, and then pass
  # it in by name because then you end up with just the name of the vector.
  indicator_variables <- c("chckbox",
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
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("live"),
                       tall_output = c("live" = tall),
                       hit_type = c(live = hit),
                       indicator_variables = list(live = indicator_variables),
                       by_line = by_line,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # # summarize by checkbox and pre-assigned grouping variables
  # summary <- pct_cover(lpi_tall,
  #                      tall = TRUE,
  #                      hit = hit,
  #                      by_line = by_line,
  #                      chckbox,
  #                      !!!grouping_variables
  # )
  #
  # # remove groupings with NAs
  # summary <- subset(
  #   summary,
  #   !indicator %in% c("0..", "1.NA.NA", "1..", "0.NA.NA")
  # )
  # # A more flexible option:
  # # summary <- subset(summary,
  # #                   !grepl(indicator, pattern = "\\.\\.|NA"))
  #
  # # replace "0" and "1" with live and dead
  # summary$indicator <- stringr::str_replace_all(
  #   summary$indicator,
  #   c(
  #     "1\\." = "Dead\\.",
  #     "0\\." = "Live\\.",
  #     "0" = "Live",
  #     "1" = "Dead"
  #   )
  # )
  #
  # if (!tall) {
  #   summary <- tidyr::spread(summary,
  #                            key = indicator,
  #                            value = percent
  #   ) %>%
  #     ## Replace the NA values with 0s because they represent 0% cover for that indicator
  #     tidyr::replace_na(replace = setNames(
  #       as.list(rep.int(0,
  #                       # Make a list of 0s named with the newly-created field names for replace_na()
  #                       times = length(unique(names(.)[!(names(.) %in% c(
  #                         "PrimaryKey",
  #                         "PlotKey",
  #                         "PlotID",
  #                         "LineKey",
  #                         "LineID"
  #                       ))]))
  #       )),
  #       unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
  #     ))
  # }
  #
  # # return
  # return(summary)
}

#' @export pct_cover_species
#' @rdname cover_indicators

# Percent Cover by Species ####
pct_cover_species <- function(lpi_tall,
                              tall = TRUE,
                              by_line = FALSE,
                              hit = "any",
                              verbose = FALSE) {
  pct_cover_indicators(lpi_tall = lpi_tall,
                       indicator_families = c("species"),
                       tall_output = c("species" = tall),
                       hit_type = c(species = hit),
                       by_line = by_line,
                       verbose = verbose)[[1]]
  #### OLD APPROACH BELOW ######################################################
  # summary <- pct_cover(lpi_tall,
  #                      tall = TRUE,
  #                      hit = hit,
  #                      by_line = by_line,
  #                      code
  # )
  #
  # # Kick out codes of length < 3 because species codes should all be >= 3 characters
  # summary <- subset(summary, nchar(indicator) >= 3)
  #
  # # pct_cover() used the species as an indicator, but that name makes less sense as an output here
  # summary <- dplyr::rename(summary, Species = indicator)
  #
  # if (!tall) {
  #   summary <- tidyr::spread(summary,
  #                            key = Species,
  #                            value = percent
  #   ) %>%
  #     ## Replace the NA values with 0s because they represent 0% cover for that indicator
  #     tidyr::replace_na(replace = setNames(
  #       as.list(rep.int(0,
  #                       # Make a list of 0s named with the newly-created field names for replace_na()
  #                       times = length(unique(names(.)[!(names(.) %in% c(
  #                         "PrimaryKey",
  #                         "PlotKey",
  #                         "PlotID",
  #                         "LineKey",
  #                         "LineID"
  #                       ))]))
  #       )),
  #       unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
  #     ))
  # }
  #
  # # return
  # return(summary)
}





