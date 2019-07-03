#' Percent Cover Indicators
#' @description Calculate the percent cover  indicators by plot or line for variables or combinations of variables.This is a family of standard indicator variables to examine total foliar cover, bare soil, litter cover, and other ground cover indicators. To compute cover by species, growth habit and duration, or other custom line-point intercept combinations, see \code{pct_cover()}.
#' @param lpi_tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
#' @param by_year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by_line Logical. If \code{TRUE} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param tall Logical. If \code{TRUE} then output will be in tall format
#' @param hit String. If \code{"first"} then only top LPI hits are included. If \code{"any"} then any hit values are included.
#' @param ... Optional bare variable names. Names of variables to include as part of grouping e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @name cover_indicators
#' @return A \code{tbl} of either wide or tall format.


#' @export pct_cover_between_plant
#' @rdname cover_indicators

# Percent Cover Between Plants####
# This function assumes that all non-plant codes are <3 characters long
pct_cover_between_plant <- function(lpi_tall,
                                    tall = FALSE,
                                    by_year = FALSE,
                                    by_line = FALSE) {
  # Calculate between plant cover
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = "first",
    by_year = by_year,
    by_line = by_line,
    code
  ) %>%
    # Remove all layer codes that are >=3 codes (i.e., species codes)
    subset(., nchar(indicator) < 3)

  if (!tall) {
    summary <- tidyr::spread(summary, key = indicator, value = percent) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c(
            "PrimaryKey",
            "PlotKey",
            "PlotID",
            "LineKey",
            "LineID"
          ))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }
  return(summary)
}
#' @export pct_cover_all_ground
#' @rdname cover_indicators

# Percent Ground Cover####
# This function assumes that all non-plant codes are <3 characters long
pct_cover_all_ground <- function(lpi_tall,
                                 tall = FALSE,
                                 by_year = FALSE,
                                 by_line = FALSE) {
  # Calculate between plant cover
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = "basal",
    by_year = by_year,
    by_line = by_line,
    code
  ) %>%
    # Remove all layer codes that are >=3 codes (i.e., species codes)
    subset(., nchar(indicator) < 3)
  if (!tall) {
    summary <- tidyr::spread(summary,
      key = indicator,
      value = percent
    ) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                           "PlotKey",
                                                           "PlotID",
                                                           "LineKey",
                                                           "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }
  return(summary)
}

#' @export pct_cover_total_foliar
#' @rdname cover_indicators

# Percent Total Foliar Cover####
pct_cover_total_foliar <- function(lpi_tall,
                                   tall = FALSE,
                                   by_year = FALSE,
                                   by_line = FALSE) {
  # Calculate between plant cover
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = "first",
    by_year = by_year,
    by_line = by_line,
    code
  ) %>%
    # Remove all layer codes that are < 3 codes (i.e., non-species codes)
    subset(., nchar(indicator) >= 3)

  # Sum all first hit plant codes to get total foliar cover
  summary <- dplyr::group_by_at(summary, names(summary)[-grep("percent|indicator", names(summary))]) %>%
    dplyr::summarize(., percent = sum(percent))
  summary$indicator <- "TotalFoliarCover"

  # Widen the data frame if tall=FALSE
  if (!tall) {
    summary <- tidyr::spread(summary,
      key = indicator,
      value = percent
    ) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                           "PlotKey",
                                                           "PlotID",
                                                           "LineKey",
                                                           "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }
  return(summary)
}
#' @export pct_cover_bare_soil
#' @rdname cover_indicators

# Percent Bare Soil Cover####
pct_cover_bare_soil <- function(lpi_tall,
                                tall = FALSE,
                                by_year = FALSE,
                                by_line = FALSE) {
  # Calculate between plant cover
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = "first",
    by_year = by_year,
    by_line = by_line,
    code
  ) %>%
    # Find all of the first hit "S" codes
    subset(., indicator == "S")
  if (!tall) {
    summary <- tidyr::spread(summary,
      key = indicator,
      value = percent
    ) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                           "PlotKey",
                                                           "PlotID",
                                                           "LineKey",
                                                           "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }
  return(summary)
}
#' @export pct_cover_litter
#' @rdname cover_indicators

# Percent Litter Cover####
pct_cover_litter <- function(lpi_tall,
                             tall = FALSE,
                             by_year = FALSE,
                             by_line = FALSE) {
  # Calculate between plant cover
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = "any",
    by_year = by_year,
    by_line = by_line,
    code
  ) %>%
    # Remove all layer codes that are <3 codes (i.e., non-species codes)
    subset(., indicator %in% c("L", "WL", "NL", "HL", "AM", "DN", "ER"))
  if (!tall) {
    summary <- tidyr::spread(summary,
      key = indicator,
      value = percent
    ) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                           "PlotKey",
                                                           "PlotID",
                                                           "LineKey",
                                                           "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }

  return(summary)
}

#' @export pct_cover_live
#' @rdname cover_indicators

# Percent Cover Live vs Dead
pct_cover_live <- function(lpi_tall,
                           tall = FALSE,
                           by_year = FALSE,
                           by_line = FALSE,
                           hit = "any",
                           ...) {
  grouping_variables <- rlang::quos(...)
  # summarize by checkbox and pre-assigned grouping variables
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = hit,
    by_year = by_year,
    by_line = by_line,
    chckbox,
    !!!grouping_variables
  )

  # remove groupings with NAs
  summary <- subset(
    summary,
    !indicator %in% c("0..", "1.NA.NA", "1..", "0.NA.NA")
  )
  # A more flexible option:
  # summary <- subset(summary,
  #                   !grepl(indicator, pattern = "\\.\\.|NA"))

  # replace "0" and "1" with live and dead
  summary$indicator <- gsub(summary$indicator, pattern = "^0", "Live")
  summary$indicator <- gsub(summary$indicator, pattern = "^1", "Dead")

  if (!tall) {
    summary <- tidyr::spread(summary,
      key = indicator,
      value = percent
    ) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                           "PlotKey",
                                                           "PlotID",
                                                           "LineKey",
                                                           "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }

  # return
  return(summary)
}

#' @export pct_cover_species
#' @rdname cover_indicators

## Percent Cover by Species
pct_cover_species <- function(lpi_tall,
                              tall = TRUE,
                              by_year = FALSE,
                              by_line = FALSE,
                              hit = "any") {
  summary <- pct_cover(lpi_tall,
    tall = TRUE,
    hit = hit,
    by_year = by_year,
    by_line = by_line,
    code
  )

  # Kick out codes of length < 3 because species codes should all be >= 3 characters
  summary <- subset(summary, nchar(indicator) >= 3)

  # pct_cover() used the species as an indicator, but that name makes less sense as an output here
  summary <- dplyr::rename(summary, Species = indicator)

  if (!tall) {
    summary <- tidyr::spread(summary,
      key = Species,
      value = percent
    ) %>%
      ## Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
          # Make a list of 0s named with the newly-created field names for replace_na()
          times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                           "PlotKey",
                                                           "PlotID",
                                                           "LineKey",
                                                           "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }

  # return
  return(summary)
}
