#' Percent cover
#' @description Calculate the percent cover by plot for variables or combinations of variables. Percent cover will be calculated for every combination of the variables requested, so if the variables are \code{GrowthHabitSub} and \code{Duration} then the output will contain fields like \code{Graminoid.Perennial}, \code{Graminoid.Annual}, \code{Shrub.Perennial}, etc. whereas using just the variable \code{code} will produce one column per species code. Any number of indicator variables can be used. These are calculated as cover from anywhere in the canopy column or as only the first hit in the canopy column. Any groupings where all the variable values were \code{NA} will be dropped.
#' @param lpi_tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param hit Character string. If \code{"any"} then percent cover will be calculated using any hit in the canopy column (so a single pin drop record may be counted more than once if it had hits that corresponded to different groups). If \code{"first"} then only the first canopy hit at a pin drop will be used to calculate cover.  If \code{"basal"}, then only the soil surfacy hit will be used to calculate cover. Defaults to \code{"any"}.
#' @param by_year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by_line Logical. If \code{TRUE} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param ... Optional character strings. One or more variable name from \code{lpi_tall} to calculate percent cover for, e.g. \code{"GrowthHabitSub"} to calculate percent cover by growth habits or \code{"GrowthHabitSub", "Duration"} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @export

pct_cover <- function(lpi_tall,
                      tall = FALSE,
                      hit = "any",
                      by_year = FALSE,
                      by_line = FALSE,
                      ...) {
  ## Get a list of the variables the user wants to group by.
  grouping_variables <- rlang::quos(...)

  if (!is.data.frame(lpi_tall)) {
    stop("lpi_tall must be a data frame.")
  }

  if (!(hit %in% c("any", "first", "basal"))) {
    stop("hit must be either 'any','first' or 'basal'.")
  }

  # For how deep to group. Always by plot, sometimes by line
  if (by_line) {
    level <- rlang::quos(PrimaryKey, LineKey)
  } else {
    level <- rlang::quos(PrimaryKey)
  }

  # Drop data where there is no code value
  # (i.e. layers where there was no recorded hit)
  lpi_tall <- dplyr::filter(
    .data = lpi_tall,
    !is.na("code"),
    code != "",
    code != "None",
    code != "N",
    !is.na("PrimaryKey"),
    !is.na("LineKey"),
    code != "<NA>"
  )

  # Convert all codes to upper case
  lpi_tall$code <- toupper(lpi_tall$code)

  lpi_tall <- lpi_tall %>%
    dplyr::mutate_at(dplyr::vars(!!!grouping_variables), toupper)

  # Within a plot, we need the number of pin drops, which we'll calculate
  # taking the unique combination of PrimaryKey, LineKey and Point number
  # for each group level
  point_totals <- dplyr::distinct(
    .data = lpi_tall,
    PrimaryKey, LineKey, PointNbr
  ) %>%
    dplyr::group_by(!!!level) %>%
    dplyr::summarize(point_count = dplyr::n())

  # Add the point_counts field
  # (it'll be the same for every record associated with a plot)
  lpi_tall <- dplyr::left_join(
    x = lpi_tall,
    y = point_totals,
    by = c("PrimaryKey", "LineKey")
  )

  # make sure layer is a character field
  lpi_tall$layer <- as.character(lpi_tall$layer)

  # Get the layers into the correct order
  lpi_tall <- dplyr::mutate(
    .data = lpi_tall,
    layer = factor(layer,
      levels = c(
        "TopCanopy",
        unique(lpi_tall$layer)[grepl(unique(lpi_tall$layer),
                                     pattern = "^Lower[1-7]")],
        "SoilSurface"
      )
    )
  ) %>% dplyr::arrange(layer)

  if (hit == "basal") {
    hit <- "any"
    lpi_tall <- dplyr::filter(
      .data = lpi_tall,
      layer == "SoilSurface"
    )
  }


  summary <- switch(hit,
    "any" = {
      summary <- lpi_tall %>%
        # Remove records where there are NAs for the grouping variables
        dplyr::filter(complete.cases(!!!grouping_variables)) %>%
        dplyr::group_by(
          PrimaryKey, LineKey, PointNbr, point_count,
          !!!grouping_variables
        ) %>%
        ## Here's the breakdown of the gnarly parts:
        # Because this is a tall format, we want just
        # presence/absence for the indicator at a given point
        # so we'll write in 1 if any of the layers within that indicator
        # has a non-NA and non-"" value
        dplyr::summarize(present = dplyr::if_else(any(!is.na(code) &
                                                        code != ""), 1, 0)) %>%
        tidyr::unite(indicator, !!!grouping_variables, sep = ".") %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!!level, indicator) %>%
        # Within a plot, find the sum of all the "presents"
        # then divide by the number of possible hits, which
        # we added in point_count
        dplyr::summarize(percent = 100 * sum(present, na.rm = TRUE) / dplyr::first(point_count))
    },
    "first" = {
      summary <- lpi_tall %>%
        # Remove records where there are NAs for the grouping variables
        # dplyr::filter(complete.cases(!!!grouping_variables))%>%
        # Strip out all the non-hit codes
        dplyr::filter(!(code %in% c("", NA, "None", "N"))) %>%
        dplyr::group_by(PrimaryKey, LineKey, PointNbr, point_count) %>%
        # Get the first hit at a point
        dplyr::summarize(code = dplyr::first(code)) %>%
        # Get all the other fields back
        merge(
          x = dplyr::distinct(dplyr::select(lpi_tall,
                                            "PrimaryKey",
                                            "LineKey",
                                            "PointNbr",
                                            "code",
                                            !!!grouping_variables)),
          y = .,
          all.y = TRUE
        ) %>%
        tidyr::unite(indicator,
                     !!!grouping_variables,
                     sep = ".") %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!!level, indicator) %>%
        dplyr::summarize(percent = 100 * dplyr::n() / dplyr::first(point_count)) %>%
        dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
    }
  )

  # remove rows with no grouping applied
  summary <- subset(summary, indicator != ".")

  # add zeros where no cover occurred
  summary <- suppressWarnings(
    expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                indicator = unique(summary$indicator)) %>%
    dplyr::left_join(., summary) %>%
    dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))))

  # Remove indicators that have incomplete grouping variable combinations
  summary <- summary %>% subset(!grepl(
    x = indicator,
    pattern = "^[.]|[.]$|\\.\\.|\\.NA|NA\\.|\\.NA\\."
  ))

  if (!tall) {
    summary <- tidyr::spread(summary, key = indicator, value = percent) %>%
      # Replace the NA values with 0s because they represent 0% cover for that indicator
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
