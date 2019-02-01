#' Calculate the vegetation height
#' @param height.tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param omit.zero Logical. If \code{TRUE} the results omit height measurements of \code{0}. Defaults to \code{FALSE}.
#' @param method Character string. Indicates the type of indicator, \code{"max"}, which yields the average maximum height (of the herbaceous or woody heights) on the plot or \code{"mean"} which yields the mean height by functional group (woody/herbaceous).
#' @param by.line Logical. If \code{TRUE} then the results will be calculated on a per-line basis. If \code{FALSE} then the results will be calculated on a per-plot basis. Defaults to \code{FALSE}.
#' @param ... Optional bare variable names. One or more variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @export mean.height

mean.height <- function(height.tall,
                        method = "mean",
                        omit.zero = TRUE,
                        by.line = FALSE,
                        tall = FALSE,
                        ...) {
  ## Get a list of the variables the user wants to group by.
  grouping.variables <- rlang::quos(...)

  if (!is.data.frame(height.tall)) {
    stop("lpi.tall must be a data frame.")
  }

  if (!(method %in% c("mean", "max"))) {
    stop("method must be either 'mean' or 'max'.")
  }

  # For grouping by line
  if (by.line) {
    level <- rlang::quos(PrimaryKey, LineKey)
  } else {
    level <- rlang::quos(PrimaryKey)
  }

  # If height of zer0 is dropped by the calculation, filter out zeros
  if (omit.zero) {
    height.tall <- dplyr::filter(height.tall, Height != 0)
  }

  # Calculate mean height by grouping variable, if method == "mean"
  if (method == "mean") {
    summary <- height.tall %>%
      dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(
        !!!level,
        !!!grouping.variables
      ) %>%
      dplyr::summarize(mean.height = mean(as.numeric(Height))) %>%
      tidyr::unite(indicator,
        !!!grouping.variables,
        sep = "."
      )

    summary <- summary[!grepl(summary$indicator, pattern = "NA.|.NA"), ]
  }
  # Calculate the max height by grouping variable, if method =="max"
  if (method == "max") {
    height.tall.spread <- height.tall %>% tidyr::spread(
      key = type,
      value = Height
    )
    height.tall.spread$max <- pmax(height.tall.spread$herbaceous,
      height.tall.spread$woody,
      na.rm = TRUE
    )
    summary <- height.tall.spread %>%
      dplyr::group_by(!!!level, !!!grouping.variables) %>%
      dplyr::summarize(max.height = mean(max)) %>%
      dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
  }


  # Convert to wide format
  if (!tall) {
    summary <- summary %>% tidyr::spread(key = indicator, value = mean.height, fill = 0)
  }

  return(summary)
}
