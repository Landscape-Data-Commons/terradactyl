#' Calculate the number, length, and percent of gaps
#' @description Calculate the number, length, and percent of gaps by plot or line.
#' @param gap.tall Raw tables as imported from TerrADat Use data from \code{read.dima( all=T)}.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param by.year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by.line Logical. If \code{TRUR} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param breaks Vector of all break values.
#' @param type
#' @export


# Percent Gap
gap.cover <- function(gap.tall,
                      tall = FALSE,
                      breaks = c(20, 25, 51, 100, 200),
                      type = "canopy",
                      by.line = FALSE) {

  # For how deep to group. Always by plot, sometimes by line
  if (by.line) {
    level <- rlang::quos(PrimaryKey, LineKey)
  } else {
    level <- rlang::quos(PrimaryKey)
  }


  ## Convert the line lengths to the same units as the gaps
  # if metric (gap$Measure==1) then multiply by 100 to put the line length in centimeters
  gap.tall$LineLengthAmount[gap.tall$Measure == 1] <- 100 * gap.tall$LineLengthAmount[gap.tall$Measure == 1]

  # if English (gap$Measure==2) then multiply by 12 to put the line length in inches, then convert both the line length and measured gaps to metric
  if (unique(gap.tall$Measure) %in% 2) {
    gap.tall$LineLengthAmount[gap.tall$Measure == 2] <- gap.tall$LineLengthAmount[gap.tall$Measure == 2] * 2.54
    gap.tall$Gap[gap.tall$Measure == 2] <- gap.tall$Gap[gap.tall$Measure == 2] * 2.54
    gap.tall$GapMin[gap.tall$Measure == 2] <- gap.tall$MinGap[gap.tall$Measure == 2] * 2.54
  }
  ## Note if this is Basal or Canopy Gap by removing gaps from the opposite type.
  # "NA"s in RecType occur when there are no gaps
  if (type == "canopy") {
    gap.tall <- subset(gap.tall, RecType %in% "C")
  }
  if (type == "basal") {
    gap.tall <- subset(gap.tall, RecType %in% "B")
  }
  if (type == "perennial canopy") {
    gap.tall <- subset(gap.tall, RecType %in% "P")
  }

  # Summarize total line length for the plot
  gap.tall <- gap.tall %>%
    # get the distinct PrimaryKey-LineKey combinations
    dplyr::distinct(PrimaryKey, LineKey, .keep_all = TRUE) %>%
    dplyr::group_by(!!!level) %>%
    unique() %>%
    dplyr::summarize(total.line.length = sum(LineLengthAmount)) %>%
    # Merge back with original gap data
    dplyr::left_join(gap.tall, .)

  # Find the interval class for each gap
  breaks <- c(breaks, 100000)
  gap.tall$interval <- cut(gap.tall$Gap, breaks = breaks, right = FALSE)
  gap.tall$interval <- gap.tall$interval %>%
    as.character() %>%
    replace(., is.na(.), "NoGap")

  # Summarize gaps by interval class
  gap.summary <- gap.tall %>%
    dplyr::group_by(!!!level, total.line.length, interval) %>%
    # calculate number of gaps,total length of gaps, and percent of gaps
    # in each indicator category
    dplyr::summarize(
      n = length(Gap),
      length = sum(Gap)
    ) %>%
    dplyr::mutate(., percent = 100 * (length / total.line.length)) %>%
    dplyr::ungroup()

  # Subset the fields we need to output
  gap.summary <- gap.summary %>%
    dplyr::select(PrimaryKey, total.line.length, interval, n, length, percent)

  # Convert to wide format
  percent <- gap.summary %>%
    dplyr::select(., -n, -length) %>%
    tidyr::spread(key = interval, value = percent, fill = 0)
  n <- gap.summary %>%
    dplyr::select(., -percent, -length) %>%
    tidyr::spread(key = interval, value = n, fill = 0)
  length <- gap.summary %>%
    dplyr::select(., -n, -percent) %>%
    tidyr::spread(key = interval, value = length, fill = 0)


  ## If tall=FALSE, then convert to wide format
  if (!tall) {
    gap.summary <- list("percent" = percent, "n" = n, "length" = length)
  } else { # Convert back to tall, this adds zeros in needed columns
    gap.summary <- percent %>% tidyr::gather(
      key = Gap.Class,
      value = percent,
      -PrimaryKey,
      -total.line.length
    )
    gap.summary <- n %>%
      tidyr::gather(
        key = Gap.Class,
        value = n,
        -PrimaryKey,
        -total.line.length
      ) %>%
      merge(gap.summary, allow.cartesian = TRUE)
    gap.summary <- length %>%
      tidyr::gather(
        key = Gap.Class,
        value = length,
        -PrimaryKey,
        -total.line.length
      ) %>%
      merge(gap.summary, allow.cartesian = TRUE)
  }

  return(gap.summary)
}


##### Checks####
# of lines
## max line length (should be less than 15000 cm)
