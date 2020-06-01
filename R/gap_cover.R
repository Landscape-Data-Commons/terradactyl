#' Calculate the number, length, and percent of gaps
#' @description Calculate the number, length, and percent of gaps by plot or line.
#' @param gap_tall Raw tables as imported from TerrADat use gather_gap .
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall
#' rather than wide and will not have observations for non-existent values e.g.,
#' if no data fell into a group on a plot, there will be no row for that group
#' on that plot. Defaults to \code{FALSE}.
#' @param by_line Logical. If \code{TRUR} then results will be reported further
#' grouped by line using the \code{LineKey} field from the data forms.
#' Defaults to \code{FALSE}.
#' @param breaks Vector of all break values. Defaults to \code{20,25, 51, 100, 200}
#' @param type String. Specifies the type of gap calculation
#' \code{"canopy", "basal", "perennial canopy"}
#' @export



# Percent Gap
#' @export gap_cover
#' @rdname gap_cover
gap_cover <- function(gap_tall,
                      tall = FALSE,
                      breaks = c(20, 25, 51, 101, 201),
                      type = "canopy",
                      by_line = FALSE){

  # For how deep to group. Always by plot, sometimes by line
  if (by_line) {
    level <- rlang::quos(PrimaryKey, LineKey)
  } else {
    level <- rlang::quos(PrimaryKey)
  }


  ## Convert the line lengths to the same units as the gaps
  # if metric (gap$Measure==1) then multiply by 100 to convert to centimeters
  gap_tall$LineLengthAmount[gap_tall$Measure == 1] <-
    100 * gap_tall$LineLengthAmount[gap_tall$Measure == 1]

  # if English (gap$Measure==2) then multiply by 12 to put the line length in inches,
  if (unique(gap_tall$Measure) %in% 2) {

    # Convert LineLengthAmount from inches to centimeters
    gap_tall$LineLengthAmount[gap_tall$Measure == 2] <-
      gap_tall$LineLengthAmount[gap_tall$Measure == 2] * 2.54 * 12

     # Convert Gap from inches to centimeters
     gap_tall$Gap[gap_tall$Measure == 2] <-
      gap_tall$Gap[gap_tall$Measure == 2] * 2.54

      # Convert GapMin from inches to centimeters
     gap_tall$GapMin[gap_tall$Measure == 2] <-
      gap_tall$MinGap[gap_tall$Measure == 2] * 2.54
  }
  ## Note if this is Basal or Canopy Gap by removing gaps from the opposite type.
  # "NA"s in RecType occur when there are no gaps
  if (type == "canopy") {
    gap_tall <- subset(gap_tall, RecType %in% "C")
  }
  if (type == "basal") {
    gap_tall <- subset(gap_tall, RecType %in% "B")
  }
  if (type == "perennial canopy") {
    gap_tall <- subset(gap_tall, RecType %in% "P")
  }

  # Summarize total line length for the plot
  gap_tall <- gap_tall %>%
    # get the distinct PrimaryKey-LineKey combinations
    dplyr::distinct(PrimaryKey, LineKey, .keep_all = TRUE) %>%
    dplyr::group_by(!!!level) %>%
    unique() %>%
    dplyr::summarize(total_line_length = sum(LineLengthAmount)) %>%

    # Merge back with original gap data
    dplyr::left_join(gap_tall, .)

  # Find the interval class for each gap
  breaks <- c(breaks, 100000)
  gap_tall$interval <- cut(gap_tall$Gap, breaks = breaks, right = FALSE)
  gap_tall$interval <- gap_tall$interval %>%
    as.character() %>%
    replace(., is.na(.), "NoGap")

  # Summarize gaps by interval class
  gap_summary <- gap_tall %>%
    dplyr::group_by(!!!level, total_line_length, interval) %>%
    # calculate number of gaps,total length of gaps, and percent of gaps
    # in each indicator category
    dplyr::summarize(
      n = length(Gap),
      length = sum(Gap)
    ) %>%
    dplyr::mutate(., percent = 100 * (length / total_line_length)) %>%
    dplyr::ungroup()

  # Subset the fields we need to output
  gap_summary <- gap_summary %>%
    dplyr::select(PrimaryKey, total_line_length, interval, n, length, percent)

  # Convert to wide format
  percent <- gap_summary %>%
    dplyr::select(., -n, -length) %>%
    tidyr::spread(key = interval, value = percent, fill = 0)
  n <- gap_summary %>%
    dplyr::select(., -percent, -length) %>%
    tidyr::spread(key = interval, value = n, fill = 0)
  length <- gap_summary %>%
    dplyr::select(., -n, -percent) %>%
    tidyr::spread(key = interval, value = length, fill = 0)


  ## If tall=FALSE, then convert to wide format
  if (!tall) {
    gap_summary <- list("percent" = percent, "n" = n, "length" = length)
  } else { # Convert back to tall, this adds zeros in needed columns
    gap_summary <- percent %>% tidyr::gather(
      key = gap_class,
      value = percent,
      -PrimaryKey,
      -total_line_length
    )
    gap_summary <- n %>%
      tidyr::gather(
        key = gap_class,
        value = n,
        -PrimaryKey,
        -total_line_length
      ) %>%
      merge(gap_summary, allow.cartesian = TRUE)
    gap_summary <- length %>%
      tidyr::gather(
        key = gap_class,
        value = length,
        -PrimaryKey,
        -total_line_length
      ) %>%
      merge(gap_summary, allow.cartesian = TRUE)
  }

  return(gap_summary)
}


