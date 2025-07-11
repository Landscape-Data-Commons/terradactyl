unquoted_to_character <- function(...) {
  character_vector <- rlang::quos(...) |>
                             as.character() |>
                             # This does the cleanup that removes the prefixed ~ from everything as well
                             # as any quotation marks or bits of the definition of a vector.
                             stringr::str_replace_all(string = _,
                                                      pattern = "(^~)|(\\\")|(c\\()|(\\)$)",
                                                      replacement = "") |>
                             stringr::str_split(string = _,
                                                pattern = ",[ ]*",
                                                simplify = TRUE) |>
                             as.vector() |>
    unique()
}

read_whatever <- function(input,
                          layer = NULL) {
  if ("character" %in% class(input)) {
    if (tools::file_ext(input) == "Rdata") {
      output <- readRDS(file = lpi_tall)
    } else {
      stop("When input is a character string it must be the path to a .Rdata file containing a data frame.")
    }
  } else if (!("data.frame" %in% class(input))) {
    stop("When input is not a filepath as a character string it must be a data frame.")
  }
  output
}
