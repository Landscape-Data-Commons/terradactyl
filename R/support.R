# There are a number of functions in this package that use the ellipsis to
# allow for unnamed/freeform arguments to be passed in.
# This can be a real pain to support in some contexts, so this function will
# take those and convert them into a vector of character strings which allows
# for some internal-but-invisible-to-the-user handling of whatever they've done.
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
  character_vector
}

# OKAY! So this is a pain, but we've got a bunch of legacy code that expects
# to be fed paths to .Rdata files but also sometimes paths to geodatabases
# or .TXT or .CSV files.
# Just for ease of use in those legacy code situations, if input is a data
# frame then we'll just pass that through as the output.
# The goal here is to handle all of those as seamlessly as possible.
# If regex is TRUE and best_guess is also TRUE, then matching multiple feature
# classes will be resolved by simply reading in the one with the shortest name.
#' Read in various formats smoothly
#' @description
#' This function will flexibly read in a variety or formats including RDS, Rdata, CSV, and geodatabase feature classes.
#' It will also accept data frames, although those will be returned unaltered.
#' This exists to facilitate legacy code that was originally written using terradactyl v1.1.0 and earlier.
#'
#' @param input Data frame or character string. If a character string, this should point to an RDS, Rdata, CSV, or GDB file.
#' @param layer Optional character string. If this is not \code{NULL} and \code{input} points to a geodatabase, this is the name of the table or feature class to try to read in. Defaults to \code{NULL}.
#' @param regex Logical. If \code{TRUE} then \code{layer} will be treated as a regular expression. Defaults to \code{FALSE}.
#' @param best_guess Logical. If \code{regex} is \code{TRUE} and this is \code{TRUE} then in the case that multiple layers in the geodatabase match the regular expression \code{layer} the one with the shortest name will be used. If \code{FALSE} and multiple layers match the regular expression the function will stop and return an error. Defaults to \code{TRUE}.
#' @param accept_failure Logical. If \code{FALSE} and \code{regex} is \code{TRUE} then in the case that no layers match the regular expression the function will stop an return an error. If \code{TRUE} then the function will return \code{NULL}. Defaults to \code{FALSE}.
#' @param verbose Logical. If \code{TRUE} the function will produce diagnostic
#'   messages. Defaults to \code{FALSE}.
#' @export
read_whatever <- function(input,
                          layer = NULL,
                          regex = FALSE,
                          best_guess = TRUE,
                          accept_failure = FALSE,
                          verbose = FALSE) {
  # Get the class of input
  input_class <- class(input)

  # Because some objects have multiple classes, we're going to limit input_class
  # to only the relevant one for the particular situation.
  valid_input_classes <- c("character",
                           "data.frame")
  input_class <- intersect(x = input_class,
                           y = valid_input_classes)

  # input_class should be either 0 or 1, but in case it's somehow 2 we're still
  # good.
  if (length(input_class) != 1) {
    stop(paste0("input must be a single character string specifying a filepath to read from or a data frame to pass on. Right now class(input) returns the following class(es): ",
                paste(class(input),
                      collapse = ", ")))
  }

  # If the input is a data frame, we're just gonna pass it right back. This will
  # help make it cleaner to handle reading in various other functions.
  if (is.data.frame(input)) {
    if (verbose) {
      message("The current input is a data frame and will be returned unaltered.")
    }
    output <- input
  }

  # Now the complicated stuff! Handling the various kinds of files that might be
  # read in.
  if (is.character(input)) {
    # These are the supported filetypes (for now)
    valid_filetypes <- c("gdb",
                         "rdata",
                         "rds",
                         "csv")
    current_input_filetype <- tools::file_ext(x = input) |>
      tolower(x = _)

    if (!(current_input_filetype %in% valid_filetypes)) {
      stop(paste0("The file extension at the end of input is '", current_input_filetype, "' but must be one of the following: ",
                  paste(valid_filetypes,
                        collapse = ", ")))
    }

    if (!file.exists(input)) {
      stop(paste("Unable to find the input file. Please confirm the validity of the filepath:",
                 input))
    }

    # Given that the file exists and should be interpretable, try to read it in.
    output <- switch(EXPR = current_input_filetype,
                     "gdb" = {
                       # Only bother to check layer if it's actually going to be
                       # used for something.
                       if (!is.character(layer) | length(layer) > 1) {
                         stop("layer must be a single character string if input is the filepath to a geodatabase.")
                       }

                       # Figure out what layers are present so we can check to
                       # see if the layer name provided is in there OR treat the
                       # provided layer string as a regex pattern to search
                       # through what's available and try to identify a layer.
                       available_layers <- sf::st_layers(dsn = input)$name

                       if (regex) {
                         matched_layers <- available_layers[stringr::str_detect(string = available_layers,
                                                                                pattern = layer)]
                         # Order them according to string length in case we make
                         # a best guess.
                         matched_layers <- matched_layers[order(sapply(X = matched_layers,
                                                                       FUN = nchar))][1]
                         if (length(matched_layers) > 1 & !best_guess) {
                           stop(paste0("Using '", layer, "' as a regular expression matched multiple layers/feature classes in the geodatabase but must only match one if best_guess is FALSE. The following layers were found: ",
                                       paste(matched_layers,
                                             collapse = ", ")))
                         } else if (length(matched_layers) > 1 & !best_guess) {
                           # When making a best guess, this'll use the shortest
                           # layer name
                           if (verbose) {
                             paste0("Using '", layer, "' as a regular expression matched multiple layers/feature classes in the geodatabase. Because best_guess is TRUE, the following will be used: ",
                                    matched_layer)
                           }
                           layer <- matched_layers[1]
                         } else if (length(matched_layers) > 0){
                           layer <- matched_layers[1]
                         }
                       } else if (!(layer %in% available_layers)) {
                         stop(paste0("The geodatabase does not contain a layer/feature class called '", layer, "'. Did you intend to use it as a regular expression with the argument 'regex = TRUE'?"))
                       }

                       if (length(layer) < 1) {
                         if (accept_failure) {
                           if (verbose) {
                             message("Unable to identify a feature class to read in. Accepting failure and returning NULL.")
                           }
                           NULL
                         } else {
                           stop("Unable to identify a feature class to read in. If this is acceptable, set the argument accept_failure to TRUE.")
                         }
                       } else {
                         sf::st_read(dsn = input,
                                     layer = layer,
                                     # Making sure it doesn't complain about tables that
                                     # don't have associated geometry
                                     quiet = !verbose)
                       }
                     },
                     "rdata" = {
                       # RData files are easy peasy.
                       load(file = input,
                            verbose = verbose)
                     },
                     "csv" = {
                       # CSVs are also easy to handle.
                       read.csv(file = input,
                                stringsAsFactors = FALSE)
                     },
                     "rds" = {
                       readRDS(file = input)
                     })
  }

  # And kick the output to the user.
  output
}

# Select the first non-NULL value in the list that meets the requirements.
# This is order-sensitive, so even if multiple possible inputs are valid, the
# one returned will be the one with the lowest index value.
select_source <- function(possible_inputs,
                          valid_input_classes = c("character",
                                                  "data.frame"),
                          valid_file_extensions = c("gdb",
                                                    "csv",
                                                    "rdata")){
  if (!is.list(possible_inputs)) {
    stop("possible_inputs must be a list, even if the list has only one index.")
  }

  input <- possible_inputs[[sapply(X = possible_inputs,
                                   valid_input_classes = valid_input_classes,
                                   valid_file_extensions = valid_file_extensions,
                                   FUN = function(X, valid_input_classes, valid_file_extensions){
                                     # Check the file extension. If it's not
                                     # relevant, we'll just say it has a valid
                                     # one to make it easier in a moment.
                                     if (is.character(X)) {
                                       valid_extension <- tolower(tools::file_ext(x = X)) %in% valid_file_extensions
                                     } else {
                                       valid_extension <- TRUE
                                     }

                                     # Check the class.
                                     valid_class <- class(X) %in% valid_input_classes

                                     # Return whether it was valid overall.
                                     # valid_extension will always be TRUE for
                                     # non-character inputs, which is fine for
                                     # our purposes.
                                     valid_class & valid_extension}) |>
                              # Get the numeric indices of non-NULL values in the
                              # list.
                              which(x = _) |>
                              # Pick the lowest value, i.e. the first non-NULL in
                              # the list.
                              # If they're all NULL, this will return Inf which
                              # breaks the intended use of the [] and produces a
                              # warning.
                              min(. = _) |>
                              # Therefore, in case of Inf, this makes sure that
                              # instead we get an empty vector because the only
                              # valid values are the indices of possible_inputs.
                              intersect(x = _,
                                        y = seq_len(length(possible_inputs)))]] |>
    # This makes sure we don't get a warning message from min() returning Inf
    # because we don't care.
    suppressWarnings(expr = _)

  if (length(input) < 1) {
    stop("None of the provided possible sources were valid.")
  }

  input
}

# Here's the order of operations:
# 1) If tbl is not NULL, try to figure out how to use it
#   A) Check to see if tbl is a data frame. If so, assign it as header and move on. Otherwise try B.
#   B) Check to see if tbl is a character string ending in a file extension. If so, use read_whatever() to assign it to header and move on. Otherwise try C.
#   C) Check to see if tbl is a character string without a file extension. If so AND dsn is a filepath to a GDB, try to use it as a feature class name. Otherwise, throw an error.
# 2) if tbl is NULL, try to use dsn with read_whatever() looking for layer = default_name with regex and best_guess.

read_with_fallback <- function(dsn = NULL,
                              tbl = NULL,
                              default_name = NULL,
                              regex = FALSE,
                              best_guess = FALSE,
                              accept_failure = FALSE,
                              verbose = FALSE){
  #### Reading #################################################################
  # Here's the order of operations:
  # 1) If tbl is not NULL, try to figure out how to use it
  #   A) Check to see if tbl is a data frame. If so, assign it as header and move on. Otherwise try B.
  #   B) Check to see if tbl is a character string ending in a file extension. If so, use read_whatever() to assign it to header and move on. Otherwise try C.
  #   C) Check to see if tbl is a character string without a file extension. If so AND dsn is a filepath to a GDB, try to use it as a feature class name. Otherwise, throw an error.
  # 2) if tbl is NULL, try to use dsn with read_whatever() looking for layer = default_name with regex and best_guess.
  # 3) If no headers can be read in, throw an error.
  # Note that accept_failure doesn't apply to the whole thing, just read_whatever() calls.
  if (!is.null(tbl)) {
    if (is.data.frame(tbl)) {
      output <- tbl
    } else if (is.character(tbl)) {
      tbl_file_extension <- tools::file_ext(x = tbl) |>
        tolower()
      if (nchar(tbl_file_extension) > 0 & !is.null(dsn)) {
        output <- read_whatever(input = tbl,
                                regex = regex,
                                best_guess = best_guess,
                                accept_failure = accept_failure,
                                verbose = verbose)
      } else if (nchar(tbl_file_extension) < 1 & !is.null(dsn)) {
        output <- read_whatever(input = dsn,
                                layer = tbl,
                                regex = regex,
                                best_guess = best_guess,
                                accept_failure = accept_failure,
                                verbose = verbose)
      }
    } else {
      stop("When providing tbl it must be either a data frame or a character string.")
    }
  } else if (!is.null(dsn)) {
    if (!is.null(default_name)) {
      output <- read_whatever(input = dsn,
                              layer = default_name,
                              regex = regex,
                              best_guess = best_guess,
                              accept_failure = accept_failure,
                              verbose = verbose)
    } else {
      stop("When providing dsn but not tbl, default_name is required.")
    }
  } else {
    stop("Provide either tbl or a path to a GDB containing it")
  }
  output
}


lpi_indicator_definitions <- function(){

  # The following objects are values that we'll use to create new variables for
  # use in defining indicators.
  list(
    #### Litter code categories ------------------------------------------------
    litter_codes = list("HerbLitter" = c("HL", "L", "DN", "ER", "AM"),
                        "WoodyLitter" = c("WL"),
                        # AL and OM are older NRI codes and HT is from somewhere.
                        # NonVeg and Emb aren't used for individual indicators
                        # but are part of creating the TotalLitter variable
                        "NonVegLitter" = c("HT", "NL", "AL", "OM"),
                        "EmbLitter" = c("EL")),

    #### Rock codes ------------------------------------------------------------
    rock_codes = c("R", "GR", "CB", "ST", "BY",
                   # These are LMF codes
                   "RF", "BR"),

    #### Between-plant codes ---------------------------------------------------
    # These are for grouping values for between-plant indicators
    # NOTE: IF YOU ADD A NEW CATEGORY DON'T FORGET TO INCLUDE IT IN THE MUTATE()
    # UNDER SANITIZATION/HARMONIZATION BELOW (searching for between_plant_codes
    # will turn it up)
    between_plant_codes = list("WoodyLitter" = litter_codes[["WoodyLitter"]],
                               "HerbLitter" =  litter_codes[["HerbLitter"]],
                               # "NonVegLitter" = litter_codes[["NonVegLitter"]],
                               "EmbLitter" = litter_codes[["EmbLitter"]],
                               "DepSoil" = c("DS"),
                               "Duff" = c("D"),
                               "Lichen" = c("LC"),
                               "VagrLichen" = c("VL"),
                               "Moss" = c("M"),
                               "Cyanobacteria" = c("CY"),
                               "Water" = c("W", "WA"),
                               "Rock" = c(rock_codes),
                               "BareSoil" = c("AG", "CM", "LM", "FG", "PC", "S")),

    #### Pinyon-juniper species codes ------------------------------------------
    pj_identifiers = c("JUCA7",
                       "SACA29",
                       "JUCAS2",
                       "JUCAU",
                       "JUOCU",
                       "JUUT",
                       "SAUT3",
                       "JUCE2",
                       "JUAR3",
                       "JUCOA3",
                       "JUCOA2",
                       "JUDE2",
                       "JUNDEPD",
                       "JUNDEPS",
                       "JUDES",
                       "JUDES2",
                       "JUER",
                       "JUPIE",
                       "JUCO11",
                       "JUCOC2",
                       "JUERC",
                       "JUFL",
                       "JUNFLAF",
                       "SAFL16",
                       "JUGR7",
                       "JUNKNI",
                       "JUKN",
                       "JUMOK",
                       "JUME6",
                       "JUME7",
                       "JUUTM",
                       "JUMOG",
                       "JUNCFMON",
                       "JUNIP",
                       "JUNMEXM",
                       "JUMO",
                       "JUMOM",
                       "JUNOCCM",
                       "SAMO8",
                       "JUDEP",
                       "JUNDEPP2",
                       "JUNPAC",
                       "JUNPAC2",
                       "JUCAO",
                       "JUOS",
                       "JUNTETO",
                       "SAOS",
                       "JUOC",
                       "JUOCO",
                       "SAOC9",
                       "JUOCA2",
                       "JUOCA",
                       "JUNGYM",
                       "JUOCG",
                       "JUMOP",
                       "JUPI",
                       "JUSC2",
                       "JUVIS2",
                       "JUVIS4",
                       "SASC5",
                       "JUSCC2",
                       "JUSCP",
                       "PICA16",
                       "PIMOC2",
                       "PIMOC",
                       "PICA3",
                       "PIREC",
                       "PICE",
                       "PINCEMC",
                       "PINCEMB",
                       "PICEB",
                       "PICER",
                       "PINCULR",
                       "PIRE5",
                       "PICUD",
                       "PIDI3",
                       "CAREDU",
                       "PICEE",
                       "PIED",
                       "PIEDE",
                       "PINMONE",
                       "PINCALF",
                       "PIEDF",
                       "PINFAL",
                       "PIMOF2",
                       "PINMONF",
                       "PIMOF",
                       "APIFLE",
                       "PINCEMF",
                       "PIFL2",
                       "PIFLA",
                       "PIFLA2",
                       "PIFLC",
                       "PIFLC2",
                       "PINCEMJ",
                       "PINCULJ",
                       "PIJO",
                       "PIJU",
                       "PINQUAJ",
                       "CARMON2",
                       "PINCEMM",
                       "PINEDUM",
                       "PIMO",
                       "PIMOM2",
                       "PINCEMP2",
                       "PINPAR",
                       "PINCEMQ",
                       "PIQU"),

    #### Conifer families ------------------------------------------------------
    conifer_identifiers = c("Cupressaceae",
                            "Pinaceae",
                            "Taxaceae"),

    #### Lichen codes ----------------------------------------------------------
    lichen_identifiers = c(Lichen = "LC",
                           Cyanobacteria = "CY",
                           VagrLichen = "VL"),

    #### Biocrust codes --------------------------------------------------------
    biocrust_identifiers = c("CY",
                             "LC",
                             "M"),

    #### Moss definitions ------------------------------------------------------
    # For moss cover, we need to identify species that use irregular unknown codes
    # and species that were keyed out in addition to the traditional "where does
    # 'M' occur as a surface code"
    # This will find codes like "MOSS", "M123", "MOS123", and "MOSS123"
    unknown_moss_regex = "^(M(OS{1,2})?\\d+)|(MOSS)$",
    # In tblNationalPlants there's a variable called HigherTaxon that we can use
    # to identify which species codes are technically mosses. This is helpful
    # mostly for Alaska where they ID mosses to species, but anywhere we don't do
    # it runs the risk of underestimating the amount of moss cover if there are
    # any recorded in the canopy.
    moss_identifiers = "Moss"
  )
}
