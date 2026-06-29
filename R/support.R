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
                                                                       FUN = stringi::stri_length))][1]
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

  # Define the independent objects first so they are in the function's scope
  litter_codes_init = list("HerbLitter" = c("HL", "L", "DN", "ER", "AM"),
                           "WoodyLitter" = c("WL"),
                           "NonVegLitter" = c("HT", "NL", "AL", "OM"),
                           "EmbLitter" = c("EL"))

  rock_codes_init = c("R", "GR", "CB", "ST", "BY", "RF", "BR")

  list(
    #### Litter code categories ------------------------------------------------
    litter_codes = litter_codes_init,

    #### Rock codes ------------------------------------------------------------
    rock_codes = rock_codes_init,

    #### Between-plant codes ---------------------------------------------------
    between_plant_codes = list("WoodyLitter" = litter_codes_init[["WoodyLitter"]],
                               "HerbLitter" =  litter_codes_init[["HerbLitter"]],
                               "EmbLitter" = litter_codes_init[["EmbLitter"]],
                               "DepSoil" = c("DS"),
                               "Duff" = c("D"),
                               "Lichen" = c("LC", "2LICHN", "2LICHN1"),
                               "VagrLichen" = c("VL"),
                               "Moss" = c("M", "2MOSS", "2MOSS1"),
                               "Cyanobacteria" = c("CY"),
                               "Water" = c("W", "WA"),
                               "Rock" = c(rock_codes_init),
                               "BareSoil" = c("AG", "CM", "LM", "FG", "PC", "S")),

    #### Pinyon-juniper species codes ------------------------------------------
    pj_identifiers = c("JUCA7", "SACA29", "JUCAS2", "JUCAU", "JUOCU", "JUUT",
                       "SAUT3", "JUCE2", "JUAR3", "JUCOA3", "JUCOA2", "JUDE2",
                       "JUNDEPD", "JUNDEPS", "JUDES", "JUDES2", "JUER", "JUPIE",
                       "JUCO11", "JUCOC2", "JUERC", "JUFL", "JUNFLAF", "SAFL16",
                       "JUGR7", "JUNKNI", "JUKN", "JUMOK", "JUME6", "JUME7",
                       "JUUTM", "JUMOG", "JUNCFMON", "JUNIP", "JUNMEXM", "JUMO",
                       "JUMOM", "JUNOCCM", "SAMO8", "JUDEP", "JUNDEPP2", "JUNPAC",
                       "JUNPAC2", "JUCAO", "JUOS", "JUNTETO", "SAOS", "JUOC",
                       "JUOCO", "SAOC9", "JUOCA2", "JUOCA", "JUNGYM", "JUOCG",
                       "JUMOP", "JUPI", "JUSC2", "JUVIS2", "JUVIS4", "SASC5",
                       "JUSCC2", "JUSCP", "PICA16", "PIMOC2", "PIMOC", "PICA3",
                       "PIREC", "PICE", "PINCEMC", "PINCEMB", "PICEB", "PICER",
                       "PINCULR", "PIRE5", "PICUD", "PIDI3", "CAREDU", "PICEE",
                       "PIED", "PIEDE", "PINMONE", "PINCALF", "PIEDF", "PINFAL",
                       "PIMOF2", "PINMONF", "PIMOF", "APIFLE", "PINCEMF", "PIFL2",
                       "PIFLA", "PIFLA2", "PIFLC", "PIFLC2", "PINCEMJ", "PINCULJ",
                       "PIJO", "PIJU", "PINQUAJ", "CARMON2", "PINCEMM", "PINEDUM",
                       "PIMO", "PIMOM2", "PINCEMP2", "PINPAR", "PINCEMQ", "PIQU"),

    #### Conifer families ------------------------------------------------------
    conifer_identifiers = c("Cupressaceae", "Pinaceae", "Taxaceae"),

    #### Lichen codes ----------------------------------------------------------
    lichen_identifiers = list(Lichen = c("LC", "2LICHN", "2LICHN1"),
                           Cyanobacteria = "CY",
                           VagrLichen = "VL"),

    #### Biocrust codes --------------------------------------------------------
    biocrust_identifiers = c("CY",
                             "LC", "2LICHN", "2LICHN1",
                             "M","2MOSS", "2MOSS1"),

    #### Moss definitions ------------------------------------------------------
    # For moss cover, we need to identify species that use irregular unknown codes
    # and species that were keyed out in addition to the traditional "where does
    # 'M' occur as a surface code"
    # This will find codes like "MOSS", "M123", "MOS123", and "MOSS123"
    unknown_moss_regex = "^(M(OS{1,2})?\\d+)|(2?MOSS)$",
    # In tblNationalPlants there's a variable called HigherTaxon that we can use
    # to identify which species codes are technically mosses. This is helpful
    # mostly for Alaska where they ID mosses to species, but anywhere we don't do
    # it runs the risk of underestimating the amount of moss cover if there are
    # any recorded in the canopy.
    moss_identifiers = "Moss"
  )
}

# Calculating LPI indicators for the Terrestrial AIM Database with lpi_calc()
# depends (as of early 2026) on reformatting species attributes and deriving or
# adding a few variables to make the process smooth and to control for some
# idiosyncracies in the data.
# This is written to take the attributes (e.g., tblNationalPlants) and modify
# them to fit into the lpi_calc() pipeline.
## Duration
# Any value matching the case-insensitive regex "perennial" becomes "Peren".
# Any value matching the case-insensitive regex "annual|biennial" becomes "Ann".
# NAs become "duration_irrelevant".
# Any other values are left unaltered.
## GrowthHabit
# Any value matching the case-insensitive regex "^non-?woody$" becomes "NonWoody".
# Any value matching the case-insensitive regex "^non-?vascular$" becomes "Nonvascular".
# NAs become "growthhabit_irrelevant".
# Any other values are left unaltered.
## GrowthHabitSub
# Any value matching the case-insensitive regex "forb" becomes "Forb".
# Any value matching the case-insensitive regex "^sub-?shrub$" becomes "SubShrub".
# Any value where the associated GrowthHabit value matches one of the case-insensitive regexes "^non-?vascular$", "^moss$", or "^lichen$" becomes "growthhabitsub_irrelevant".
# NAs become "growthhabitsub_irrelevant".
# All other values are left unaltered.
## Plant
# New variable.
# This is a new variable not expected in the input species attributes and is
# used to calculate basal cover by plants, total foliar cover, and making sure
# plant properties aren't assigned to non-plant records in other variables that
# are made/modified by this function.
# The value of this variable will be "Plant" only if ALL of the following
# criteria are true, otherwise this value with be NA.
#   1) The associated value in GrowthHabit is NOT "growthhabit_irrelevant".
#   2) The associated value in GrowthHabit is NOT NA.
#   3) The associated value in code consists of three or more characters. As of
#        January 2026, this is still a difference between valid species codes
#        and all other types of values found in code.
## ShrubSucculent
# New variable.
# This is added to make it easy to calculate indicators specific to shrubs,
# subshrubs, and succulents.
# The value of this variable will be "ShrubSucculent" if the associated value in GrowthHabitSub matches the case-insensitive regex "shrub|succulent".
# Otherwise, this value will be NA.
## C3
# New variable.
# This is to enable the calculation of C3 photosynthetic pathway indicators
# because a single record may contain both "C3" and "C4" in the Photosynthesis
# variable.
# The value of this variable will be "C3" if the associated value in Photosynthesis matches the regex "C3".
# Otherwise, the value will be NA.
## C4
# New variable.
# This is to enable the calculation of C4 photosynthetic pathway indicators
# because a single record may contain both "C3" and "C4" in the Photosynthesis
# variable.
# The value of this variable will be "C4" if the associated value in Photosynthesis matches the regex "C4".
# Otherwise, the value will be NA.
#' Harmonize species attributes with TerrADat needs
#' @export
adjust_species_attributes <- function(data,
                                      fail_on_missing = FALSE,
                                      verbose = FALSE){

  # This is a list of all the various bits of definitions for modifying the
  # species attributes in accordance with AIM definitions
  definitions_list <- lpi_indicator_definitions()

  if (verbose) {
    message("Harmonizing species characteristics with AIM indicator needs.")
  }

  # Let's check for the required variables for all these.
  # If any are missing, we can warn the user that those variables will be
  # created but populated with NA and so no indicators that involve them will
  # be calculated.
  expected_variables <- c("GrowthHabit",
                          "GrowthHabitSub",
                          "Duration",
                          "Family",
                          "HigherTaxon",
                          "Nonnative",
                          "Invasive",
                          "Noxious",
                          "SpecialStatus",
                          "Photosynthesis",
                          "PJ",
                          "chckbox")

  missing_expected_variables <- setdiff(x = expected_variables,
                                        names(data))

  if (length(missing_expected_variables) > 0) {
    if (fail_on_missing) {
      stop(paste0("The provided species information does not contain all expected variables required for the standard set of indicators. Set fail_on_missing = FALSE to skip indicators which cannot be calculated. The variables in question are: ",
                  paste(missing_expected_variables,
                        collapse = ", ")))
    }
    warning(paste0("The provided species information does not contain all expected variables required for the standard set of indicators. Indicators which depend on those variables will not be calculated. The variables in question are: ",
                   paste(missing_expected_variables,
                         collapse = ", ")))
    # This makes a new data frame without any data in it consisting of only the
    # missing variables and a number of rows equal to the number of lpi_species
    # records then binds them together.
    data <- matrix(nrow = nrow(data),
                   ncol = length(missing_expected_variables)) |>
      as.data.frame() |>
      setNames(object = _,
               nm = missing_expected_variables) |>
      dplyr::bind_cols(data,
                       .x = _)
  }

  #### Live ----------
  if (all(c("chckbox") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          Live = dplyr::case_when(chckbox %in% c(0, "0") ~ "Live",
                                                  # chckbox %in% c("1") ~ "Dead",
                                                  .default = NA)
    )
  }

  #### Duration ----------
  if (all(c("Duration") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          Duration = dplyr::case_when(grepl(x = Duration,
                                                            pattern = "perennial",
                                                            ignore.case = TRUE) ~ "Peren",
                                                      grepl(x = Duration,
                                                            pattern = "(annual)|(biennial)",
                                                            ignore.case = TRUE) ~ "Ann",
                                                      is.na(Duration) ~ "duration_irrelevant",
                                                      .default = Duration)
    )
  }

  #### GrowthHabit ------------
  if (all(c("GrowthHabit") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          GrowthHabit = dplyr::case_when(grepl(x = GrowthHabit,
                                                               pattern = "^non-?woody$",
                                                               ignore.case = TRUE) ~ "NonWoody",
                                                         grepl(x = GrowthHabitSub,
                                                               pattern = "^non-?vascular$",
                                                               ignore.case = TRUE) ~ "Nonvascular",
                                                         # This removes sedges from consideration???
                                                         # Maybe an artifact of trying to avoid spitting
                                                         # out unused indicators
                                                         # GrowthHabitSub == "Sedge" ~ "growthhabit_irrelevant",
                                                         # For first-hit calculations
                                                         # is.na(GrowthHabit) ~ "growthhabit_irrelevant",
                                                         .default = GrowthHabit)
    )
  }

  #### GrowthHabitSub -----------
  if (all(c("GrowthHabitSub") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          GrowthHabitSub = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                  pattern = "forb",
                                                                  ignore.case = TRUE) ~ "Forb",
                                                            grepl(x = GrowthHabitSub,
                                                                  pattern = "^sub-?shrub$",
                                                                  ignore.case = TRUE) ~ "SubShrub",
                                                            # Not sure why we're removing non-vasculars??
                                                            # Maybe an artifact of trying to avoid spitting
                                                            # out unused indicators. Blame Alaska.
                                                            grepl(x = GrowthHabitSub,
                                                                  pattern = "^non-?vascular$",
                                                                  ignore.case = TRUE) ~ "growthhabitsub_irrelevant",
                                                            # Anyway, doing the exact same to moss
                                                            grepl(x = GrowthHabitSub,
                                                                  pattern = "^moss$",
                                                                  ignore.case = TRUE) ~ "growthhabitsub_irrelevant",
                                                            # And to lichen
                                                            grepl(x = GrowthHabitSub,
                                                                  pattern = "^lichen$",
                                                                  ignore.case = TRUE) ~ "growthhabitsub_irrelevant",
                                                            # For first-hit calculations
                                                            # is.na(GrowthHabit) ~ "growthhabitsub_irrelevant",
                                                            .default = GrowthHabitSub)
    )
  }

  #### Plant --------------
  if (all(c("GrowthHabitSub", "code") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          # Because there are species attribute records where
                          # there are not assigned GrowthHabit or GrowthHabitSub
                          # values, we define this negatively against nonvasculars
                          # to try to keep it to just vascular plants.
                          # Previously we experimented with rejecting NA values
                          # but that dropped records we needed.
                          # Plant = dplyr::case_when(!(GrowthHabit %in% c("growthhabit_irrelevant",
                          #                                               NA)) & nchar(code) >= 3 ~ "Plant",
                          #                          .default = NA)
                          Plant = dplyr::case_when(!(GrowthHabitSub %in% c("growthhabitsub_irrelevant")) &
                                                     GrowthHabit != "Nonvascular",
                                                   stringi::stri_length(code) >= 3 ~ "Plant",
                                                   .default = NA)
    )
  }

  #### ShrubSucculent ---------------------------
  if (all(c("GrowthHabitSub") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          ShrubSucculent = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                  pattern = "shrub|succulent",
                                                                  ignore.case = TRUE) ~ "ShrubSucculent",
                                                            .default = NA)
    )
  }


  if (all(c("code") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          #### Litter ---------------------
                          Litter = dplyr::case_when(
                            code %in% definitions_list[["litter_codes"]][["HerbLitter"]] ~ "HerbLitter",
                            code %in% definitions_list[["litter_codes"]][["WoodyLitter"]] ~ "WoodyLitter",
                            # Fixed the line below to point to definitions_list
                            code %in% definitions_list[["litter_codes"]][["EmbLitter"]] ~ "EmbLitter",
                            .default = "litter_irrelevant"),
                          #### TotalLitter ---------------------
                          TotalLitter = dplyr::case_when(code %in% unlist(definitions_list[["litter_codes"]]) ~ "TotalLitter",
                                                         .default = "total_litter_irrelevant"),
                          #### Biocrust ----------------
                          Biocrust = dplyr::case_when(code %in% definitions_list[["biocrust_identifiers"]] ~ "Biocrust",
                                                      .default = NA),
                          #### Lichen -----------------------------------
                          Lichen = dplyr::case_when(code %in% definitions_list[["lichen_identifiers"]][["Lichen"]] ~ "Lichen",
                                                    code %in% definitions_list[["lichen_identifiers"]][["VagrLichen"]] ~ "VagrLichen",
                                                    code %in% definitions_list[["lichen_identifiers"]][["Cyanobacteria"]] ~ "Cyanobacteria",
                                                    .default = "lichen_irrelevant"),
                          #### PJ ---------------------------------------
                          # PJ = dplyr::case_when(code %in% definitions_list[["pj_identifiers"]] ~ "PJ",
                          #                       .default = NA),
                          #### Rock -------------------------------------
                          Rock = dplyr::case_when(code %in% definitions_list[["rock_codes"]] ~ "Rock",
                                                  .default = NA),
                          ###### Duff -------------------------------------

                          Duff = dplyr::case_when(code == "D" ~ "Duff",

                                                  .default = NA),



                          ###### Water ------------------------------------

                          Water = dplyr::case_when(code %in% c("W", "WA") ~ "Water",

                                                   .default = "water_irrelevant"),
                          #### AdditionalRemoteSensing ----------------
                          AdditionalRemoteSensing = dplyr::case_when(code %in% c("DS") ~ "DepSoil",
                                                                     .default = "remote_sensing_irrelevant")
    )
  }

  #### PJ --------------
  if (all(c("PJ") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          PJ = dplyr::case_when(PJ %in% c(TRUE) ~ "PJ",
                                                .default = NA)
    )
  } else if (all(c("code") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          PJ = dplyr::case_when(code %in% definitions_list[["pj_identifiers"]] ~ "PJ",
                                                .default = NA)
    )
  }

  #### C3/C4 ---------------------
  if (all(c("Photosynthesis") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          C3 = dplyr::case_when(grepl(x = Photosynthesis,
                                                      pattern = "C3") ~ "C3",
                                                .default = NA),
                          C4 = dplyr::case_when(grepl(x = Photosynthesis,
                                                      pattern = "C4") ~ "C4",
                                                .default = NA)
    )
  }

  if (all(c("Family") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          #### Grass ------------
                          Grass = dplyr::case_when(Family %in% c("Poaceae") ~ "Grass",
                                                   .default = NA),
                          #### Conifer -------------------
                          Conifer = dplyr::case_when(Family %in% definitions_list[["conifer_identifiers"]] ~ "Conifer",
                                                     .default = NA)
    )
  }


  if (all(c("GrowthHabitSub", "Family") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          #### Forb/Graminoid ---------------------
                          ForbGraminoid = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                                 pattern = "(^((graminoid)|(grass))$)|forb",
                                                                 ignore.case = TRUE) ~ "ForbGraminoid",
                                                           .default = NA),
                          #### Forb/Grass ---------------------
                          ForbGrass = dplyr::case_when(grepl(x = GrowthHabitSub,
                                                             pattern = "forb",
                                                             ignore.case = TRUE) | Family %in% "Poaceae" ~ "ForbGrass",
                                                       .default = NA)
    )
  }


  #### SG_Group ---------------------
  if (all(c("SG_Group", "SpeciesState", "GrowthHabitSub") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          # This is to turn the SG_Group codes into values
                          # that match the expected indicator names for
                          # our convenience.
                          SG_Group = stringr::str_replace_all(string = SG_Group,
                                                              pattern = "StaturePerennialGrass",
                                                              replacement = "PerenGrass"),
                          # This makes sure that the value in SG_Group is
                          # only the string associated with the group for
                          # the species code in the relevant state.
                          # Records where there's not a group value for the
                          # associated state (or "US") will get NA instead.
                          SG_Group = stringr::str_extract(string = SG_Group,
                                                          pattern = paste0("(?<=((US)|(", SpeciesState, ")):)[A-z]+")),
                          # This makes sure that we've assigned any shrubs
                          # that didn't get a sage-grouse group are
                          # assigned to "NonSagebrushShrub"
                          SG_Group = dplyr::case_when(is.na(SG_Group) & GrowthHabitSub == "Shrub" ~ "NonSagebrushShrub",
                                                      # So that first-hit calcs work as intended.
                                                      is.na(SG_Group) & GrowthHabitSub != "Shrub" ~ "Irrelevant",
                                                      .default = SG_Group)
    )
  }

  if (all(c("code", "HigherTaxon") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          #### Moss ---------------------
                          Moss = dplyr::case_when(HigherTaxon %in% definitions_list[["moss_identifiers"]] |
                                                    stringr::str_detect(string = code,
                                                                        pattern = definitions_list[["unknown_moss_regex"]]) |
                                                    code %in% c("M") ~ "Moss",
                                                  # We're not keeping an indicator calculated for
                                                  # "Nonmoss" but we do need that info for first hits to work
                                                  .default = "moss_irrelevant"),
                          #### between_plant ----------------------------
                          between_plant = dplyr::case_when(code %in% definitions_list[["between_plant_codes"]][["WoodyLitter"]] ~ "WoodyLitter",
                                                           code %in% definitions_list[["between_plant_codes"]][["HerbLitter"]] ~ "HerbLitter",
                                                           # code %in% definitions_list[["between_plant_codes"]][["NonVegLitter"]] ~ "NonVegLitter",
                                                           code %in% definitions_list[["between_plant_codes"]][["EmbLitter"]] ~ "EmbLitter",
                                                           code %in% definitions_list[["between_plant_codes"]][["DepSoil"]] ~ "DepSoil",
                                                           code %in% definitions_list[["between_plant_codes"]][["Duff"]] ~ "Duff",
                                                           code %in% definitions_list[["between_plant_codes"]][["Lichen"]] ~ "Lichen",
                                                           HigherTaxon %in% definitions_list[["moss_identifiers"]] |
                                                             stringr::str_detect(string = code,
                                                                                 pattern = definitions_list[["unknown_moss_regex"]]) |
                                                             code %in% definitions_list[["between_plant_codes"]][["Moss"]] ~ "Moss",
                                                           code %in% definitions_list[["between_plant_codes"]][["Cyanobacteria"]] ~ "Cyanobacteria",
                                                           code %in% definitions_list[["between_plant_codes"]][["Water"]] ~ "Water",
                                                           code %in% definitions_list[["between_plant_codes"]][["Rock"]] ~ "Rock",
                                                           code %in% definitions_list[["between_plant_codes"]][["VagrLichen"]] ~ "VagrLichen",
                                                           code %in% definitions_list[["between_plant_codes"]][["BareSoil"]] ~ "BareSoil",
                                                           .default = "between_plant_irrelevant")
    )
  }

  #### Native ---------------------
  if (all(c("Nonnative", "Plant") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          # This is for the native and non-native cover
                          # It assumes that everything flagged as EXOTIC or
                          # ABSENT should be considered NonNative and that
                          # everything else is Native
                          Native = dplyr::case_when(Nonnative %in% c("NATIVE", "native", "Native", NA) &
                                                      !is.na(Plant) ~ "Native",
                                                    !(Nonnative %in% c("NATIVE", "native", "Native", NA)) &
                                                      !is.na(Plant) ~ "NonNative",
                                                    .default = NA)
    )
  }

  #### Invasive ---------------------
  if (all(c("Invasive") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          # This is just to make the Invasive values match
                          # the desired indicator names.
                          Invasive = stringr::str_to_title(string = Invasive) |>
                            tidyr::replace_na(data = _,
                                              replace = "NonInv")
    )
  }

  #### Noxious ----------------------------------
  # For noxious cover. This previously assumed that
  # anything flagged as YES is noxious and nothing
  # else is. This is now disabled because noxious
  # status is being handled more appropriately and
  # through a different format. I'm leaving this
  # for posterity for the moment though.
  # Noxious = dplyr::case_when(Noxious %in% c("YES") ~ "Noxious",
  #                            .default = NA),
  #
  # Noxious is now encoded as a character string
  # with localities separated by |s. We need to
  # check for the relevant locality based on the
  # State variable NOT the AdminState because these
  # determinations are made based on the physical
  # location of the sampling within the legal
  # boundaries of states, not which state is
  # administering the lands (which is sometimes
  # different).
  # The regex checks to see if the beginning of
  # the string or the characters immediately
  # following a | are "US", the code from the State
  # variable, or the code from the State variable
  # and the value from the County variable
  # separated by a :, e.g., "OR:Jefferson".
  # The single-letter designations for type of
  # noxiousness are not taken into account, e.g.,
  # "OR:A" and "OR:B" will be treated identically.
  # County-level designations may eventually be
  # removed, but for now they're still in there and
  # this regex will work regardless.
  if (all(c("Noxious", "SpeciesState") %in% names(data))) {
    data <- dplyr::mutate(.data = data,
                          Noxious = dplyr::case_when(stringr::str_detect(string = Noxious,
                                                                         pattern = paste0("(^|\\|)((", SpeciesState, ")|(US))")) ~ "Noxious",
                                                     .default = "noxious_irrelevant")
    )
  }

  # These are all variables that this function intends to modify or create
  target_vars <- c("Duration", "GrowthHabit", "GrowthHabitSub", "Plant", "ShrubSucculent", "Litter", "TotalLitter", "Biocrust", "Lichen", "PJ", "Rock", "Duff", "Water", "AdditionalRemoteSensing", "C3", "C4", "Grass", "Conifer", "ForbGraminoid", "ForbGrass", "SG_Group", "Moss", "between_plant", "Native", "Invasive", "Noxious", "Live")

  missed_vars <- setdiff(x = target_vars,
                         y = names(data))
  if (length(missed_vars) > 0) {
    warning(paste0("The following variables were not modified or created due to missing dependency variables: ",
                   paste(missed_vars,
                         collapse = ", ")))
  }

  data
}

# These are the indicator groupings for producing the TerrADat indicators from
# the output from adjust_species_attributes()
default_indicators_vars <- function(source,
                                    hit = c("any", "first", "basal"),
                                    verbose = FALSE){

  valid_sources <- c("terradat", "ldc")
  source <- unique(source) |>
    toupper()
  if (length(source) > 1 | !all(source %in% valid_sources)) {
    stop(paste0("source must be one of the following values: '",
                paste(valid_sources,
                      collapse = "', '"), "'"))
  }

  valid_hits <- c("any", "first", "basal")
  if (!all(hit %in% valid_hits)) {
    stop("Valid values for hit are: '",
         paste(valid_hits,
               collapse = "', '"), "'")
  }

  groupings_lists <- list(
    terradat = list(
      first = list(c("Duration", "GrowthHabitSub"),
                   c("Duration", "ForbGraminoid"),
                   c("GrowthHabitSub"),
                   c("SG_Group"),
                   c("Noxious", "Duration", "GrowthHabitSub"),
                   c("between_plant"),
                   c("Litter"),
                   c("Lichen"),
                   c("TotalLitter"),
                   c("Moss"),
                   c("Duff"),
                   c("Water")),
      any = list(c("Plant"),
                 c("GrowthHabit"),
                 c("GrowthHabitSub"),
                 c("Duration", "GrowthHabit"),
                 c("Duration", "GrowthHabitSub"),
                 c("Duration", "ForbGraminoid"),
                 c("ShrubSucculent"),
                 c("Noxious"),
                 c("Litter"),
                 c("TotalLitter"),
                 c("SG_Group"),
                 c("SG_Group", "Live"),
                 c("Grass"),
                 c("Duration", "Grass"),
                 c("C3", "Duration", "Grass"),
                 c("C4", "Duration", "Grass"),
                 c("Native"),
                 c("Invasive"),
                 c("Invasive", "Duration", "GrowthHabitSub"),
                 c("Invasive", "Duration", "ShrubSucculent"),
                 c("Invasive", "Duration", "Grass"),
                 c("Invasive", "Duration", "ForbGrass"),
                 c("Conifer"),
                 c("PJ"),
                 c("Moss"),
                 c("Rock"),
                 c("Biocrust"),
                 c("Lichen"),
                 c("Duff"),
                 c("Water")),
      basal = list(c("Duration", "Grass"),
                   c("Plant"))
    ),
    ldc = list(any = c(),
               first = c(),
               basal = c()))

  groupings_lists[[source]][hit]
}

default_lpi_indicators <- function(source,
                                   lookup = FALSE){

  valid_sources <- c("terradat", "ldc")
  source <- unique(source) |>
    toupper()
  if (length(source) > 1 | !all(source %in% valid_sources)) {
    stop(paste0("source must be one of the following values: '",
                paste(valid_sources,
                      collapse = "', '"), "'"))
  }

  indicators_vectors <- list(
    terradat = c("TotalFoliarCover" = "AH_PlantCover",
                 "BareSoilCover" = "FH_BareSoilCover",
                 "AH_ForbCover",
                 "AH_PerenForbCover",
                 "AH_AnnForbCover",
                 "AH_PreferredForbCover",
                 "AH_GrassCover",
                 "AH_GraminoidCover",
                 "AH_PerenGrassCover",
                 "AH_PerenGraminoidCover",
                 "AH_C3PerenGrassCover",
                 "AH_C4PerenGrassCover",
                 "AH_AnnGrassCover",
                 "AH_AnnGraminoidCover",
                 "AH_TallPerenGrassCover",
                 "AH_ShortPerenGrassCover",
                 "AH_PerenForbGraminoidCover",
                 "AH_AnnForbGraminoidCover",
                 "AH_ShrubCover",
                 "AH_ShrubSucculentCover",
                 "AH_TreeCover",
                 "AH_SubShrubCover",
                 "AH_SagebrushCover",
                 "AH_SagebrushCover_Live",
                 "AH_NonSagebrushShrubCover",
                 "AH_TotalLitterCover",
                 "AH_WoodyLitterCover",
                 "AH_HerbLitterCover",
                 "AH_DuffCover",
                 "AH_VagrLichenCover",
                 "AH_LichenCover",
                 "AH_MossCover",
                 "AH_CyanobacteriaCover",
                 "AH_RockCover",
                 "AH_EmbLitterCover",
                 "AH_WaterCover",
                 "AH_InvasiveCover",
                 "AH_InvasivePerenForbCover",
                 "AH_InvasiveAnnForbCover",
                 "AH_InvasivePerenGrassCover",
                 "AH_InvasiveAnnGrassCover",
                 "AH_InvasivePerenForbGrassCover",
                 "AH_InvasiveAnnForbGrassCover",
                 "AH_InvasiveShrubCover",
                 "AH_InvasiveSubShrubCover",
                 "AH_InvasiveSucculentCover",
                 "AH_InvasiveTreeCover",
                 "AH_NonInvPerenForbCover",
                 "AH_NonInvAnnForbCover",
                 "AH_NonInvPerenGrassCover",
                 "AH_NonInvAnnGrassCover",
                 "AH_NonInvPerenForbGrassCover",
                 "AH_NonInvAnnForbGrassCover",
                 "AH_NonInvShrubCover",
                 "AH_NonInvSubShrubCover",
                 "AH_NonInvSucculentCover",
                 "AH_NonInvTreeCover",
                 "AH_NativeCover",
                 "AH_NonNativeCover",
                 "AH_NoxiousCover",
                 "AH_PJCover",
                 "AH_ConiferCover",
                 "AH_BasalCover" = "AH_BasalPlantCover",
                 "AH_BasalPerenGrassCover",
                 "AH_BiocrustCover",
                 "FH_TotalLitterCover",
                 "FH_WoodyLitterCover",
                 "FH_HerbLitterCover",
                 "FH_DuffCover",
                 "FH_VagrLichenCover",
                 "FH_LichenCover",
                 "FH_MossCover",
                 "FH_CyanobacteriaCover",
                 "FH_RockCover",
                 "FH_EmbLitterCover",
                 "FH_WaterCover",
                 "FH_DepSoilCover",
                 "FH_ForbCover",
                 "FH_PerenForbCover",
                 "FH_AnnForbCover",
                 "FH_GraminoidCover",
                 "FH_AnnGraminoidCover",
                 "FH_PerenGraminoidCover",
                 "FH_PerenForbGraminoidCover",
                 "FH_ShrubCover",
                 "FH_SagebrushCover",
                 "FH_NonSagebrushShrubCover",
                 "FH_TreeCover",
                 "SagebrushShape_All_ColumnCount",
                 "SagebrushShape_All_SpreadCount",
                 "SagebrushShape_All_Predominant"),
    ldc = c()
  )

  output <- indicators_vectors[source]

  # If the user doesn't want this as a lookup for something like tidyselect,
  # this replaces the values in the vectors with the names where there are names
  if (!lookup) {
    output[where(!is.na(names(output)))] <- purrr::discard(.x = names(output),
                                                           .p = is.na)
  }

  output
}
