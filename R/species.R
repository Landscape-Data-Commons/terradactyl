species_read_aim <- function(dsn,
                             verbose = FALSE) {
  #### Validity checks #########################################################
  if (!is.character(dsn)) {
    stop("dsn must be a character string specifying the filepath to a geodatabase containing tables called 'tblNationalPlants' and 'tblStateSpecies'.")
  }
  if (!file.exists(dsn)) {
    stop("dsn must be a character string specifying the filepath to a geodatabase containing tables called 'tblNationalPlants' and 'tblStateSpecies'.")
  }
  if (!(tools::file_ext(dsn) %in% c("GDB", "gdb"))) {
    stop("dsn must be a character string specifying the filepath to a geodatabase containing tables called 'tblNationalPlants' and 'tblStateSpecies'.")
  }
  required_tables <- c("tblNationalPlants",
                       "tblStateSpecies")
  available_layers <- sf::st_layers(dsn = dsn)$name
  missing_tables <- setdiff(x = required_tables,
                            y = available_layers)
  if (length(missing_tables) > 0) {
    stop(paste0("The following tables are required but do not exist in the specified geodatabase: ",
                paste(missing_tables,
                      collapse = ", ")))
  }

  #### Reading #################################################################
  # This is way more complicated now that we're working with tblNationalPlants
  # AND tblStateSpecies. This combines them for use in species_join().
  # First, we grab tblNationalPlants and tblStateSpecies.
  # Then we discard everything from tblStateSpecies except the variables
  # containing codes, the states, and the sage-grouse groups.
  # We summarize tblStateSpecies to get a data frame of codes with a variable
  # called SG_Group that contains the sage-grouse species info by state as a
  # series of pipe-separated values in a character string.
  # We finish up by joining the new sage-grouse data frame to tblNationalPlants
  # using CurrentPLANTSCode (*NOT* NameCode) and SpeciesCode.

  if (verbose) {
    message("Reading in tblNationalPlants.")
  }

  tblNationalPlants <- sf::st_read(dsn = dsn,
                                   layer = "tblNationalPlants",
                                   quiet = TRUE)
  if (verbose) {
    message("Reading in tblStateSpecies and creating state sage-grouse lookup table.")
  }

  tblStateSpecies <- sf::st_read(dsn = dsn,
                                 layer = "tblStateSpecies",
                                 quiet = TRUE) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(c(code = "SpeciesCode",
                                       "SG_Group",
                                       "SpeciesState"))) |>
    dplyr::distinct()

  #### Munging #################################################################
  # We'll take the SpeciesState and SG_Group variables from tblStateSpecies to
  # make a new data frame where there's only one record per species code and
  # we store all the per-state SG_Group assignments in a character string as
  # pipe-separated values, e.g. "NM:PreferredForb|OR:PreferredForb".
  # This should be significantly faster than trying to join by both the species
  # codes and SpeciesState, at least for very large data sets.
  sg_group_lookup <- dplyr::select(.data = tblStateSpecies,
                                   tidyselect::all_of(c("CurrentPLANTSCode" = "code",
                                                        "SpeciesState",
                                                        "SG_Group"))) |>
    dplyr::filter(.data = _,
                  !is.na(SG_Group)) |>
    dplyr::mutate(.data = _,
                  sg_string = paste(SpeciesState,
                                    SG_Group,
                                    sep = ":")) |>
    dplyr::summarize(.data = _,
                     .by = Species,
                     SG_Group = paste(sg_string,
                                      collapse = "|"))

  if (verbose) {
    message("Adding SG_Group from tblStateSpecies to tblNationalPlants")
  }

  output <- dplyr::left_join(x = tblNationalPlants,
                             y = tblStateSpecies,
                             relationship = "many-to-one",
                             by = "CurrentPLANTSCode") |>
    # This is so that we have a variable that can be easily used internally
    # because other sources use this variable name mostly.
    dplyr::mutate(.data = _,
                  SpeciesCode = NameCode)


  output
}
#' @export generic_growth_habits
#' @rdname species

# Attribute generic species growth habits, for now this assumes field names.
generic_growth_habits <- function(data,
                                  data_code = "code",
                                  species_list,
                                  species_code = "SpeciesCode",
                                  species_growthhabit_code = "GrowthHabit",
                                  species_growthhabitsub_code = "GrowthHabitSub",
                                  species_duration = "Duration",
                                  verbose = FALSE) {
  #### Setup ###################################################################
  ### Regexes for unknown plant properties
  # These are used to assign growth habit, growth habit sub, and duration for
  # unknown plants. They're broken into LMF and AIM because the generic code
  # formats differ between the two.

  # Growth habit
  # The commented-out versions capture litter, which seems wrong but was the
  # evident previous behavior.
  lmf_growthhabit_regexes <- c("Woody" = "^2(S(?!LIME)|T|[GV]W)",
                               "NonWoody" = "^2((F(?!SMUT|[FJRU]))|(G(?!W)|(VH)))",
                               "Nonvascular" = "^2(A|BRY|HORN|L(?!TR)|LTRL|MOSS|PROT|SLIME)",
                               "Other" = "^2(PLANT|BACT|CYAN|DIAT|DINO|(F(?=SMUT|[FJRU])|(LTR(?!L))))")
  aim_growthhabit_regexes <- c("Woody" = "^((S[HU]|TR)\\d{1,999})|(SSHH|TTRR|SSUU)",
                               "NonWoody" = "^(([AP]{1,2}[FG]{1,2})|(PPSS))\\d{1,999}$",
                               "Nonvascular" = "^(VL|CY|LC|M)$")

  # Growth habit sub regexes
  lmf_growthhabitsub_regexes <- c("Forb" = "^2F(?![FJRSU])",
                                  "Tree" = "^2T(?!S)",
                                  "SubShrub" = "^2S[SU](?![SL$])",
                                  "Shrub" = "^2((S[BDEHN])|(S$))",
                                  "Graminoid" = "^2G",
                                  "Succulent" = "^2(FS(?!(MUT)|(UNGI))|(SS(?![BDEN]))|(TS))")
  aim_growthhabitsub_regexes <- c("Forb" = "^[AP]{1,2}F{1,2}\\d{1,999}$",
                                  "Tree" = "^TR\\d{1,999}$",
                                  "Shrub" = "^((SH)|(SSHH))\\d{1,999}$",
                                  "Graminoid" = "^[AP]{1,2}G{1,2}\\d{1,999}$",
                                  "Succulent" = "^SU\\d{1,999}$")

  # Duration regexes
  lmf_duration_regexes <- c("Annual" = "^2(([FG]|VH)[DMSL]?[AB])$",
                            "Perennial" = "^2((F[DMS]?P)|(GL?[PN])|(GRAM)|(S(?!LIME).*)|(T.*)|(VH[DMS]?P)|(VW.*))$")
  aim_duration_regexes <- c("Annual" = "^A{1,2}[FG]{1,2}\\d{1,999}",
                            "Perennial" = "^(((P{1,2}[FG]{1,2})|(TR)|(PPSS))\\d{1,999})|(S{1,2}[HU]{1,2})")

  regexes_list <- list("GrowthHabit" = c(lmf_growthhabit_regexes,
                                         aim_growthhabit_regexes),
                       "GrowthHabitSub" = c(lmf_growthhabitsub_regexes,
                                            aim_growthhabitsub_regexes),
                       "Duration" = c(lmf_duration_regexes,
                                      aim_duration_regexes))

  #### Validity checks #########################################################
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  if (!(data_code %in% names(data))) {
    stop(paste0("The variable '", data_code, "' (specified with the argument data_code) does not appear in the provided data."))
  }

  if (!is.data.frame(species_list)) {
    stop("data must be a data frame.")
  }
  required_species_variables <- c(species_code = species_code,
                                  species_growthhabit_code = species_growthhabit_code,
                                  species_growthhabitsub_code = species_growthhabitsub_code)
  missing_species_list_variables <- setdiff(x = required_species_variables,
                                            y = names(species_list))
  if (length(missing_species_list_variables) > 0) {
    missing_required_variable_arguments <- names(required_species_variables)[!(required_species_variables %in% names(species_list))]
    bad_variables_string <- sapply(X = missing_required_variable_arguments,
                                   required_species_variables = required_species_variables,
                                   FUN = function(X, required_species_variables){
                                     paste0(required_species_variables[X], " (specified with the argument ", X, ")")
                                   }) |>
      paste(. = _,
            collapse = ", ")
    stop(paste0("The following expected variables are missing from species_list: ", bad_variables_string))
  }

  # Which codes in the data aren't represented in the species_list provided?
  # These are the codes that we'll attempt to interpret as generics.
  missing_codes_df <- dplyr::select(.data = data,
                                    tidyselect::all_of(x = c("code" = data_code))) |>
    dplyr::distinct(.data = _) |>
    dplyr::filter(.data = _,
                  !(code %in% species_list[[species_code]]),
                  !is.na(code)) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(x = setNames(object = "code",
                                                  nm = species_code)))


  #### Apply regexes ###########################################################
  if (verbose) {
    message("Checking for standardized unknown plant codes.")
  }

  # It's tough to apply this str_detect() approach with mutate() so instead this
  # is an old-school sapply() that grabs the name of the regex that was detected
  # but it'll freak out if there are multiple regex hits. Luckily that shouldn't
  # happen unless I've screwed up the regexes themselves.
  for (variable in names(regexes_list)) {
    missing_codes_df[[variable]] <- sapply(X = missing_codes_df[[species_code]],
                                           regexes = regexes_list[[variable]],
                                           FUN = function(X, regexes){
                                             output <- names(regexes)[sapply(X = regexes,
                                                                             string = X,
                                                                             FUN = stringr::str_detect)] |>
                                               unique()

                                             if (length(output) < 1) {
                                               output <- NA
                                             } else if (length(output) > 1) {
                                               stop(paste0("The species code ", X, " matches multiple regexes: '",
                                                           paste(regexes[output], collapse = "', '"),
                                                           "'"))
                                             }

                                             output
                                           }) |> unlist()
  }

  # Remove records where we didn't actually add any information. So, we'll keep
  # only records where at least one of the regexed variables wasn't NA.
  missing_codes_df <- dplyr::filter(.data = missing_codes_df,
                                    dplyr::if_any(.cols = tidyselect::all_of(x = names(regexes_list)),
                                                  .fns = ~ !is.na(x = .x))) |>
    dplyr::distinct()


  # Append the generic codes to the species list!
  dplyr::bind_rows(species_list,
                   missing_codes_df |>
                     dplyr::mutate(.data = _,
                                   dplyr::across(.cols = dplyr::where(fn = is.logical),
                                                 .fns = as.character))) |>
    dplyr::distinct()
}

#' @export species_join
#' @rdname species


# Join species with field data
species_join <- function(data, # field data,
                         data_code = "code", # Species field in the data
                         species_file, # path to .csv or .gdb holding  the species table
                         species_layer = "tblNationalPlants",
                         species_code = "NameCode", # field name in species file that identifies the species code
                         species_growth_habit_code = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
                         species_duration = "Duration", # field name in species file of the Duration assignment
                         species_property_vars = c("GrowthHabit",
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
                                                   "CurrentPLANTSCode"),
                         growth_habit_file = "", # path to .csv or gdb holding tblSpeciesGrowthHabit
                         growth_habit_code = "Code",
                         overwrite_generic_species = FALSE,
                         generic_species_file = "",
                         update_species_codes = FALSE,
                         by_species_key = FALSE,
                         check_species = FALSE,
                         verbose = FALSE) {
  #### Validity checks #########################################################


  #### Cleanup #################################################################
  # Some projects use "None" to indicate "No species". Convert those to "N"
  # instead.
  data <- dplyr::mutate(.data = data,
                        dplyr::across(.cols = tidyselect::all_of(data_code),
                                      .fns = ~ stringr::str_replace(string = .x,
                                                                    pattern = "None",
                                                                    replacement = "N")))

  # Pull in the species list from whatever source (or just reformat it from the
  # provided data frame)
  if (verbose) {
    message("Prepping species data.")
  }
  # I don't think we need or want gather_species() anymore? It seems to be
  # there to support the use of a growth habit lookup table of some kind, but
  # that part of the workflow that no longer applies as far as I can tell.
  # species_list <- gather_species(species_file = species_file,
  #                                growth_habit_file = growth_habit_file,
  #                                growth_habit_code = growth_habit_code,
  #                                species_growth_habit_code = species_growth_habit_code)

  if (is.character(species_file)) {
    # First off, does this point to a real file?
    if (!file.exists(species_file)) {
      stop("The specified species_file does not exist.")
    }

    # If the species_file does point to a real file, can we read anything in?
    species_file_extension <- tools::file_ext(x = species_file) |>
      toupper()

    if (species_file_extension == "CSV") {
      species_list <- read.csv(file = species_file,
                               stringsAsFactors = FALSE,
                               na.strings = c("", " "))
    } else if (species_file_extension == "GDB") {
      if (verbose) {
        message(paste0("Given that species_file points to a GDB, attempting to read in ", species_layer, "."))
      }
      # This is written as a stub for if we want to support multiple layers at
      # once, but I don't think we will. This is because in 2025 Terrestrial AIM
      # uses two species tables, tblNationalPlants and tblStateSpecies, but
      # we've opted to do two species joins.
      relevant_layers <- sf::st_layers(dsn = species_file)[["name"]] |>
        intersect(x = _,
                  y = species_layer)
      if (length(relevant_layers) < 1) {
        stop(paste0("The species_file points to a GDB which does not contain the feature class specified by species_layer: ", paste(relevant_layers,
                                                                                                                                    collapse = ", ")))
      } else {
        if (verbose) {
          missing_species_tables <- setdiff(x = names(relevant_layers),
                                            y = species_layer)
          if (length(missing_species_tables) > 0) {
            message(paste0("Unable to find the following expected (but potentially not required) tables in the GDB: ",
                           paste(missing_species_tables,
                                 collapse = ", ")))
          }
        }
      }

      species_list <- sf::st_read(dsn = species_file,
                                  layer = relevant_layers,
                                  quiet = TRUE) |>
        # But just in case there is geometry somehow, we'll discard it.
        sf::st_drop_geometry(x = _)

    } else {
      stop("The specified species_file does not point to a CSV or GDB.")
    }

  } else if (is.data.frame(species_file)) {
    species_list <- species_file
  } else {
    stop("species_file needs to be a data frame or the filepath to a GDB or a CSV.")
  }

  # Cleanup!
  # These are variables for internal use within
  # the geodatabase, so we'll chuck them to
  # prevent issues down the line, specifically
  # trying to use distinct()
  species_list <- dplyr::select(.data = species_list,
                                -tidyselect::any_of(x = c("created_user",
                                                          "created_date",
                                                          "last_edited_user",
                                                          "last_edited_date",
                                                          "GlobalID",
                                                          "DateLoadedInDb",
                                                          "DBKey"))) |>
    # Making sure that we have character variables
    # instead of factors, that we've removed any
    # leading or trailing whitespace from strings,
    # and that we're not keeping any records where
    # the species code is NA.
    dplyr::mutate(.data = _,
                  dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                .fns = ~ as.character(x = .x) |>
                                  stringr::str_trim(string = _)),
                  dplyr::across(.cols = tidyselect::where(fn = is.character),
                                .fns = ~ dplyr::na_if(x = .x,
                                                      y = ""))) |>
    dplyr::filter(.data = _,
                  !is.na(dplyr::vars(species_code))) |>
    dplyr::distinct()


  #### Updating species codes ##################################################
  # Look for UpdatedSpecies and Update the Observation codes, if necessary
  if ("UpdatedSpeciesCode" %in% names(species_list) & update_species_codes) {
    if (any(!is.na(species_list$UpdatedSpeciesCode))) {
      if (verbose) {
        message("Applying the values from UpdatedSpeciesCode.")
      }
      # Make a new variable called internal_code_var that we can work with
      species_list <- dplyr::rename(.data = species_list,
                                    tidyselect::all_of(c(internal_code_var = species_code)))

      # Make sure Updated Species Code is a character vector
      species_list$UpdatedSpeciesCode <- as.character(species_list$UpdatedSpeciesCode)

      # Merge the Updated Species codes to the data.

      # Get the appropraite joining variables set up depending on if
      # SpeciesState should be included.
      update_joining_vars <- c("internal_code_var")
      if (by_species_key) {
        update_joining_vars <- c(update_joining_vars,
                                 "SpeciesState")
      }

      # Update the data!
      # This starts by taking the current species list and strips it down to
      # only the variables in update_joining_vars and UpdateSpeciesCode.
      species_updates <- dplyr::select(.data = species_list,
                                       tidyselect::all_of(c(update_joining_vars,
                                                            "UpdatedSpeciesCode"))) |>
        # Strip out any of the records that has an NA in any of the variables.
        dplyr::filter(.data = _,
                      dplyr::if_all(.cols = tidyselect::all_of(c(update_joining_vars,
                                                                 "UpdatedSpeciesCode")),
                                    .fns = ~ !is.na(.x))) |>
        # Make sure it's distinct!
        dplyr::distinct()

      if (by_species_key) {
        species_updates <- dplyr::filter(.data = species_updates,
                                         SpeciesState %in% data$SpeciesState)
      }

      # Stop here if the species list has duplicates!!!!!
      duplicated_species_indices <- duplicated(x = species_updates)
      if (any(duplicated_species_indices)) {
        stop(paste("Unable to update species codes when joining species information to your data. This is because there is at least one code which maps to multiple updated codes. Please either correct your species file or set update_species_codes to FALSE. The problematic codes are:",
                   paste(unique(species_updates$internal_code_var[duplicated_species_indices]),
                         collapse = ", ")))
      }

      # Provided we don't have duplicates, rename the data_code variable to
      # internal_code_var so we can actually reference it programmatically.
      data_update <- dplyr::rename(.data = data,
                                   internal_code_var = tidyselect::all_of(data_code)) |>
        # Join the updated species information to the data.
        dplyr::left_join(x = _,
                         y = species_updates,
                         relationship = "many-to-one",
                         by = update_joining_vars) |>
        # Make a final_code variable that preferentially populates with the
        # updated code but will use the original if there isn't an updated one.
        dplyr::mutate(.data = _,
                      final_code = dplyr::case_when(!is.na(UpdatedSpeciesCode) ~ UpdatedSpeciesCode,
                                                    .default = internal_code_var)) |>
        # Rename the variable back to whatever data_code is.
        dplyr::rename(.data = _,
                      tidyselect::all_of(setNames(object = "final_code",
                                                  nm = data_code)))

      # Overwrite original data with updated data
      data <- dplyr::select(.data = data_update,
                            tidyselect::all_of(names(data)))

      # Fix the renamed variable in species_list
      species_list <- dplyr::rename(.data = species_list,
                                    tidyselect::all_of(setNames(object = "internal_code_var",
                                                                nm = species_code)))
    } else {
      if (verbose) {
        message("Skipping species code updates due to lack of qualifying records.")
      }
    }
  } else if (!("UpdatedSpeciesCode" %in% names(species_list)) & update_species_codes) {
    warning("update_species_code is TRUE but UpdatedSpeciesCode is not a variable in the species list. No species codes will be updated.")
  }

  #### Handling generic species codes ##########################################
  if (verbose) {
    message("Inferring missing species information for standardized unknown species codes.")
  }
  ## Merge unknown codes
  # We need to make sure that the data object doesn't have geometry associated
  species_generic <- generic_growth_habits(data = sf::st_drop_geometry(x = data),
                                           data_code = data_code,
                                           species_list = species_list,
                                           species_code = species_code,
                                           species_growthhabit_code = species_growth_habit_code, # field name in species file of the species code to link to GrowthHabit
                                           species_duration = species_duration,
                                           verbose = verbose)

  # Disabled for now because using CurrentPLANTSCode means that there will be
  # duplicates thanks to synonymy and it's complicated to procedurally handle
  # all the possible variables that might be present.

  #### Checking for duplicate species ##########################################
  if (verbose) {
    message("Checking for duplicate species codes.")
  }

  # speciescodes_counts <- table(species_generic[[species_code]])
  # nonunique_speciescodes <- names(speciescodes_counts)[speciescodes_counts > 1]
  nonunique_speciescodes <- dplyr::summarize(.data = species_generic,
                                             .by = tidyselect::all_of(species_code),
                                             n = dplyr::n()) |>
    dplyr::filter(.data = _,
                  n > 1) |>
    dplyr::pull(.data = _,
                var = species_code)

  if (length(nonunique_speciescodes) > 0) {
    warning(paste0("There are ", length(nonunique_speciescodes), " codes which occur in the species list more than once in the variable ", species_code, ". This is expected when using a variable like CurrentPLANTSCode. The first record for each of these codes will be kept, even if other records have more complete species information. If this is unexpected, check your species list for accuracy."))
  } else {
    if (verbose) {
      message("No duplicate species codes found!")
    }
  }

  # This handles any duplicate codes.
  species_generic <- dplyr::summarize(.data = species_generic,
                                      .by = tidyselect::all_of(species_code),
                                      dplyr::across(.cols = tidyselect::any_of(x = species_property_vars),
                                                    .fns = ~ .x[!is.na(.x)] |>
                                                      dplyr::first(x = _) |>
                                                      as.character()))

  if (verbose) {
    message("Adding species_list information to the data.")
  }

  # Set join levels, so that we can flexibly include SpeciesState
  join_by <- data_code
  if (by_species_key) {
    if ("SpeciesState" %in% names(data) & "SpeciesState" %in% names(species_generic)) {
      join_by <- c(data_code, "SpeciesState")
    } else {
      if (verbose) {
        warning("by_species_key is TRUE but the variable SpeciesState is not present in both the data and species and so cannot be used in joins.")
      }
    }
  }

  # Make sure that the variables containing the codes are named the same thing
  # between the species list and the data then join them!
  data_species <- dplyr::select(.data = species_generic,
                                tidyselect::all_of(x = setNames(object = species_code,
                                                                nm = data_code)),
                                tidyselect::everything()) |>
    dplyr::left_join(x = data,
                     y = _,
                     # Enforcing that there shouldn't be multiple records in
                     # species_generic that share a code!!!!!
                     relationship = "many-to-one",
                     by = join_by) |>
    dplyr::distinct()

  # Overwrite generic species assignments with provided table
  if (overwrite_generic_species & generic_species_file != "") {
    if (!file.exists(generic_species_file)) {
      stop(paste0("The generic_species_file path points to ", generic_species_file, " which does not exist."))
    }
    if (verbose) {
      message("Attempting to read in and join generic species information using generic_species_file")
    }
    ext <- tolower(x = tools::file_ext(x = generic_species_file))
    if (ext == "csv"){
      tbl_species_generic <- read.csv(generic_species_file) |>
        dplyr::select(.data = _,
                      # This'll keep only these variables, renaming SpeciesCode
                      # to whatever data_code is.
                      tidyselect::all_of(x = setNames(object = c("SpeciesCode",
                                                                 "GrowthHabitCode",
                                                                 "Duration",
                                                                 "SG_Group",
                                                                 "Noxious"),
                                                      nm = c(data_code)))) |>
        # Make sure we're not dealing with factors
        dplyr::mutate(.data = _,
                      dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                    .fns = as.character))
    } else if (ext == "gdb") {
      tbl_species_generic <- read.csv(generic_species_file) |>
        dplyr::select(.data = _,
                      # This'll keep only these variables, renaming SpeciesCode
                      # to whatever data_code is.
                      tidyselect::all_of(x = setNames(object = c("SpeciesCode",
                                                                 "GrowthHabitCode",
                                                                 "Duration",
                                                                 "SG_Group",
                                                                 "Noxious"),
                                                      nm = c(data_code)))) |>
        # Make sure we're not dealing with factors
        dplyr::mutate(.data = _,
                      dplyr::across(.cols = tidyselect::where(fn = is.factor),
                                    .fns = as.character))
    } else {
      stop("Unknown generic species list format. Must be a path to a CSV")
    }

    # Join data_species to the generic species table and do some munging.
    data_species <- dplyr::left_join(x = data_species,
                                     y = tbl_species_generic,
                                     by = c(data_code),
                                     relationship = "many-to-one",
                                     suffix = c("", "_generic")) |>
      # This CSV is apparently expected to encode growth habit info as integers
      # so here's the hardcoded lookup.
      dplyr::mutate(.data = _,
                    GrowthHabit = dplyr::case_match(.x = GrowthHabitCode,
                                                    c(1:4) ~ "Woody",
                                                    c(5:7) ~ "NonWoody",
                                                    .missing = GrowthHabit),
                    GrowthHabitSub = dplyr::case_match(.x = GrowthHabitCode,
                                                       1 ~ "Tree",
                                                       2 ~ "Shrub",
                                                       3 ~ "Subshrub",
                                                       4 ~ "Succulent",
                                                       5 ~ "Forb",
                                                       6 ~ "Graminoid",
                                                       7 ~ "Sedge",
                                                       .missing = GrowthHabitSub),
                    # Only update these variables if the generic species info
                    # contradicts the already existing value and isn't NA
                    Duration = dplyr::case_when(!(Duration %in% Duration_generic) & !is.na(Duration_generic) ~ Duration_generic,
                                                .default = Duration),
                    SG_Group = dplyr::case_when(!(SG_Group %in% SG_Group_generic) & !is.na(SG_Group_generic) ~ SG_Group_generic,
                                                .default = SG_Group),
                    Noxious = dplyr::case_when(!(Noxious %in% Noxious_generic) & !is.na(Noxious_generic) ~ Noxious_generic,
                                               .default = Noxious)) |>
      # Keep only the relevant variables.
      dplyr::select(.data = _,
                    tidyselect::all_of(x = names(data_species)))
  }

  data_species
}

#' @export species_count
#' @rdname gather_species_inventory
species_count <- function(species_inventory_tall,
                          ...,
                          indicator_variables = NULL,
                          verbose = FALSE) {
  ##### Indicator variables -----------------------------------------------------
  # Get a list of the variables the user wants to group data by for calculations.
  # There's a grouping_variables argument that takes the names of variables as
  # character strings, so we'll handle that.
  if (!is.null(indicator_variables)) {
    if (!is.character(indicator_variables)) {
      stop("indicator_variables must be a character string or vector of character strings")
    }
  }
  # Clean this up!
  indicator_variables <- unique(indicator_variables)

  # This here because we're trying to support the legacy decision to originally
  # allow for bare variables as the indicator-defining variables.
  # Now it can be bare variable names, character strings, vectors of character
  # strings or some combination of the three.
  # BUT! You can't create a vector, store it in the environment, and then pass
  # it in by name because then you end up with just the name of the vector.
  indicator_variables <- c(indicator_variables,
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

  if (verbose) {
    message(paste0("indicator_variables contains: ",
                   paste(indicator_variables,
                         collapse = ", ")))
  }

  # Make sure that we have distinct records. Wouldn't do to have duplicated
  # anything here.
  species_inventory_tall <- dplyr::select(.data = species_inventory_tall,
                                          tidyselect::all_of(x = c("PrimaryKey",
                                                                   "Species")),
                                          tidyselect::all_of(x = indicator_variables)) |>
    dplyr::distinct()

  # dplyr::count() doesn't respect tidyselect functions, so this is easier if we
  # create the indicator variable first so we can provide count() with the bare
  # variable name.
  output <- tidyr::unite(data = species_inventory_tall,
                         col = indicator,
                         tidyselect::all_of(indicator_variables),
                         sep = ".") |>
    # I guess this is a more efficient approach than summarize()?
    dplyr::count(x = _,
                 PrimaryKey,
                 indicator) |>
    # Unconvinced that this is necessary, but a version of it was here
    # previously, so I'm leaving it for now.
    dplyr::filter(.data = _,
                  !stringr::str_detect(string = indicator,
                                       pattern = "^NA$|\\.NA|NA\\.|\\.NA\\."))

  output
}

#' Accumulated species presence, cover, and height across Line-point intercept, Vegetation height, and Species inventory methods.
#' @param lpi_tall File path to LPI rdata file
#' @param height_tall File path to height rdata file
#' @param spp_inventory_tall File path to species inventory rdata file
#' @param species_file File path to species file if you want species attributes or updated species. Geodatabase or csv allowed.
#' @param header File path to header rdata file
#' @param ... Optional filtering expression to subset the number of plots
#' @examples
#' # Get a list of all species occurring on a plot across methods (LPI, height, species inventory)
#' # This method also adds cover and height by species. Be aware that sample sizes may be insufficient to make an accurate estimate

#'accumulated_species <- accumulated_species(lpi_tall = "~/AIM/Data/lpi_tall.rdata",
#'                                                       spp_inventory_tall = "~/AIM/Data/spp_inventory_tall.rdata",
#'                                                        height_tall = "~/AIM/Data/height_tall.rdata",
#'                                                        header = "~/AIM/Data/header.rdata",
#'                                                        species_file = "species_file.csv",
#'                                                        SpeciesState %in% "NM")


#'@rdname accumulated_species
#'@export accumulated_species

accumulated_species <- function(header,
                                lpi_tall = NULL,
                                height_tall = NULL,
                                spp_inventory_tall = NULL,
                                species_file = "",
                                dead = TRUE,
                                source = c("TerrADat", "AIM", "LMF", "NRI"),
                                ...,
                                # indicator_variables = NULL,
                                generic_species_file = NULL,
                                verbose = FALSE) {
  #### SETUP ###################################################################
  # # Get a list of the variables the user wants to group data by for calculations.
  # # There's a grouping_variables argument that takes the names of variables as
  # # character strings, so we'll handle that.
  # if (!is.null(indicator_variables)) {
  #   if (!is.character(indicator_variables)) {
  #     stop("indicator_variables must be a character string or vector of character strings")
  #   }
  # }
  # # Clean this up!
  # indicator_variables <- unique(indicator_variables)
  #
  # # This here because we're trying to support the legacy decision to originally
  # # allow for bare variables as the indicator-defining variables.
  # # Now it can be bare variable names, character strings, vectors of character
  # # strings or some combination of the three.
  # # BUT! You can't create a vector, store it in the environment, and then pass
  # # it in by name because then you end up with just the name of the vector.
  # indicator_variables <- c(indicator_variables,
  #                          unquoted_to_character(...)) |>
  #   unique()
  # indicator_variables <- indicator_variables[!(indicator_variables %in% c(""))]

  # If generic_species_file is not provided, assume it is the same as species_file
  if (is.null(generic_species_file)){
    generic_species_file <- species_file
  }

  filter_exprs <- rlang::quos(...)

  #### READING #################################################################
  ##### Headers ----------------------------------------------------------------
  if (verbose) {
    message("Reading in headers and filtering them using any provided filtering expressions.")
  }

  if ("character" %in% class(header)) {
    if (tools::file_ext(header) == "Rdata") {
      header <- readRDS(file = header)
    } else {
      stop("When header is a character string it must be the path to a .Rdata file containing header data.")
    }
  }

  header <- dplyr::filter(.data = header,
                          !!!filter_exprs) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(x = c("PrimaryKey",
                                           "PlotID",
                                           "State",
                                           "SpeciesState",
                                           "Latitude_NAD83",
                                           "Longitude_NAD83",
                                           "source")))

  ##### Data -------------------------------------------------------------------
  inputs_list <- lapply(X = c("lpi_tall",
                              "height_tall",
                              "spp_inventory_tall"),
                        inputs = list("lpi_tall" = lpi_tall,
                                      "height_tall" = height_tall,
                                      "spp_inventory_tall" = spp_inventory_tall),
                        header = header,
                        verbose = verbose,
                        FUN = function(X, inputs, header, verbose){
                          if ("character" %in% class(inputs[[X]])) {
                            if (tools::file_ext(inputs[[X]]) == "Rdata") {
                              output <- readRDS(file = inputs[[X]])
                            } else {
                              stop(paste("When", X, "is a character string it must be the path to a .Rdata file containing tall data."))
                            }
                          } else if (!("data.frame" %in% class(inputs[[X]]))) {
                            if (verbose) {
                              message(paste(X, "either doesn't contain data or was NULL."))
                            }
                            return(NULL)
                          } else {
                            output <- inputs[[X]]
                          }

                          output <- dplyr::select(.data = output,
                                                  -tidyselect::any_of(x = c("FormDate")))

                          dplyr::left_join(x = dplyr::select(.data = header,
                                                             tidyselect::all_of(x = c("PrimaryKey",
                                                                                      "SpeciesState"))),
                                           y = output,
                                           relationship = "one-to-many",
                                           by = c("PrimaryKey")) |>
                            dplyr::rename(.data = _,
                                          tidyselect::any_of(x = c("code" = "Species")))
                        })

  ##### Species -----------------------------------------------------------------
  # This is way more complicated now that we're working with tblNationalPlants
  # AND tblStateSpecies.
  # First, we use species_join() to add the important information from
  # tblNationalPlants and to handle the generic species stuff.
  # Then we read in tblStateSpecies (discarding everything except the variables
  # containing codes, the states, and the sage-grouse groups) and join that to
  # the data to add in the SG_Group variable because that's all that
  # tblStateSpecies is good for these days.
  # Also, tblStateSpecies contains some duration and growth habit information
  # that (as of May 2025) is not reflected in or directly contradicts
  # tblNationalPlants or is flat-out incorrect. Those variables aren't being
  # used, but discrepancies in indicators calculated before versus after 2024
  # may be due to those not being applied.
  tblNationalPlants <- sf::st_read(dsn = species_file,
                                   layer = "tblNationalPlants",
                                   quiet = TRUE)

  tblStateSpecies <- sf::st_read(dsn = species_file,
                                 layer = "tblStateSpecies",
                                 quiet = TRUE) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(c(code = "SpeciesCode",
                                       "Duration",
                                       "GrowthHabit",
                                       "GrowthHabitSub",
                                       "SG_Group",
                                       "SpeciesState"))) |>
    dplyr::distinct()
  if (verbose) {
    message("Adding SG_Group from tblStateSpecies")
  }

  # We'll take the SpeciesState and SG_Group variables from tblStateSpecies to
  # make a new data frame where there's only one record per species code and
  # we store all the per-state SG_Group assignments in a character string as
  # pipe-separated values, e.g. "NM:PreferredForb|OR:PreferredForb".
  # This should be significantly faster than trying to join by both the species
  # codes and SpeciesState, at least for very large data sets.
  species_info <- dplyr::select(.data = tblStateSpecies,
                                tidyselect::all_of(c("code",
                                                     "SpeciesState",
                                                     "SG_Group"))) |>
    dplyr::filter(.data = _,
                  !is.na(SG_Group)) |>
    dplyr::mutate(.data = _,
                  sg_string = paste(SpeciesState,
                                    SG_Group,
                                    sep = ":")) |>
    dplyr::summarize(.data = _,
                     .by = code,
                     SG_Group = paste(sg_string,
                                      collapse = "|")) |>
    dplyr::left_join(x = tblNationalPlants,
                     y = _,
                     relationship = "many-to-one",
                     by = c("CurrentPLANTSCode" = "code"),
                     suffix = c("",
                                "_tblstatespecies")) |>
    dplyr::distinct()

  ###### Joining to the data ---------------------------------------------------
  inputs_list <- lapply(X = inputs_list,
                        species_info = species_info,
                        FUN = function(X, species_info){
                          if (is.null(X)) {
                            return(NULL)
                          }
                          current_data <- species_join(data = X,
                                                       species_file = species_info,
                                                       species_code = "NameCode",
                                                       species_property_vars = c("GrowthHabit",
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
                                                                                 "CurrentPLANTSCode",
                                                                                 "SG_Group",
                                                                                 "GrowthHabit_measured"),
                                                       update_species_codes = FALSE,
                                                       by_species_key = FALSE,
                                                       verbose = verbose) |>
                            # We want to use whatever is the currently accepted code in USDA PLANTS for
                            # the species, even though that may be less taxonomically correct.
                            # Using dplyr::case_when() lets us keep any codes that don't have a
                            # CurrentPLANTSCode value, e.g., "R" which doesn't represent a species.
                            # dplyr::mutate(.data = _,
                            #               code = dplyr::case_when(!is.na(CurrentPLANTSCode) ~ CurrentPLANTSCode,
                            #                                       .default = code)) |>
                            # Not necessary, but I'm paranoid
                            dplyr::distinct() |>
                            dplyr::mutate(.data = _,
                                          # Correct the Non-Woody to NonWoody
                                          GrowthHabit = dplyr::case_when(stringr::str_detect(string = GrowthHabit,
                                                                                             pattern = "^Non(-)?[Ww]oody$") ~ "NonWoody",
                                                                         .default = GrowthHabit),
                                          # This is to turn the SG_Group codes into values
                                          # that match the expected indicator names for
                                          # our convenience.
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
                                                                      .default = SG_Group),
                                          SpecialStatus = stringr::str_extract(string = SpecialStatus,
                                                                          pattern = paste0("(?<=((US)|(", SpeciesState, ")):)[A-z]+")),
                                          # This is just to make the Invasive values match
                                          # the desired indicator names
                                          Invasive = stringr::str_to_title(string = Invasive),
                                          # This is for the native and non-native cover
                                          # It assumes that everything flagged as EXOTIC or
                                          # ABSENT should be considered NonNative and that
                                          # everything else is Native
                                          Native = dplyr::case_when(Nonnative %in% c("NATIVE", NA) ~ "Native",
                                                                    .default = "Nonnative"),
                                          # For noxious cover. This assumes that anything
                                          # flagged as YES is noxious and nothing else is.
                                          # NOTE: This is now disabled because noxious
                                          # status is being handled more appropriately and
                                          # through a different format. I'm leaving this
                                          # for posterity for the moment though.
                                          # Noxious = dplyr::case_when(Noxious %in% c("YES") ~ "Noxious",
                                          #                            .default = NA),
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
                                          Noxious = dplyr::case_when(stringr::str_detect(string = Noxious,
                                                                                         pattern = paste0("(^|\\|)((", SpeciesState, ")|(US))")) ~ "Noxious",
                                                                     .default = NA))
                        })

  names(inputs_list) <- c("cover",
                          "heights",
                          "species")

  #### CALCULATIONS #############################################################
  output_list <- list()
  ##### Cover ------------------------------------------------------------------
  if (!is.null(inputs_list[["cover"]])) {
    if (verbose) {
      message("Working on cover indicators.")
    }

    # calculate cover by species
    species_cover <- pct_cover_species(lpi_tall = inputs_list[["cover"]]) |>
      dplyr::filter(.data = _,
                    percent > 0) |>
      dplyr::rename(.data = _,
                    "Species" = "indicator")

    ###### Live vs dead --------------------------------------------------------
    # If dead == TRUE then calculate live and dead hits as well
    if(dead) {
      if (verbose) {
        message("Calculating cover for live and dead hits.")
      }
      species_cover_live_dead <- pct_cover_live(lpi_tall = inputs_list[["cover"]],
                                                hit = "any",
                                                tall = TRUE,
                                                by_line = FALSE,
                                                code) |>
        dplyr::filter(.data = _,
                      percent > 0) |>
        # Separate the indicators based on the live vs dead.
        tidyr::separate(data = species_cover_live_dead,
                        col = indicator,
                        intto = c( "status", "Species"),
                        sep = "\\.") |>
        # Add AH as prefix and Cover as a suffix
        dplyr::mutate(.data = _,
                      status = paste0("AH_Species", status, "Cover")) |>
        # Pivot to wide so that Live and Dead are separate fields
        tidyr::pivot_wider(data = _,
                           names_from = status,
                           values_from = percent)

      # merge back with species_cover
      species_cover <- dplyr::left_join(species_cover,
                                        species_cover_live_dead)
    }

    if (verbose) {
      message("Adding hit counts to species_cover.")
    }
    # Add in the number of pin drops that each species was recorded at in the
    # LPI data.
    species_cover <- dplyr::filter(.data = inputs_list[["cover"]],
                                   nchar(as.character(code)) >= 3,
                                   !(code %in% c("None"))) |>
      dplyr::select(.data = _,
                    tidyselect::all_of(x = c("PrimaryKey",
                                             "LineKey",
                                             "PointNbr",
                                             "code"))) |>
      dplyr::distinct() |>
      dplyr::count(x = _,
                   PrimaryKey,
                   code) |>
      dplyr::left_join(x = species_cover,
                       y = _,
                       by = c("PrimaryKey",
                              "Species" = "code")) |>
      dplyr::rename(.data = _,
                    "AH_SpeciesCover" = "percent",
                    "AH_SpeciesCover_n" = "n")


    # species_cover <- lpi_species %>%
    #   subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    #   subset(nchar(as.character(code)) >= 3 & code != "None") %>%
    #   dplyr::distinct(PrimaryKey, LineKey, PointNbr, code) %>%
    #   dplyr::count(PrimaryKey, code) %>%
    #   dplyr::left_join(species_cover, .,
    #                    by = c("PrimaryKey", "Species" = "code")) %>%
    #   dplyr::rename("AH_SpeciesCover_n" = "n",)


  } else {
    if (verbose) {
      message("No LPI data provided.")
    }
    species_cover <- NULL
  }

  output_list[["cover"]] <- species_cover

  ##### Heights ----------------------------------------------------------------
  if (!is.null(inputs_list[["heights"]])) {

    # For any unresolved height errors, change height to "0" so
    # they are omitted from the calculations
    height_species <- dplyr::filter(.data = inputs_list[["heights"]],
                                    GrowthHabit_measured == GrowthHabit)
    # height_species <- height_species %>% subset(GrowthHabit_measured == GrowthHabit)

    if (verbose) {
      message("Calculating per-species mean heights.")
    }
    # calculate height by species
    species_height <- mean_height(height_tall = height_species,
                                  method = "mean",
                                  by_line = FALSE,
                                  omit_zero = TRUE,
                                  tall = TRUE,
                                  indicator_variables = c("code"))

    # add n of samples for each calculation
    species_height <- dplyr::count(x = height_species,
                                   PrimaryKey,
                                   code) |>
      dplyr::left_join(x = species_height,
                       y = _,
                       relationship = "one-to-one",
                       by = c("PrimaryKey",
                              "indicator" = "code")) |>
      dplyr::rename(.data = _,
                    "Species" = "indicator",
                    "Hgt_Species_Avg" = "mean_height",
                    "Hgt_Species_Avg_n" = "n")

    # species_height <- height_species %>%
    #   subset(PrimaryKey %in% header_sub$PrimaryKey) %>%
    #   dplyr::count(PrimaryKey, Species) %>%
    #   dplyr::left_join(., species_height,
    #                    by = c("PrimaryKey",
    #                           "Species" = "indicator")) %>%
    #   dplyr::rename("Hgt_Species_Avg_n" = "n") %>%
    #
    #   # remove "None" codes
    #   subset(Species != "None")

    ###### Live vs dead --------------------------------------------------------
    if(dead) {
      message("Calculating live and dead heights.")
      species_height_live_dead <- mean_height(height_tall = readRDS(height_tall) %>%
                                                subset(PrimaryKey %in% header_sub$PrimaryKey),
                                              method = "mean",
                                              by_line = FALSE,
                                              omit_zero = TRUE,
                                              tall = TRUE,
                                              Chkbox, Species)
      species_height_live_dead_split <- species_cover_live_dead  |>
        # Identify 0 as Live and 1 as dead
        dplyr::mutate(indicator = stringr::str_replace_all(indicator,
                                                           c("1\\." = "Dead\\.",
                                                             "0\\." = "Live\\."))
        )  |>
        # split out Live and Dead into a separate column
        tidyr::separate(indicator, c( "status", "Species"), sep = "\\.") |>
        # Add AH as prefix and Cover as a suffix
        dplyr::mutate(status = paste("Hgt_Species", status, "_Avg", sep = "")) |>
        # Pivot to wide so that Live and Dead are separate fields
        tidyr::pivot_wider(names_from = status,
                           values_from = percent)

      # merge back with species_cover
      species_height <- dplyr::left_join(species_height_live_dead_split,
                                         species_height,
                                         by = c("Species", # = "indicator",
                                                "PrimaryKey"))

    }

  } else {
    if (verbose) {
      message("No height data provided")
    }
    species_height <- NULL
  }

  output_list[["heights"]] <- species_height

  ##### Species inventory #######################################################
  if (!is.null(inputs_list[["species"]])) {
    # get list of species occurring in species inventory
    species_inventory <- dplyr::select(.data = inputs_list[["species"]],
                                       tidyselect::all_of(x = c("PrimaryKey",
                                                                "Species" = "code"))) |>
      dplyr::distinct()
  } else {
    if (verbose) {
      message("No species inventory data provided")
    }
    species_inventory <- NULL
  }

  output_list[["species"]] <- species_inventory



  #### OUTPUT ##################################################################
  # Remove all the unnecessary variables.
  # FormDate was causing problems because it was sometimes character strings and
  # sometimes POSITx dates, so we'll do some sanitization here because we're
  # only after PrimaryKey and the freshly-calculated variables that start with
  # "AH_" or "Hgt_"
  output_list <- lapply(X = output_list,
                        FUN = function(X){
                          dplyr::select(.data = X,
                                        tidyselect::all_of(x = c("PrimaryKey",
                                                                 "Species")),
                                        dplyr::matches(match = "^AH_"),
                                        dplyr::matches(match = "^Hgt_"))
                        })

  # Combine only the cover and heights via a left_join!
  # The purrr::reduce() over a list is so that if we have more tables in the
  # future this will be easy, but we could get away without it.
  output <- purrr::reduce(.x = output_list[c("cover",
                                             "heights")],
                          .f = dplyr::left_join,
                          by = c("PrimaryKey", "Species")) |>
    # And if we have species inventory stuff, we'll bind that to the end row-wise
    # then make sure we keep only the first instance of each species for each
    # PrimaryKey because only species not encountered on LPI or measured for
    # heights should be added from species inventory
    dplyr::bind_rows(. = _,
                     output_list[["species"]]) |>
    dplyr::summarize(.data = _,
                     .by = tidyselect::all_of(x = c("PrimaryKey",
                                                    "Species")),
                     dplyr::across(.cols = tidyselect::matches(match = "(AH)|(Hgt)"),
                                   .fns = dplyr::first))

  if (verbose) {
    message("Joining header and output.")
  }
  output <- dplyr::full_join(x = header,
                             y = output,
                             relationship = "one-to-many",
                             by = c("PrimaryKey"))

  # If we added species info, we'll use it here.
  # It'd be slow to use species_join() again, so we'll use the inputs_list
  if (species_file != "") {
    if (verbose) {
      message("Joining species data to the output")
    }

    # We're going to yank the species information from the inputs_list() because
    # that's computationally cheaper than doing a join from scratch again.
    suitable_input_sources <- sapply(X = inputs_list,
                                     FUN = function(X){
                                       !is.null(X)
                                     }) |>
      which()

    final_species_info <- dplyr::bind_rows(inputs_list[suitable_input_sources]) |>
      dplyr::select(.data = _,
                    tidyselect::all_of(x = c("PrimaryKey")),
                    # Should only need the last one in this vector, but the
                    # others don't hurt and were there from previous iterations
                    # of the function. Consider removing them.
                    tidyselect::any_of(x = c("Species",
                                             "Species" = "NameCode",
                                             "Species" = "code")),
                    # We're going to put the PLANTS code in its own variable so
                    # we don't collapse species codes that are distinct but
                    # unrecognized by PLANTS.
                    tidyselect::all_of(x = c("CurrentPLANTSCode")),
                    tidyselect::any_of(c("GrowthHabit",
                                         "GrowthHabitSub",
                                         "Duration",
                                         "Nonnative",
                                         "Noxious",
                                         "Invasive",
                                         "SpecialStatus",
                                         "SG_Group",
                                         "CommonName"))) |>
      dplyr::distinct()


    output <- dplyr::left_join(x = output,
                       y = final_species_info,
                       relationship = "many-to-one",
                       by = c("PrimaryKey",
                              "Species"))
  }

  missing_indicators <- setdiff(x = c("AH_SpeciesCover",
                                      "AH_SpeciesCover_n",
                                      "Hgt_Species_Avg",
                                      "Hgt_Species_Avg_n"),
                                y = names(output))

  for (current_missing_indicator in missing_indicators) {
    output[[current_missing_indicator]] <- NA
  }

  output
}

