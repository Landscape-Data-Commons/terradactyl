#' #' Gather species attribute data
#' #' @description Gather species attributes and join to species observations.
#' #' @param species_file Character string. The full file path (including file extension)
#' #' to the file containing the species list OR the species list as a data frame.
#' #' @param species_growth_habit_code Character. The field name for the growth habit
#' #'  codes in the species file. Defaults to \code{"GrowthHabitSub"}
#' #' @param growth_habit_file Character string. The full file path (including file extension)
#' #' to the file containing the growth habit list. If \code{""} we assume the species list contains those values. Defaults to \code{""}.
#' #' @param growth_habit_code Character. The field name for the growth habit codes
#' #' in the growth habit file. Defaults to \code{"Code"}
#' #' @param species_code Character. The field name for the species codes in the species file.
#' #' @param species_duration Character. the field name for the Duration field in the species file.
#' #' @param data Dataframe containing species data
#' #' @param data_code Character. The field name with the species codes in the data.
#' #' @param species_list Dataframe. Species list output from \code{}
#' #' @param generic_species_file Character. The full file path (including file extension)to the file containing the species list.
#' #' @param by_species_key Logical. If \code{TRUE} then the join will attempt to use the variable \code{"SpeciesState"} if it exists. Defaults to \code{TRUE}.
#'
#'
#' #' @export gather_species
#' #' @rdname species
#'
#' # Function to gather species information
#' gather_species <- function(species_file, #
#'                            species_growth_habit_code = "GrowthHabitSub",
#'                            growth_habit_file = "",
#'                            growth_habit_code = "Code" #
#' ) {
#'   if (is.character(species_file)) {
#'     # check to see if the species file exists and read in the appropriate file type
#'     if (!file.exists(species_file)) {
#'       stop("The species file does not exist")
#'     }
#'
#'     # read from .csv or .gdb. If gdb we assume it is of the schema aim.gdb
#'     species <- switch(EXPR = stringr::str_extract(species_file,
#'                                                   pattern = "[A-z]{3}$") |>
#'                         toupper(x = _),
#'                       GDB = {
#'                         sf::st_read(dsn = species_file,
#'                                     layer = "tblStateSpecies",
#'                                     stringsAsFactors = FALSE) |>
#'                           suppressWarnings()
#'                       },
#'                       CSV = {
#'                         read.csv(file = species_file,
#'                                  stringsAsFactors = FALSE,
#'                                  na.strings = c("", " "))
#'                       })
#'
#'   } else if (is.data.frame(species_file)) {
#'     species <- species_file
#'   }
#'
#'   # If somehow species is NULL at this point, we've got to abort.
#'   if (is.null(species)) {
#'     stop("No valid species_file value. Must be a data frame, .csv, or .gdb file.")
#'   } else {
#'     # These are variables for internal use within the geodatabase, so we'll chuck
#'     # them to prevent issues down the line, specifically trying to use distinct()
#'     species <- dplyr::select(.data = species,
#'                              -tidyselect::any_of(x = c("created_user",
#'                                                        "created_date",
#'                                                        "last_edited_user",
#'                                                        "last_edited_date",
#'                                                        "GlobalID",
#'                                                        "DateLoadedInDb",
#'                                                        "DBKey"))) |>
#'       # Speaking of distinct(), this shouldn't be necessary but doesn't hurt.
#'       dplyr::distinct()
#'   }
#'
#'   # TODO: Consider removing growth habit info
#'   # This bit tries to read in growth habit info if it can.
#'   # As far as I know, this feature is vestigial in 2025, but we're keeping it
#'   # in place because I don't want to break legacy code.
#'   growth_habit <- switch(EXPR = stringr::str_extract(growth_habit_file,
#'                                                      pattern = "[A-z]{3}$") |>
#'                            toupper(x = _),
#'                          # This no longer appears in the TerrADat geodatabase
#'                          # and hasn't in a few years, so it's disabled for now.
#'                          # GDB = {
#'                          #   sf::st_read(dsn = growth_habit_file,
#'                          #               layer = "tblSpeciesGrowthHabit",
#'                          #               stringsAsFactors = FALSE) |>
#'                          #     suppressWarnings()
#'                          # },
#'                          CSV = {
#'                            read.csv(file = growth_habit_file,
#'                                     stringsAsFactors = FALSE)
#'                          })
#'
#'   if (!is.null(growth_habit)) {
#'     # Rename the growth habit code variable to reflect the one in the species
#'     # data we're working with.
#'     growth_habit <- dplyr::rename(.data = growth_habit,
#'                                   tidyselect::all_of(x = setNames(object = growth_habit_code,
#'                                                                   nm = species_growth_habit_code)))
#'
#'     # Strip out any variables that are for internal-to-the-TerrADat-GDB purposes
#'     growth_habit <- dplyr::select(.data = growth_habit,
#'                                   -tidyselect::any_of(x = c("created_user",
#'                                                             "created_date",
#'                                                             "last_edited_user",
#'                                                             "last_edited_date",
#'                                                             "GlobalID",
#'                                                             "DateLoadedInDb",
#'                                                             "DBKey",
#'                                                             "PrimaryKey")))
#'
#'     # Join the species list and the growth habit stuff
#'     # This didn't have a "by" specified previously, so I've left it unspecified
#'     # for now.
#'     species <- dplyr::left_join(x = dplyr::select(.data = species,
#'                                                   tidyselect::any_of(x = diff(x = names(growth_habit),
#'                                                                               y = c("PrimaryKey")))),
#'                                 y = growth_habit,
#'                                 # I think that this should be a one-to-one but
#'                                 # if this breaks things we can take it out.
#'                                 relationship = "one-to-one")
#'   } else if (growth_habit_file != "") {
#'     warning("The provided value for growth_habit_file does not point to the filepath for a CSV file and has been ignored.")
#'   }
#'
#'   # Final cleanup!
#'   # Making sure that we have character variables instead of factors, that we've
#'   # removed any leading or trailing whitespace from strings, and that we're not
#'   # keeping any records where the species code is NA.
#'   species <- dplyr::mutate(.data = species,
#'                            dplyr::across(.cols = tidyselect::where(fn = is.factor),
#'                                          .fns = ~ as.character(x = .x) |>
#'                                            stringr::str_trim(string = _))) |>
#'     dplyr::filter(.data = _,
#'                   !is.na(dplyr::vars(species_code)))
#'
#'   species
#' }


#' @export generic_growth_habits
#' @rdname species

# Attribute generic species growth habits, for now this assumes field names.
generic_growth_habits <- function(data,
                                  data_code = "code", # Species field in the data
                                  species_list, # from  gather_species ()
                                  species_code = "SpeciesCode", # Species code value from species list
                                  species_growth_habit_code = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
                                  species_duration = "Duration" # field name for duration

) {
  generic_df <- data.frame(
    SpeciesFixed = unique(data[[data_code]]),
    SpeciesOriginal = unique(data[[data_code]])
  ) %>%

    # Clean up the species codes, remove white space
    dplyr::mutate(SpeciesFixed = toupper(SpeciesFixed) %>%
                    stringr::str_replace_all(
                      string = .,
                      pattern = " |-", replacement = ""
                    )) %>%

    # Get unknown codes and clean them up. Unknown codes beging with a 2 (LMF/NRI)
    # or a 2 letter prefix followed by a number.
    # Older projects also used "AAFF" etc. to identify unknown and dead
    # beyond recognition codes. So we'll need to detect those too
    dplyr::filter(stringr::str_detect(
      string = SpeciesFixed,
      pattern = "^2|^[A-z]{2}[[:digit:]]|\\b(?=\\w*(^[A|P|S|T])\\1+)\\w+\\b"
    )) %>%

    # Identify prefix
    dplyr::mutate(Prefix = gsub(SpeciesFixed,
                                pattern = "[[:digit:]]",
                                replacement = ""
    ) %>%
      as.character()) %>%
    # reduce AAFF etc to two letter prefix
    dplyr::mutate(Prefix = dplyr::if_else(
      stringr::str_detect(
        string = SpeciesOriginal,
        pattern = "^[[:alpha:]]"
      ),
      stringr::str_replace_all(
        string = Prefix,
        pattern = "([[:alpha:]])\\1",
        replacement = "\\1"
      ),
      Prefix
    )) %>%

    # Rename to data species code field
    dplyr::rename_at(dplyr::vars(SpeciesOriginal), ~data_code)

  # If there a no unknown species, no need to proceed
  generic_df <- generic_df[!generic_df[, data_code] %in%
                             species_list[, species_code], ]


  # Merge with generic species definitions
  generic.code.df <- dplyr::inner_join(
    terradactyl::generic.species %>% dplyr::select(-c(Source, CommonName)) %>%
      dplyr::distinct(),
    generic_df,
    by = "Prefix"
  )


  # Connect unknown codes to SpeciesState
  if ("SpeciesState" %in% colnames(species_list) & "SpeciesState" %in% colnames(data)) {
    generic.code.df <- dplyr::inner_join(generic.code.df[!is.na(species_code), ],
                                         dplyr::select(data, data_code, SpeciesState))
  } else {
    warning("Variable 'SpeciesState' is not present in either the data or the lookup table")
    generic.code.df <- dplyr::inner_join(generic.code.df[!is.na(species_code), ],
                                         # We have to use dplyr::select() because that returns
                                         # a data frame instead of a vector when there's only
                                         # one variable being asked for
                                         dplyr::select(data, data_code))
  }

  generic.code.df <- unique(generic.code.df)

  # if there are records in generic.code.df
  if (nrow(generic.code.df) > 0) {
    # Indicate that generic codes are non-noxious
    if ("Noxious" %in% names(species_list)) {
      generic.code.df$Noxious <- "NO"
    }

    # Indicate that generic shrubcodes are SG_Group "NonSagebrushShrub"
    if ("SG_Group" %in% names(species_list)) {
      generic.code.df$SG_Group[generic.code.df$Code == "SH" | generic.code.df$Code == "2SHRUB"] <- "NonSagebrushShrub"
    }
  }

  # Rename to SpeciesCode in species list
  generic.code.df <- generic.code.df %>%
    dplyr::rename_at(dplyr::vars(data_code), ~species_code)

  # Subset generic species that are not defined in species list
  generic.code.df <- generic.code.df %>%
    dplyr::filter(!dplyr::vars(data_code) %in% dplyr::select(data, data_code))

  # Merge with main species list
  species_generic <- dplyr::full_join(species_list, generic.code.df)

  # Remove Code, Prefix, and PrimaryKey if they exist
  species_generic <- species_generic[, !colnames(species_generic) %in%
                                       c("Code", "PrimaryKey", "Prefix", "DateLoadedInDb")]

  # remove GrowthHabit, GrowthHabitSub, and Duration if they are not the specified data columns
  if(species_growth_habit_code != "GrowthHabitSub") {
    species_generic <- species_generic %>% dplyr::select_if(!colnames(.) %in% c("GrowthHabit", "GrowthHabitSub"))
  }

  if(species_duration != "Duration") {
    species_generic <- species_generic %>% dplyr::select_if(!colnames(.) %in% "Duration")
  }

  # Remove NA in species list
  if ("SpeciesCode" %in% names(species_generic)) {
    species_generic <- species_generic %>% subset(!is.na(SpeciesCode))

    return(species_generic)
  }

  return(species_generic)
}

#' @export species_join
#' @rdname species

# Join species with field data
species_join <- function(data, # field data,
                         data_code = "code", # Species field in the data
                         species_file, # path to .csv or .gdb holding  the species table
                         species_code = "SpeciesCode", # field name in species file that identifies the species code
                         species_growth_habit_code = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
                         species_duration = "Duration", # field name in species file of the Duration assignment
                         growth_habit_file = "", # path to .csv or gdb holding tblSpeciesGrowthHabit
                         growth_habit_code = "Code",
                         overwrite_generic_species = FALSE,
                         generic_species_file = "",
                         update_species_codes = TRUE,
                         by_species_key = TRUE) {
  # message
  message("Gathering species data")

  # Set join levels, so that we can flexibly include SpeciesState
  join_by <- data_code
  if (by_species_key) {
    if ("SpeciesState" %in% names(data)) {
      join_by <- c(data_code, "SpeciesState")
    }
  }


  # Some projects use "None" to indicate "No species". Convert those to "N"
  # instead.
  data <- dplyr::mutate(.data = data,
                        dplyr::across(.cols = tidyselect::all_of(data_code),
                                      .fns = ~ stringr::str_replace(string = .x,
                                                                    pattern = "None",
                                                                    replacement = "N")))

  ## Load species data
  species_list <- terradactyl::gather_species(species_file = species_file,
                                              growth_habit_file = growth_habit_file,
                                              growth_habit_code = growth_habit_code,
                                              species_growth_habit_code = species_growth_habit_code)

  # Clean up values in the species_list that should be NA.
  # There are two passes because dplyr::na_if() can only compare vectors of
  # identical lengths.
  species_list <- dplyr::mutate(.data = species_list,
                                dplyr::across(.cols = dplyr::where(fn = is.character),
                                              .fns = ~ dplyr::na_if(x = .x,
                                                                    y = "")),
                                dplyr::across(.cols = dplyr::where(fn = is.character),
                                              .fns = ~ dplyr::na_if(x = .x,
                                                                    y = "NA")))


  # Look for UpdatedSpecies and Update the Observation codes, if necessary
  if ("UpdatedSpeciesCode" %in% names(species_list) & update_species_codes) {
    if (any(!is.na(species_list$UpdatedSpeciesCode))) {

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
    }
  }

  ## Merge unknown codes
  species_generic <- generic_growth_habits(
    data = as.data.frame(data), # in some applications, data will be an sf object
    data_code = data_code,
    species_list = species_list,
    species_code = species_code,
    species_growth_habit_code = species_growth_habit_code, # field name in species file of the species code to link to GrowthHabit
    species_duration = species_duration # field name for duration
  )



  # # check for duplicate species
  # if (nrow(species_generic[duplicated(species_generic$Symbol), ]) > 0) {
  #   warning("Duplicate species codes in the species file.
  #           The first species occurrence will be used.")
  #   message(species_generic[duplicated(species_generic$Symbol), ])
  # }


  # message
  message("Merging data and species tables")

  ## Rename column
  species_generic <- species_generic %>%
    dplyr::rename_at(dplyr::vars(species_code), ~data_code)

  ## Remove any duplicate values
  species_generic <- species_generic %>% dplyr::distinct()

  # If species are entered more than once but with different data (eg Family is missing once), it wont be removed by the above
  species_generic <-
    species_generic[!duplicated(species_generic %>%
                                  dplyr::select(all_of(join_by))),]

  # Add species information to data
  data_species <- dplyr::left_join(
    x = data %>% dplyr::mutate_at(dplyr::vars(data_code), toupper),
    y = species_generic,
    # Enforcing that there shouldn't be multiple records in species_generic that
    # share a code!!!!!
    relationship = "many-to-one",
    by = join_by
  )

  data_species <- data_species %>% dplyr::distinct()

  # Overwrite generic species assignments with provided table
  if (overwrite_generic_species) {
    if (generic_species_file == "") {
      generic_species_file <- species_file
    }
    ext <- tolower(x = tools::file_ext(x = generic_species_file))
    if(ext == "gdb"){
      tbl_species_generic <- sf::st_read(
        dsn = generic_species_file,
        layer = "tblSpeciesGeneric",
        stringsAsFactors = FALSE
      )
    } else if (ext == "csv"){
      tbl_species_generic <- read.csv(generic_species_file)
    } else {
      stop("Unknown generic species list format. Must be a path to a geodatabase (.gdb) or comma-separated values file (.csv)")
    }
    # Read tblSpeciesGeneric
    tbl_species_generic <- tbl_species_generic %>%
      # Select only the needed fields
      dplyr::select(
        SpeciesCode,
        # NOTE: WHY WAS DBKEY INCLUDED HERE????
        # DBKey,
        GrowthHabitCode,
        Duration, SG_Group, Noxious
      ) %>%
      # Convert to character
      dplyr::mutate_if(is.factor, as.character)

    # Rename SpeciesCode to the data_code value

    tbl_species_generic <- tbl_species_generic %>%
      dplyr::rename_at("SpeciesCode", ~data_code)

    # Join data_species to the generic species table
    data_species_generic <- dplyr::left_join(
      x = data_species,
      y = tbl_species_generic,
      by = c(data_code)
    )

    # Convert GrowthHabitCode to GrowthHabit and GrowthHabitSub
    data_species_generic <- data_species_generic %>%
      dplyr::mutate(
        GrowthHabit = dplyr::recode(as.character(GrowthHabitCode),
                                    "1" = "Woody",
                                    "2" = "Woody",
                                    "3" = "Woody",
                                    "4" = "Woody",
                                    "5" = "NonWoody",
                                    "6" = "NonWoody",
                                    "7" = "NonWoody",
                                    .missing = as.character(GrowthHabit)
        ),
        GrowthHabitSub = dplyr::recode(as.character(GrowthHabitCode),
                                       "1" = "Tree",
                                       "2" = "Shrub",
                                       "3" = "Subshrub",
                                       "4" = "Succulent",
                                       "5" = "Forb",
                                       "6" = "Graminoid",
                                       "7" = "Sedge",
                                       .missing = as.character(GrowthHabitSub)
        ),

        # If the Duration assignments are different, overwrite
        Duration = ifelse(Duration.x != as.character(Duration.y) & !is.na(Duration.y),
                          Duration.y, Duration.x
        ),

        # If the SG_Group assignments are different, overwrite
        SG_Group = ifelse(SG_Group.x != as.character(SG_Group.y) & !is.na(SG_Group.y),
                          SG_Group.y, SG_Group.x
        ),

        # If the Noxious assignments are different, overwrite
        Noxious = ifelse(Noxious.x != as.character(Noxious.y) & !is.na(Noxious.y),
                         Noxious.y, Noxious.x
        )
      )

    # Select only the fields from the original data_species file
    data_species <- data_species_generic[, colnames(data_species)]
  }

  return(data_species)
}

#' @export species_read
#' @rdname species

# Read in info about species
species_read <- function(path,
                         names = NULL) {
  if (!(class(names) %in% "character")) {
    stop("If used, 'names' must be of the class 'character'.")
  }

  if (is.character(path)) {
    if (length(path) != 1) {
      stop("path must be a single character string defining the full filepath to a GDB containing the species information as tables, a folder containing containing the species information in a CSV, or to the CSV itself.")
    }
    # if (verbose) {
    #   message("Attempting to read in species information.")
    # }
    # Check to see if the path exists
    if (!file.exists(path)) {
      stop(paste("Unable to find ",
                 path))
    }

    # Determine the filetype of the source so we can handle it appropriately
    path_file_extension <- toupper(x = stringr::str_extract(string = path,
                                                            pattern = "(?<=\\.)\\w{2,4}$"))

    # If species_source is a GDB, we'll attempt to pull the tables from it
    # This is written for the possibility of reading in multiple tables, but we're
    # not doing that right now.
    if (path_file_extension %in% "GDB") {
      if (is.null(names)) {
        stop("When 'path' points to a GDB, you must use 'names' to specify the name of one or more layers to read.")
      }
      if (verbose) {
        message("Preparing to read from GDB.")
      }

      layer_info <- sf::st_layers(dsn = path)
      missing_layers <- names[!(names %in% layer_info[["name"]])]
      if (length(missing_layers) > 0) {
        stop(paste("Unable to find the following layer(s):",
                   paste(missing_layers,
                         collapse = ", ")))
      }
      geometry_layers <- names[names %in% layer_info[["name"]][!is.na(layer_info[["geometry_type"]])]]
      if (length(geometry_layers) > 0) {
        warning(paste("The following layers will have their geometry stripped when read in:",
                      paste(geometry_layers,
                            collapse = ", ")))
      }
    } else if (path_file_extension %in% "CSV") {
      # If the path points directly to a CSV, split it so we can read it in below
      if (!is.null(names)) {
        warning("'path' points to a CSV. Ignoring the value(s) in 'names'.")
      }
      names <- basename(path = path)
      path <- dirname(path = path)
    } else if (is.na(path_file_extension)) {
      # If we get an NA, that means that there wasn't a file extension in the
      # path, so we'll need to check for the CSV
      if (verbose) {
        message("Checking validity of filename(s) in 'names'.")
      }
      if (is.null(names)) {
        stop("When 'path' points to a folder, you must use 'name' to specify the filename of the CSV to read.")
      }
      names <- sapply(X = names,
                      FUN = function(X){
                        current_file_extension <- toupper(x = stringr::str_extract(string = X,
                                                                                   pattern = "(?<=\\.)\\w{2,4}$"))
                        if (current_file_extension %in% "CSV") {
                          X
                        } else if (is.na(current_file_extension)) {
                          warning(paste("No file extension present, assuming CSV."))
                          paste0(X, ".csv")
                        } else {
                          stop(paste("The file extension", current_file_extension, "is not valid. Only CSV files are accepted when 'path' does not point to a GDB."))
                        }
                      })
    }

    # Now that we've got path and name sorted, we can read it in
    if (verbose) {
      message("Attempting to read in species information.")
    }

    # This is a lapply() so that in the future we have the option of reading in
    # multiple species sources in a go, but that's not critical right now.
    species_info <- lapply(X = names,
                           filepath = path,
                           source_extension = path_file_extension,
                           FUN = function(X, filepath, source_extension){
                             if (source_extension %in% "GDB") {
                               current_species_info <- suppressWarnings(expr = sf::st_drop_geometry(sf::st_read(dsn = filepath,
                                                                                                                layer = X,
                                                                                                                stringsAsFactors = FALSE)))
                             } else {
                               current_species_info <- read.csv(file = paste0(filepath, "/", X),
                                                                stringsAsFactors = FALSE)
                             }
                             dplyr::distinct(current_species_info)
                           })
  } else {
    stop("'path' must be a character string specifying the filepath to a CSV, to a folder containing the CSV specified in 'name', or to a GDB containing the layer specified in 'name'.")
  }

  # No need to keep things in a list if there's only one data frame
  if (length(species_info) == 1) {
    species_info[[1]]
  } else {
    species_info
  }
}


#' @export species_join
#' @rdname species

# # Join species with field data
# species_join_aim <- function(data, # field data,
#                              data_species_var = "code", # Species field in the data
#                              species_info = NULL, # path to .csv or .gdb holding  the species table
#                              species_info_path = NULL,
#                              species_info_names = NULL,
#                              species_info_species_var = "SpeciesCode", # field name in species file that identifies the species code
#                              species_info_growthhabit_var = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
#                              species_info_duration_var = "Duration", # field name in species file of the Duration assignment
#                              species_info_species_update_var = "UpdatedSpeciesCode",
#                              species_info_noxious_var = "Noxious",
#                              species_info_sg_var = "SG_Group",
#                              additional_join_vars = "SpeciesState",
#                              standardize_nones = TRUE,
#                              check_updated_codes = TRUE,
#                              verbose = FALSE) {
#   # This sets up renaming variables for easier joins later
#   names(data_species_var) <- "internal_species_var"
#   names(species_info_species_var) <- "internal_species_var"
#   names(additional_join_vars) <- paste0("internal_join_var_", seq(length(additional_join_vars)))
#
#   # Handle the species information. We'll check to see if it's been provided as
#   # a data frame and if not attempt to read it in from the provided source(s).
#   if (is.null(species_info)) {
#     if (is.null(species_info_path)) {
#       stop("You must provide either a data frame as species_info or at least a filepath to the species information as species_path.")
#     } else {
#       if (verbose) {
#         message("Starting with the species info.")
#       }
#       species_info <- species_read(path = species_info_path,
#                                    names = species_info_names)
#
#     }
#   } else {
#     if (!is.null(species_info_path)) {
#       # if (verbose) {
#       #   message("Ignoring species_info_path because species_info has been provided.")
#       # }
#       warning("Ignoring species_info_path because species_info has been provided.")
#     }
#   }
#
#   if (verbose) {
#     message("Checking validity of the provided species info.")
#   }
#
#   # Check that species_info is a data frame or list of them
#   if ("data.frame" %in% class(species_info)) {
#     # We'll want it in a list just so we can handle things the same down the line
#     # whether there were multiple species info data frames or not
#     species_info <- list(species_info)
#   } else if ("list" %in% class(species_info)) {
#     species_info_classes_dataframe <- sapply(X = species_info,
#                                              FUN = function(X){
#                                                "data.frame" %in% class(X)
#                                              })
#     if (any(!species_info_classes_dataframe)) {
#       stop(paste("The species info provided is not all in data frames. This is most likely to occur when providing a list of objects as species_info. The problem indices are:",
#                  paste(which(!species_info_classes_dataframe),
#                        collapse = ", ")))
#     }
#   } else {
#     stop("species_info must be either a data frame or a list of data frames.")
#   }
#
#   # And then we'll check for the required variables
#   # First, we'll check for the absolutely required variable
#   species_info_species_var_present <- sapply(X = species_info,
#                                              var_name = species_info_species_var,
#                                              FUN = function(X, var_name) {
#                                                var_name %in% names(X)
#                                              })
#   if (any(!species_info_species_var_present)) {
#     stop(paste("The required variable", species_info_species_var, "specified with the argument species_info_species_var is not present in the species info data frame(s) at the following indices:",
#                paste0(which(!species_info_species_var_present),
#                       collapse = ", ")))
#   }
#
#   # And and now we'll make sure that the other requested variables each show up
#   # in at least one data frame in species_info
#   required_vars <- unique(c(species_info_growthhabit_var,
#                             species_info_duration_var,
#                             additional_join_vars))
#
#   required_vars_present_count <- sapply(X = required_vars,
#                                         species_info = species_info,
#                                         FUN = function(X, species_info){
#                                           sum(sapply(X = species_info,
#                                                      current_var = X,
#                                                      FUN = function(X, current_var){
#                                                        current_var %in% names(X)
#                                                      }))
#                                         })
#
#   if (0 %in% required_vars_present_count) {
#     stop(paste("Unable to find all required variables in species info. The following variable names do not appear:",
#                paste0(required_vars[which(required_vars_present_count %in% 0)],
#                       collapse = ", ")))
#   }
#   if (any(required_vars_present_count > 1)) {
#     warning(paste("One or more required variables occur more than once in species info, which may cause issues with joins. The following variable names appear multiple times:",
#                   paste0(required_vars[which(required_vars_present_count > 1)],
#                          collapse = ", ")))
#   }
#
#   if (verbose) {
#     message("Converting empty strings and 'NA' strings to NAs.")
#   }
#   species_info <- lapply(X = species_info,
#                          FUN = function(X){
#                            current_species_info <- X
#                            for (current_value in c("", "NA")) {
#                              current_species_info <- dplyr::mutate(.data = current_species_info,
#                                                                    dplyr::across(.cols = dplyr::where(is.character),
#                                                                                  .fns = ~ dplyr::na_if(x = .x,
#                                                                                                        y = current_value)))
#                            }
#                            current_species_info
#                          })
#
#   if (verbose) {
#     message("Species info appears valid.")
#   }
#
#   if (verbose) {
#     message("Checking data for required variables.")
#   }
#   # #_#_#_#_#_#_#
#   # data <- data.frame(PrimaryKey = c("1", "2", "3"),
#   #                    code = c("why", "None", "there"),
#   #                    SpeciesState = c("KS", "OR", "NM"),
#   #                    UpdatedSpeciesCode = c(NA, NA, "grievous"))
#   # #_#_#_#_#_#_#
#   required_data_variables <- c(data_species_var,
#                                additional_join_vars)
#   required_variable_present <- sapply(X = required_data_variables,
#                                       data = data,
#                                       FUN = function(X, data){
#                                         X %in% names(data)
#                                       })
#   if (any(!required_variable_present)) {
#     stop(paste("Not all required variables were found in the data. The missing variables are:",
#                paste0(required_data_variables[!required_variable_present],
#                       collapse = ", ")))
#   }
#
#   # Because some projects have recorded the absence of plant cover in the top
#   # canopy with "None" instead of our assumed "N", we'll change that here.
#   if (standardize_nones) {
#     none_present_vector <- stringr::str_detect(string = data[[data_species_var]],
#                                                pattern = "^None$")
#     if (any(none_present_vector)) {
#       if (verbose) {
#         message("Converting 'None' records to 'N' in data.")
#       }
#       data[[data_species_var]][none_present_vector] <- "N"
#     }
#   }
#
#   if (check_updated_codes) {
#     if (verbose) {
#       message("Attempting to check for an updated species code variable in species info.")
#     }
#     updated_code_var_present <- sapply(X = species_info,
#                                        var_name = species_info_species_update_var,
#                                        FUN = function(X, var_name){
#                                          var_name %in% names(X)
#                                        })
#     if (any(updated_code_var_present)) {
#       if (sum(updated_code_var_present) == 1) {
#         if (verbose) {
#           message("Update variable found. Species codes will be updated after species info is joined to the data.")
#         }
#       } else {
#         warning("Update variable found at multiple indices in species_info. The update will be applied after each join, which may cause errors. It is advisable to only have one species info source with species code update information.")
#       }
#     } else {
#       if (verbose) {
#         message(paste("The variable", species_info_species_update_var, "does not appear in the species info. If this is unexpected, please check your species info source(s). Proceeding without updating."))
#       }
#       # Flag this so we can make sure to not do anything about it later.
#       check_updated_codes <- FALSE
#     }
#   }
#
#   # Let's merge the species info into the data
#   # A loop is easiest and shouldn't be inefficient in this context
#   # #_#_#_#_#_#_#
#   # data <- data.frame(PrimaryKey = as.character(seq(7)),
#   #                    code = c("AF69420", "PF69420",
#   #                             "AG69420", "PG69420",
#   #                             "TR69420",
#   #                             "SH69420",
#   #                             "SU69420"),
#   #                    Duration = c(NA, "Biennial",
#   #                                 NA, NA,
#   #                                 NA,
#   #                                 NA,
#   #                                 NA),
#   #                    GrowthHabitSub = c("Moss", NA,
#   #                                       NA, NA,
#   #                                       NA,
#   #                                       NA,
#   #                                       NA),
#   #                    SpeciesState = c("KS", "KS", "KS", "KS", "KS", "KS", "KS"))
#   # species_info <- list(data.frame(PrimaryKey = c("AF69420", "PF69420",
#   #                                                "AG69420", "PG69420",
#   #                                                "TR69420",
#   #                                                "SH69420",
#   #                                                "SU69420"),
#   #                                 SpeciesCode = c("AF69420", "PF69420",
#   #                                                 "AG69420", "PG69420",
#   #                                                 "TR69420",
#   #                                                 "SH69420",
#   #                                                 "SU69420"),
#   #                                 SpeciesState = c("KS", "KS", "KS", "KS", "KS", "KS", "KS"),
#   #                                 GrowthHabitSub = c("test", "test2",
#   #                                                    NA, NA,
#   #                                                    NA,
#   #                                                    NA,
#   #                                                    NA),
#   #                                 UpdatedSpeciesCode = c("kombucha", "garbage",
#   #                                                        NA, NA,
#   #                                                        NA,
#   #                                                        NA,
#   #                                                        NA)),
#   #                      data.frame(PrimaryKey = as.character(seq(7)),
#   #                                 SpeciesCode = c("AF69420", "PF69420",
#   #                                                 "AG69420", "PG69420",
#   #                                                 "TR69420",
#   #                                                 "SH69420",
#   #                                                 "SU69420"),
#   #                                 SpeciesState = c("KS", "KS", "KS", "KS", "KS", "KS", "KS"),
#   #                                 SG_Group = c("PreferredForb", "PreferredForb",
#   #                                              NA, NA,
#   #                                              NA,
#   #                                              "Sagebrush",
#   #                                              NA),
#   #                                 UpdatedSpeciesCode = c(NA, NA,
#   #                                                        "ethanol", "synthehol",
#   #                                                        NA,
#   #                                                        NA,
#   #                                                        NA)))
#   #_#_#_#_#_#_#
#   # These will be used to rename variables to easily join and then rename them
#   # back after the join.
#   data_joining_vars <- c(data_species_var,
#                          additional_join_vars)
#   species_info_joining_vars <- c(species_info_species_var,
#                                  additional_join_vars)
#   # We're doing this by index so we can report back that information in warning
#   # messages.
#   for (current_species_info_index in seq(length(species_info))) {
#     if (verbose) {
#       message(paste0("Attempting to join species info to data. (",
#                      current_species_info_index, " of ", length(species_info), ")"))
#     }
#     current_species_info <- species_info[[current_species_info_index]]
#     # We're going to drop variables from current_species_info that aren't join
#     # variables but do share names with variables in data.
#     # We're doing this on each loop in case there's a conflict between two
#     # indices in species_info
#     current_data_var_names <- names(data)
#     vars_to_drop <- current_data_var_names[!(current_data_var_names %in% species_info_joining_vars)]
#     current_species_info_reduced <- dplyr::select(.data = current_species_info,
#                                                   -dplyr::any_of(vars_to_drop))
#     if (!identical(current_species_info, current_species_info_reduced)) {
#       warning(paste("At index", current_species_info_index, "of", length(species_info), "of species_info, at least one non-joining variable name is shared in common between the data and the species information. Variables with duplicated names have been dropped from species information."))
#     }
#     # This is renaming variables to internal names for easy, clean joining using
#     # the named vector of joining variables as a lookup. We'll switch them back
#     # after the join.
#     current_data_joining <- dplyr::rename(.data = data,
#                                           dplyr::any_of(x = data_joining_vars))
#     current_species_info_joining <- dplyr::rename(.data = current_species_info_reduced,
#                                                   dplyr::any_of(x = species_info_joining_vars))
#
#     # Join the data and the current species info
#     current_data_joined <- dplyr::left_join(x = current_data_joining,
#                                             y = current_species_info_joining,
#                                             by = dplyr::all_of(unique(names(c(data_joining_vars,
#                                                                               species_info_joining_vars)))))
#     # And rename them back
#     current_data_joined <- dplyr::rename(.data = current_data_joined,
#                                          # This inverts the values and names in the data_joining_vars
#                                          # vector so we can swap them back in the data frame
#                                          dplyr::all_of(x = setNames(object = names(data_joining_vars),
#                                                                     nm = unname(data_joining_vars))))
#
#     # Now we'll do any species code updates that are called for and drop the
#     # update variable once we're done with it to prevent collisions with subsequent
#     # data frames in species_info (and because all the relevant info has been
#     # moved into the appropriate variable anyway)
#     if (check_updated_codes & species_info_species_update_var %in% names(current_data_joined)) {
#       if (verbose) {
#         message(paste("Updating species codes joined from species info then removing the variable",
#                       species_info_species_update_var,
#                       "from the join result."))
#       }
#       if (current_species_info_index != length(species_info)) {
#         warning("This update may result in unexpected join results because there are additional propoerties to be joined after this.")
#       }
#       current_data_joined <- dplyr::mutate(.data = current_data_joined,
#                                            {{data_species_var}} := dplyr::coalesce(current_data_joined[[species_info_species_update_var]],
#                                                                                    current_data_joined[[data_species_var]]))
#       current_data_joined <- dplyr::select(.data = current_data_joined,
#                                            -dplyr::all_of(species_info_species_update_var))
#     }
#     # Storing the joined data!
#     data <- current_data_joined
#   }
#
#   if (verbose) {
#     message("Attempting to add duration and growth habit attributes to species recorded using the BLM AIM unknown plant codes.")
#   }
#   # # We're going to try to guess generic species codes' durations and growth
#   # # habits keep using the same tricks as the indicator renaming in lpi_calc().
#   # # These vectors define the expected regex patterns for duration and growth
#   # # habit values based on the standard BLM AIM implementation of unknown plant
#   # # codes used when a species has not yet been identified.
#   # # The names of the values in the vectors are the values to write into the
#   # # relevant variables and the values themselves are the corresponding regular
#   # # expressions.
#   # # We'll use this to detect whether a value is a valid generic unknown code.
#   # generic_regex <- "^(([AP][GF])|(TR)|(SH)|(SU))\\d+$"
#   # # An unknown code represents an annual plant when it starts with an A which is
#   # # followed by either a G or F (graminoid or forb) and then a series of numbers
#   # # until the end of the string.
#   # generic_duration_regex_vector <- c(annual = "^A[GF]\\d+$",
#   #                                    # An unknown code represents a perennial
#   #                                    # plant when it starts with a TR (tree), SH
#   #                                    # (shrub), SU (succulent), or a P which is
#   #                                    # followed by either a G or F. These codes
#   #                                    # also end with a series of numbers until
#   #                                    # the end of the string.
#   #                                    perennial = "^((P[GF])|(TR)|(SH)|(SU))\\d+$")
#   # # For the growth habits we're really only looking at the first two letters,
#   # # but we want to be making sure that the whole code matches the BLM AIM
#   # # unknown format
#   # generic_growthhabitsub_regex_vector <- c(graminoid = "^[AP]G\\d+$",
#   #                                          forb = "^[AP]F\\d+$",
#   #                                          tree = "^TR\\d+$",
#   #                                          shrub = "^SH\\d+$",
#   #                                          succulent = "^SU\\d+$")
#   #
#   #
#   # # Then it's as simple as putting subbing in the desired values wherever there
#   # # isn't already a value. This won't guarantee that every code in the data has
#   # # a growth habit and duration assigned, but ought to catch all the correctly-
#   # # formatted unknowns.
#   # data <- dplyr::mutate(.data = data,
#   #                       Duration = dplyr::case_when(is.na(Duration) & stringr::str_detect(string = code,
#   #                                                                                         pattern = generic_regex) ~ stringr::str_to_title(stringr::str_replace_all(string = code,
#   #                                                                                                                                                                   pattern = setNames(object = names(generic_duration_regex_vector),
#   #                                                                                                                                                                                      nm = unname(generic_duration_regex_vector)))),
#   #                                                   .default = Duration),
#   #                       GrowthHabitSub = dplyr::case_when(is.na(GrowthHabitSub) & stringr::str_detect(string = code,
#   #                                                                                                     pattern = generic_regex) ~ stringr::str_to_title(stringr::str_replace_all(string = code,
#   #                                                                                                                                                                               pattern = setNames(object = names(generic_growthhabitsub_regex_vector),
#   #                                                                                                                                                                                                  nm = unname(generic_growthhabitsub_regex_vector)))),
#   #                                                         .default = GrowthHabitSub))
#
#   #### Generic species handling ------------------------------------------------
#   # This is the old stuff, but it works. We'll replace it with something that
#   # doesn't need a lookup table eventually.
#
#   ## Merge unknown codes
#   species_generic <- generic_growth_habits(data = data,
#                                            data_code = "code",
#                                            species_list = species_list,
#                                            species_code = "SpeciesCode",
#                                            species_growth_habit_code = "GrowthHabitSub",
#                                            species_duration = "Duration")
#
#   # Check for duplicate species
#   if (any(duplicated(species_generic$Symbol))) {
#     warning("Duplicate species codes in the species file.
#             The first species occurrence will be used.")
#   }
#
#   if (verbose) {
#     message("Merging data and species tables")
#   }
#
#   ## Rename column
#   species_generic <- dplyr::rename(.data = species_generic,
#                                    setNames(object = "SpeciesCode",
#                                             nm = data_code))
#   # species_generic <- species_generic %>%
#   #   dplyr::rename_at(dplyr::vars(species_code), ~data_code)
#
#   ## Remove any duplicate values
#   species_generic <- dplyr::distinct(species_generic)
#
#   # If species are entered more than once but with different data (e.g., Family
#   # is missing once), it wont be removed by the above
#   species_generic <- species_generic[!duplicated(dplyr::select(.data = species_generic,
#                                                                tidyselect::all_of(join_by))), ]
#
#   # Add species information to data
#   data_species <- dplyr::mutate_at(.data = data,
#                                    dplyr::across(.cols = data_code,
#                                                  .fns = toupper)) |>
#     dplyr::left_join(x = _,
#                      y = species_generic,
#                      by = join_by) |>
#     dplyr::distinct()
#
#   # We're hardcoding the fact that generics are always considered non-noxious
#   # for AIM and that generic shrubs are assumed to not be sagebrush.
#   if (species_info_sg_var %in% names(data)) {
#     if (verbose) {
#       message("Making sure that any records with generic shrub codes are classified as 'NonSagebrushShrub'")
#     }
#     test <- dplyr::mutate(.data = data,
#                           {{species_info_sg_var}} = dplyr::case_when(stringr::str_detect(string = code,
#                                                                                          pattern = generic_regex) & stringr::str_detect({{species_info_growthhabit_var}},
#                                                                                                                                         pattern = "shrub",
#                                                                                                                                         ignore.case = TRUE) ~ "NonSagebrushShrub",
#                                                                      .default = {{species_info_sg_var}}))
#   } else {
#
#   }
#   # #_#_#_#_#_#_#
#   # test_species <- data.frame(code = c("AF69420", "PF69420",
#   #                                     "AG69420", "PG69420",
#   #                                     "TR69420",
#   #                                     "SH69420",
#   #                                     "SU69420"),
#   #                            Duration = c(NA, "Biennial",
#   #                                         NA, NA,
#   #                                         NA,
#   #                                         NA,
#   #                                         NA),
#   #                            GrowthHabitSub = c("Moss", NA,
#   #                                               NA, NA,
#   #                                               NA,
#   #                                               NA,
#   #                                               NA))
#   #
#   # dplyr::mutate(.data = test_species,
#   #               Duration = dplyr::case_when(is.na(Duration) ~ stringr::str_to_title(stringr::str_replace_all(string = code,
#   #                                                                                                            pattern = setNames(object = names(generic_duration_regex_vector),
#   #                                                                                                                               nm = unname(generic_duration_regex_vector)))),
#   #                                           .default = Duration),
#   #               GrowthHabitSub = dplyr::case_when(is.na(GrowthHabitSub) ~ stringr::str_to_title(stringr::str_replace_all(string = code,
#   #                                                                                                                        pattern = setNames(object = names(generic_growthhabitsub_regex_vector),
#   #                                                                                                                                           nm = unname(generic_growthhabitsub_regex_vector)))),
#   #                                                 .default = GrowthHabitSub))
#   # #_#_#_#_#_#_#
#   # ## Merge unknown codes
#   # species_generic <- generic_growth_habits(
#   #   data = sf::st_drop_geometry(data), # in some applications, data will be an sf object
#   #   data_code = data_code,
#   #   species_list = species_list,
#   #   species_code = species_code,
#   #   species_growth_habit_code = species_growth_habit_code, # field name in species file of the species code to link to GrowthHabit
#   #   species_duration = species_duration # field name for duration
#   # )
#   #
#   #
#   #
#   # # check for duplicate species
#   # if (nrow(species_generic[duplicated(species_generic$Symbol), ]) > 0) {
#   #   warning("Duplicate species codes in the species file.
#   #           The first species occurrence will be used.")
#   #   message(species_generic[duplicated(species_generic$Symbol), ])
#   # }
#   #
#   #
#   # # message
#   # message("Merging data and species tables")
#   #
#   # ## Rename column
#   # species_generic <- species_generic %>%
#   #   dplyr::rename_at(dplyr::vars(species_code), ~data_code)
#   #
#   # ## Remove any duplicate values
#   # species_generic <- species_generic %>% dplyr::distinct()
#   #
#   # # If species are entered more than once but with different data (eg Family is missing once), it wont be removed by the above
#   # species_generic <-
#   #   species_generic[!duplicated(species_generic %>%
#   #                                 dplyr::select(all_of(join_by))),]
#   #
#   # # Add species information to data
#   # data_species <- dplyr::left_join(
#   #   x = data %>% dplyr::mutate_at(dplyr::vars(data_code), toupper),
#   #   y = species_generic,
#   #   by = join_by
#   # )
#   #
#   # data_species <- data_species %>% dplyr::distinct()
#
#   # Overwrite generic species assignments with provided table
#   if (overwrite_generic_species) {
#     ext <- substr(species_file, (nchar(species_file) - 2), nchar(species_file))
#     if(ext == "gdb"){
#       tbl_species_generic <- sf::st_read(
#         dsn = generic_species_file,
#         layer = "tblSpeciesGeneric",
#         stringsAsFactors = FALSE
#       )
#     } else if (ext == "csv"){
#       tbl_species_generic <- read.csv(generic_species_file)
#     } else {
#       stop("Unknown generic species list format. Must be a path to a geodatabase (.gdb) or comma-separated values file (.csv)")
#     }
#     # Read tblSpeciesGeneric
#     tbl_species_generic <- tbl_species_generic %>%
#       # Select only the needed fields
#       dplyr::select(
#         SpeciesCode,
#         # NOTE: WHY WAS DBKEY INCLUDED HERE????
#         # DBKey,
#         GrowthHabitCode,
#         Duration, SG_Group, Noxious
#       ) %>%
#       # Convert to character
#       dplyr::mutate_if(is.factor, as.character)
#
#     # Rename SpeciesCode to the data_code value
#
#     tbl_species_generic <- tbl_species_generic %>%
#       dplyr::rename_at("SpeciesCode", ~data_code)
#
#     # Join data_species to the generic species table
#     data_species_generic <- dplyr::left_join(
#       x = data_species,
#       y = tbl_species_generic,
#       by = c(data_code, "DBKey")
#     )
#
#     # Convert GrowthHabitCode to GrowthHabit and GrowthHabitSub
#     data_species_generic <- data_species_generic %>%
#       dplyr::mutate(
#         GrowthHabit = dplyr::recode(as.character(GrowthHabitCode),
#                                     "1" = "Woody",
#                                     "2" = "Woody",
#                                     "3" = "Woody",
#                                     "4" = "Woody",
#                                     "5" = "NonWoody",
#                                     "6" = "NonWoody",
#                                     "7" = "NonWoody",
#                                     .missing = as.character(GrowthHabit)
#         ),
#         GrowthHabitSub = dplyr::recode(as.character(GrowthHabitCode),
#                                        "1" = "Tree",
#                                        "2" = "Shrub",
#                                        "3" = "Subshrub",
#                                        "4" = "Succulent",
#                                        "5" = "Forb",
#                                        "6" = "Graminoid",
#                                        "7" = "Sedge",
#                                        .missing = as.character(GrowthHabitSub)
#         ),
#
#         # If the Duration assignments are different, overwrite
#         Duration = ifelse(Duration.x != as.character(Duration.y) & !is.na(Duration.y),
#                           Duration.y, Duration.x
#         ),
#
#         # If the SG_Group assignments are different, overwrite
#         SG_Group = ifelse(SG_Group.x != as.character(SG_Group.y) & !is.na(SG_Group.y),
#                           SG_Group.y, SG_Group.x
#         ),
#
#         # If the Noxious assignments are different, overwrite
#         Noxious = ifelse(Noxious.x != as.character(Noxious.y) & !is.na(Noxious.y),
#                          Noxious.y, Noxious.x
#         )
#       )
#
#     # Select only the fields from the original data_species file
#     data_species <- data_species_generic[, colnames(data_species)]
#   }
#
#   return(data_species)
# }
