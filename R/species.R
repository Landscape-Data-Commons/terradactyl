#' Gather species attribute data
#' @description Gather species attributes and join to species observations.
#' @param species_file Character string. The full file path (including file extension)
#' to the file containing the species list OR the species list as a data frame.
#' @param species_growth_habit_code Character. The field name for the growth habit
#'  codes in the species file. Defaults to \code{"GrowthHabitSub"}
#' @param growth_habit_file Character string. The full file path (including file extension)
#' to the file containing the growth habit list. If \code{""} we assume the species list contains those values. Defaults to \code{""}.
#' @param growth_habit_code Character. The field name for the growth habit codes
#' in the growth habit file. Defaults to \code{"Code"}
#' @param species_code Character. The field name for the species codes in the species file.
#' @param species_duration Character. the field name for the Duration field in the species file.
#' @param data Dataframe containing species data
#' @param data_code Character. The field name with the species codes in the data.
#' @param species_list Dataframe. Species list output from \code{}
#' @param generic_species_file Character. The full file path (including file extension)to the file containing the species list.
#' @param by_species_key Logical. If \code{TRUE} then the join will attempt to use the variable \code{"SpeciesState"} if it exists. Defaults to \code{TRUE}.


#' @export gather_species
#' @rdname species

# Function to gather species information
gather_species <- function(species_file, #
                           species_growth_habit_code = "GrowthHabitSub",
                           growth_habit_file = "",
                           growth_habit_code = "Code" #
) {



  if (is.character(species_file)) {
    # check to see if the species file exists and read in the appropriate file type
    if (!file.exists(species_file)) {
      stop("The species file does not exist")
    }

    # read from .csv or .gdb. If gdb we assume it is of the schema aim.gdb
    species <- switch(toupper(stringr::str_extract(species_file,
                                                   pattern = "[A-z]{3}$"
    )),
    GDB = {
      suppressWarnings(sf::st_read(
        dsn = species_file,
        layer = "tblStateSpecies",
        stringsAsFactors = FALSE
      ))
    },
    CSV = {
      read.csv(species_file, stringsAsFactors = FALSE, na.strings = c("", " "))
    }
    )
  } else if (is.data.frame(species_file)) {
    species <- species_file
  }

  # Remove some of the gdb management variables, as they cause issues later
  species <- species[, !colnames(species) %in%
                       c(
                         "created_user", "created_date",
                         "last_edited_user", "last_edited_date", "GlobalID"
                       )]

  # stop if there is no species .csv or .gdb file assigned
  if (is.null(species)) {
    stop("No valid Species Table. Must be .csv or .gdb file")
  }
  # TODO Consider removing growth habit info
  # read in the growth habit information
  growth_habit <- switch(toupper(stringr::str_extract(growth_habit_file,
                                                      pattern = "[A-z]{3}$"
  )),
  GDB = {
    suppressWarnings(sf::st_read(
      dsn = growth_habit_file,
      layer = "tblSpeciesGrowthHabit",
      stringsAsFactors = FALSE
    ))
  },
  CSV = {
    read.csv(growth_habit_file, stringsAsFactors = FALSE)
  }
  )
  # if there is no growth habit file provided, provide a warning.
  # This is not a stop in case the growth habits were
  # assigned in the species file.
  if (is.null(growth_habit)) {
    # convert factors to character
    species <- species %>% dplyr::mutate_if(is.factor, as.character) %>%
      # remove white space
      dplyr::mutate_if(is.character, stringr::str_trim)

    # Remove NA from species
    species <- species %>% dplyr::filter(!is.na(dplyr::vars(species_code)))
  } else {

    # rename spcies growth habits
    growth_habit <- growth_habit %>%
      dplyr::rename_at(
        dplyr::vars(growth_habit_code),
        ~species_growth_habit_code
      )

    # remove PrimaryKey, DBKey, and DateLoadedInDb if they exist
    growth_habit <- growth_habit[, !colnames(growth_habit) %in%
                                   c("DBKey", "PrimaryKey", "DateLoadedInDb")]

    # Merge species list and growth habit
    species_list <- dplyr::left_join(
      x = species[, !colnames(growth_habit) %in% "PrimaryKey"],
      y = growth_habit
    )
    # convert factors to character
    species_list <- species_list %>% dplyr::mutate_if(is.factor, as.character) %>%
      # remove white space
      dplyr::mutate_if(is.character, stringr::str_trim)

    # Remove NA from species
    species_list <- species_list %>% dplyr::filter(!is.na(dplyr::vars(species_code)))
  }
}


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
    SpeciesFixed = unique(data[, colnames(data) == data_code]),
    SpeciesOriginal = unique(data[, colnames(data) == data_code])
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
    generic.code.df <- generic.code.df %>%
      subset(!is.na(species_code)) %>%
      dplyr::inner_join(., dplyr::select(
        data, !!!dplyr::vars(data_code),
        "SpeciesState"
      ))
  } else {
    warning("Variable 'SpeciesState' is not present in either the data or the lookup table")
    generic.code.df <- generic.code.df %>%
      subset(!is.na(species_code)) %>%
      dplyr::inner_join(., dplyr::select(
        data, !!!dplyr::vars(data_code)
      ))
  }


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
                         by_species_key = TRUE) {

  # Print
  print("Gathering species data")

  # Set join levels, so that we can flexibly include SpeciesState
  if (by_species_key) {
    if ("SpeciesState" %in% names(data)) {
      join_by <- c(data_code, "SpeciesState")
    } else {
      join_by <- data_code
    }
  } else {
    join_by <- data_code
  }


  # Some projects use "None" to indicate "No species". Convert those to N instead
  data <- data %>% dplyr::mutate_at(
    data_code,
    ~ stringr::str_replace(
      pattern = "None",
      replacement = "N",
      string = data[[data_code]]
    )
  )
  ## Load species data
  species_list <- gather_species(
    species_file = species_file,
    growth_habit_file = growth_habit_file,
    growth_habit_code = growth_habit_code,
    species_growth_habit_code = species_growth_habit_code
  )

  # clean up NA values in species list
  species_list <- species_list %>%
    dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
    dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "NA")))


  # Look for UpdatedSpecies and Update the Observation codes, if necessary
  if ("UpdatedSpeciesCode" %in% names(species_list)) {
    if (any(!is.na(species_list$UpdatedSpeciesCode))) {

      ## Rename column
      species_list <- species_list %>%
        dplyr::rename_at(dplyr::vars(species_code), ~data_code)

      # Make sure Updated Species Code is a character vector
      species_list$UpdatedSpeciesCode <- as.character(species_list$UpdatedSpeciesCode)

      # Merge the Updated Species codes to the data
      if (by_species_key) {
        data_update <- dplyr::left_join(data,
                                        dplyr::select(
                                          species_list, data_code,
                                          UpdatedSpeciesCode, SpeciesState
                                        ),
                                        by = join_by
        )
      } else {
        data_update <- dplyr::left_join(data,
                                        dplyr::select(
                                          species_list, data_code,
                                          UpdatedSpeciesCode
                                        ),
                                        by = join_by
        )
      }


      # Overwrite the original data code with any updated species codes
      data_update <- data_update %>%
        dplyr::mutate_at(
          data_code,
          ~ dplyr::coalesce(
            data_update$UpdatedSpeciesCode,
            data_update[[data_code]]
          )
        )

      # Overwrite original data with updated data
      data <- data_update %>% dplyr::select(names(data))

      # Rename species_list
      ## Rename column
      species_list <- species_list %>%
        dplyr::rename_at(dplyr::vars(data_code), ~species_code)
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



  # check for duplicate species
  if (nrow(species_generic[duplicated(species_generic$Symbol), ]) > 0) {
    warning("Duplicate species codes in the species file.
            The first species occurrence will be used.")
    print(species_generic[duplicated(species_generic$Symbol), ])
  }


  # Print
  print("Merging data and species tables")

  ## Rename column
  species_generic <- species_generic %>%
    dplyr::rename_at(dplyr::vars(species_code), ~data_code)

  ## Remove any duplicate values
  species_generic <- species_generic %>% dplyr::distinct()

  # Add species information to data
  data_species <- dplyr::left_join(
    x = data %>% dplyr::mutate_at(dplyr::vars(data_code), toupper),
    y = species_generic,
    by = join_by
  )

  data_species <- data_species %>% dplyr::distinct()


  # Overwrite generic species assignments with provided table
  if (overwrite_generic_species) {
    # Read tblSpeciesGeneric
    tbl_species_generic <- sf::st_read(
      dsn = species_file,
      layer = "tblSpeciesGeneric",
      stringsAsFactors = FALSE
    ) %>%
      # Select only the needed fields
      dplyr::select(
        SpeciesCode, DBKey, GrowthHabitCode,
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
      by = c(data_code, "DBKey")
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
