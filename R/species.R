#' Gather species data
#' @param species.file Character string. The full file path (including file extension) to the csv containing the species list. If NULL then the file from the provided geodatabase will be used.
#' @param species.growth.habit.code Character. The field name for the growth habit codes in the species file.
#' @param growth.habit.file Character string. The full file path (including file extension) to the csv containing the growth habit list. If NULL then the file from the provided geodatabase will be used.
#' @param growth.habit.code Character. The field name for the growth habit codes in the growth habit file.
#' @param species.code Character. The field name for the species codes in the species file.
#' @param species.duration Character. the field name for the Duration field in the species file.
#' @name species
#' @export gather.species
#' @rdname gather.species

# Function to gather species information
gather.species <- function(species.file, #
                           species.growth.habit.code = "GrowthHabitSub",
                           growth.habit.file = "",
                           growth.habit.code = "Code" #
) {


  # check to see if the species file exists and read in the appropriate file type
  if (!file.exists(species.file)) {
    stop("The species file does not exist")
  }

  # read from .csv or .gdb
  species <- switch(toupper(stringr::str_extract(species.file, pattern = "[A-z]{3}$")),
    GDB = {
      suppressWarnings(sf::st_read(dsn = species.file, layer = "tblStateSpecies"))
    },
    CSV = {
      read.csv(species.file, stringsAsFactors = FALSE, na.strings = c("", " "))
    }
  )

  # Remove some of the gdb fields, as they cause issues later
  species <- species[, !colnames(species) %in% c("created_user", "created_date", "last_edited_user", "last_edited_date", "GlobalID")]
  # stop if there is no species .csv or .gdb file assigned
  if (is.null(species)) {
    stop("No valid Species Table. Must be .csv or .gdb file")
  }
  # read in the growth habit information
  growth.habit <- switch(toupper(stringr::str_extract(growth.habit.file, pattern = "[A-z]{3}$")),
    GDB = {
      suppressWarnings(sf::st_read(dsn = growth.habit.file, layer = "tblSpeciesGrowthHabit"))
    },
    CSV = {
      read.csv(growth.habit.file, stringsAsFactors = FALSE)
    }
  )
  # if there is no growth habit file provided, provide a warning. This is not a stop in case the growth habits were
  # assigned in the species file.
  if (is.null(growth.habit)) {
    warning("No valid Growth Habit Table. Must be .csv or .gdb file")
    return(species)
  } else {

    # rename spcies growth habits
    growth.habit <- growth.habit %>% dplyr::rename_at(dplyr::vars(growth.habit.code), ~species.growth.habit.code)

    # remove PrimaryKey, DBKey, and DateLoadedInDb if they exist
    growth.habit <- growth.habit[, !colnames(growth.habit) %in% c("DBKey", "PrimaryKey", "DateLoadedInDb")]

    # Merge species list and growth habit
    species.list <- dplyr::left_join(
      x = species[, !colnames(growth.habit) %in% "PrimaryKey"],
      y = growth.habit
    )


    return(species.list)
  }
}


#' @export generic.growth.habits
#' @rdname gather.species
#' Attribute generic species growth habits, for now this assumes field names.
generic.growth.habits <- function(data,
                                  data.code = "code", # Species field in the data
                                  species.list, # from  gather.species ()
                                  species.code = "SpeciesCode", # Species code value from species list
                                  species.growth.habit.code = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
                                  species.duration = "Duration" # field name for duration

) {
  generic.df <- data.frame(SpeciesFixed = unique(data[, colnames(data) == data.code])) %>%

    # Clean up the species codes
    dplyr::mutate(SpeciesFixed = toupper(SpeciesFixed) %>%
      stringr::str_replace_all(string = ., pattern = " |-", replacement = "")) %>%

    # Get unknown codes and clean them up. Unknown codes beging with a 2 (LMF/NRI) or a 2 letter prefix followed by a number.
    # Older projects also used "AAFF" etc. to identify unknown and dead beyond recognition codes. So we'll need to detect those too
    dplyr::filter(stringr::str_detect(SpeciesFixed, "^2|^[A-z]{2}[[:digit:]]|\\b(?=\\w*(^[A|P|S|T])\\1+)\\w+\\b")) %>%

    # Identify prefix
    dplyr::mutate(Code = gsub(SpeciesFixed, pattern = "[[:digit:]]", replacement = "") %>%
      gsub(., pattern = "([[:alpha:]])\\1+", replacement = "\\1") %>%
      as.character()) %>%

    # Rename to data species code field
    dplyr::rename_at(dplyr::vars(SpeciesFixed), ~data.code)


  # Merge with generic species definitions
  generic.code.df <- dplyr::inner_join(
    terradactyl::generic.species,
    generic.df
  )


  # Connect unknown codes to SpeciesState
  if ("SpeciesState" %in% colnames(species.list)) {
    generic.code.df <- generic.code.df %>%
      subset(!is.na(species.code)) %>%
      dplyr::inner_join (., dplyr::select(data, !!!dplyr::vars(data.code),
                                          SpeciesState)
    )
  }

  # Rename to SpeciesCode in species list
  generic.code.df <- generic.code.df %>%
    dplyr::rename_at(dplyr::vars(data.code), ~species.code)

   # Merge with main species list
  species.generic <- dplyr::full_join(species.list, generic.code.df)

  # Remove Code, Prefix, and PrimaryKey if they exist
  species.generic <- species.generic[, !colnames(species.generic) %in%
                                       c("Code", "PrimaryKey", "Prefix", "DateLoadedInDb")]


  return(species.generic)
}

#' @export species.join
#' @rdname gather.species

# Join species with field data
species.join <- function(data, # field data,
                         data.code = "code", # Species field in the data
                         species.file = "", # path to .csv or .gdb holding  the species table
                         species.code = "SpeciesCode", # field name in species file that identifies the species code
                         species.growth.habit.code = "GrowthHabitSub", # field name in species file of the species code to link to GrowthHabit
                         species.duration = "Duration", # field name in species file of the Duration assignment
                         growth.habit.file = "", # path to .csv or gdb holding tblSpeciesGrowthHabit
                         growth.habit.code = "Code") { # field name in growth habit file to link to GrowthHabit data table

  # Print
  print("Gathering species data")

  ## Load species data
  species <- gather.species(
    species.file = species.file,
    growth.habit.file = growth.habit.file,
    growth.habit.code = growth.habit.code,
    species.growth.habit.code = species.growth.habit.code
  )

  ## Merge unknown codes
  species.generic <- generic.growth.habits(
    data = as.data.frame(data), #in some applications, data will be an sf object
    data.code = data.code,
    species.list = species,
    species.code = species.code,
    species.growth.habit.code = species.growth.habit.code, # field name in species file of the species code to link to GrowthHabit
    species.duration = species.duration
  ) # field name for duration



  # check for duplicate species
  if (nrow(species.generic[duplicated(species.generic$Symbol), ]) > 0) {
    warning("Duplicate species codes in the species file. The first species occurrence will be used.")
    print(species.generic[duplicated(species.generic$Symbol), ])
  }


  # Print
  print("Merging data and species tables")

  ## Rename column
  species.generic <- species.generic %>% dplyr::rename_at(dplyr::vars(species.code), ~data.code)

  ## Add species information to LPI table
  data.species <- dplyr::left_join(
    x = data %>% dplyr::mutate_at(dplyr::vars(data.code), toupper),
    y = species.generic)

  return(data.species)
}
