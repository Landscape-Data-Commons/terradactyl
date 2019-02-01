# Species Checks--Check that the species list covers all observed species
#' @param dsn_tall The observed data datasource
#' @param species_list_file The file path to the species list. If the dsn contains the species list, then specify the layer within the dsn.

#' @export species_list_check
#' @rdname species
species_list_check <- function(dsn_tall, species_list_file, ...) {

  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # Read header information to provide subset and link between species tables
  load(paste(dsn_tall, "header.Rdata", sep = ""))

  # Read in LPI
  load(paste(dsn_tall, "lpi_tall.Rdata", sep = ""))


  # Read in height
  load(paste(dsn_tall, "height_tall.Rdata", sep = ""))

  # Species inventory
  load(paste(dsn_tall, "spp_inventory_tall.Rdata", sep = ""))


  # Filter header according to filter expressions
  header_sub <- header %>% dplyr::filter(!!!filter_exprs)


  # Merge all Species together with header data

  species_all <- dplyr::bind_rows(
    dplyr::select(lpi, PrimaryKey, Species = code),
    dplyr::select(height, PrimaryKey, Species),
    dplyr::select(spp_inventory, PrimaryKey, Species)
  ) %>%
    subset(nchar(Species) >= 3) %>%
    dplyr::distinct() %>%
    dplyr::left_join(header_sub, .) %>%


    # Join to species
    species_join(
      data = ., data_code = "Species",
      species_file = species_list_file
    )


  # Determine which species are missing GrowthHabit, GrowthHabitSub, Duration,
  # or Noxious Assignments

  species_all_problems <- species_all %>% dplyr::mutate(
    GrowthHabit =
      dplyr::case_when(
        is.na(GrowthHabit) ~ "GrowthHabit missing"
      ),
    GrowthHabitSub = dplyr::case_when(
      is.na(GrowthHabitSub) ~ "GrowthHabitSub missing"
    ),
    Duration =
      dplyr::case_when(
        is.na(Duration) ~ "Duration missing"
      ),
    Noxious = dplyr::case_when(
      is.na(Noxious) ~ "Noxious missing"
    )
  )



  species_issues <- species_all_problems %>%
    dplyr::select(
      PrimaryKey, PlotID, Species, GrowthHabit, GrowthHabitSub, Duration,
      Noxious, SpeciesState
    ) %>%
    dplyr::filter_at(
      dplyr::vars(
        GrowthHabit, GrowthHabitSub, Duration,
        Noxious
      ),
      dplyr::any_vars(!is.na(.))
    )

  species_hits <- species_issues %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(n_hits = n())

  unique_species_issues <- species_issues %>%
    dplyr::select(-PrimaryKey, -PlotID) %>%
    dplyr::distinct() %>%
    dplyr::left_join(., species_hits, by = "Species")


  write.csv(unique_species_issues,
            file = paste(dirname(species_list_file), "/",
                         unique(unique_species_issues$SpeciesState)[1],
                         "_species_missing_attributes_",
                         Sys.Date(), ".csv",
                         sep = ""
            )
  )


  # If the height was missclassified (e.g., herbaceous species in woody height,
  # drop it)
  # convert herbaceous to "non-woody"
  height.mismatched.growth.habit <- height %>%
    dplyr::left_join(header_sub, .) %>%
    species.join(
      data = ., data.code = "Species",
      species.file = species_list_file
    ) %>%
    dplyr::filter(toupper(GrowthHabit_measured) != toupper(GrowthHabit) | is.na(GrowthHabit)) %>%

    # Remove all instances where it fails because there is an NA in Species
    dplyr::filter(!is.na(Species)) %>%

    # Remove all instances where GrowthHabit is NA
    dplyr::filter(!is.na(GrowthHabit)) %>%

    # Make the number of columns more manageable
    dplyr::select(
      PrimaryKey, PlotID, Height, Species, GrowthHabit_measured,
      GrowthHabit, ScientificName, CommonName, Duration, Noxious, SG_Group
    )



  write.csv(height.mismatched.growth.habit,
    file = paste(dirname(species_list_file),
      "/",
      unique(unique_species_issues$SpeciesState)[1],
      "_height_mismatched_growth_habit_",
      Sys.Date(), ".csv",
      sep = ""
    )
  )


  # Evaluate the codes used in the species list
  species_list <- read.csv(species_list_file)

  species_list <- species_list %>% dplyr::mutate_all(dplyr::funs(toupper))

  growth_habit <- terradactyl::generic.codes$GrowthHabit %>% unique() %>% toupper()
  duration <- terradactyl::generic.codes$Duration %>% unique() %>% toupper()
  noxious <- c("YES", "NO")
  growth_habit_sub <- terradactyl::generic.codes$GrowthHabitSub %>%
    unique() %>%
    toupper()


  bad.growth <- list(
    GrowthHabit = species_list$GrowthHabit[
      !species_list$GrowthHabit %in% growth_habit
    ] %>%
      unique(),
    GrowthHabitSub = species_list$GrowthHabitSub[
      !species_list$GrowthHabitSub %in% growth_habit_sub
    ] %>%
      unique(),
    Duration = species_list$Duration[!species_list$Duration %in% duration] %>%
      unique(),
    Noxious = species_list$Noxious[!species_list$Noxious %in% noxious] %>%
      unique()
  )



  bad_attributes <- rbind(
    species_list %>%
      subset(!GrowthHabit %in% growth_habit) %>%
      dplyr::mutate(Error = "Invalid GrowthHabit"),

    species_list %>%
      subset(!GrowthHabitSub %in% growth_habit_sub) %>%
      dplyr::mutate(Error = "Invalid GrowthHabitSub"),

    species_list %>%
      subset(!Duration %in% duration) %>%
      dplyr::mutate(Error = "Invalid Duration"),

    species_list %>%
      subset(!GrowthHabit %in% growth_habit) %>%
      dplyr::mutate(Error = "Invalid Noxious")
  )

  print("Check for bad species attributes")

  return(bad_attributes)
}

####
