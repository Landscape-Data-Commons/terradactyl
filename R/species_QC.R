# Species Checks--Check that the species list covers all observed species
#' @param dsn_tall The observed data data source
#' @param species_list_file The file path to the species list. If the dsn contains the species list, then specify the layer within the dsn.

#' @export species_list_check
#' @rdname species
species_list_check <- function(dsn_tall, species_list_file, ...) {

  ### Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # Read header information to provide subset and link between species tables
  header <- readRDS(paste(dsn_tall, "header.Rdata", sep = ""))

  # Read in LPI
  lpi <- readRDS(paste(dsn_tall, "lpi_tall.Rdata", sep = ""))


  # Read in height
  height <- readRDS(paste(dsn_tall, "height_tall.Rdata", sep = ""))

  # Species inventory
  spp_inventory <- readRDS(paste(dsn_tall, "spp_inventory_tall.Rdata", sep = ""))


  # Filter header according to filter expressions
  header_sub <- header %>% dplyr::filter(!!!filter_exprs)


  # Merge all Species together with header data

  species_all <- dplyr::bind_rows(
    dplyr::select(lpi, PrimaryKey, Species = code),
    dplyr::select(height, PrimaryKey, Species),
    dplyr::select(spp_inventory, PrimaryKey, Species)
  ) %>%
    subset(nchar(Species) >= 3 & Species != "None") %>%
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
      Noxious, SpeciesState, source
    ) %>%
    dplyr::filter_at(
      dplyr::vars(
        GrowthHabit, GrowthHabitSub, Duration,
        Noxious
      ),
      dplyr::any_vars(!is.na(.))
    )

  species_hits <- species_issues %>%
    dplyr::group_by(Species, source) %>%
    dplyr::summarise(n_hits = dplyr::n())

  unique_species_issues <- species_issues %>%
    dplyr::select(-PrimaryKey, -PlotID) %>%
    dplyr::distinct() %>%
    dplyr::left_join(., species_hits, by = c("Species", "source"))

  # Write species level problems
  write.csv(unique_species_issues,
            file = paste(dirname(species_list_file), "/",
                         unique(unique_species_issues$SpeciesState)[1],
                         "_species_missing_attributes_",
                         Sys.Date(), ".csv",
                         sep = ""
            )
  )
  # Write out all data with problems
  write.csv(species_issues,
            file = paste(dirname(species_list_file), "/",
                         unique(species_issues$SpeciesState)[1],
                         "_species_missing_attributes_plots",
                         Sys.Date(), ".csv",
                         sep = ""
            )
  )

  # If the height was missclassified (e.g., herbaceous species in woody height,
  # drop it)
  # convert herbaceous to "non-woody"
  height.mismatched.growth.habit <- height %>%
    dplyr::left_join(header_sub, .) %>%
    species_join(
      data = ., data_code = "Species",
      species_file = species_list_file
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
  species_list <- switch(stringr::str_sub(species_list_file, start = -3),
    "gdb" = sf::st_read(dsn = species_list_file, layer="tblStateSpecies"),
    "csv" = read.csv(species_list_file))


  species_list <- species_list %>% dplyr::mutate_all(dplyr::funs(toupper))

  growth_habit <- terradactyl::species_attributes$GrowthHabit %>%
    unique() %>% toupper()
  duration <- terradactyl::species_attributes$Duration %>%
    unique() %>% toupper()
  noxious <- terradactyl::species_attributes$Noxious %>%
    unique() %>% toupper()
  growth_habit_sub <- terradactyl::species_attributes$GrowthHabitSub %>%
    unique() %>%
    toupper()
  sg_group <- terradactyl::species_attributes$SG_GROUP %>%
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
      unique(),
    SG_Group  = species_list$SG_Group[!species_list$SG_Group %in% sg_group] %>%
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
      subset(!Noxious %in% noxious) %>%
      dplyr::mutate(Error = "Invalid Noxious"),

    species_list %>%
      subset(!SG_Group %in% sg_group) %>%
      dplyr::mutate(Error = "Invalid SG_Group")
  )

  write.csv(bad_attributes,
            file = paste(dirname(species_list_file), "/",
                         unique(unique_species_issues$SpeciesState)[1],
                         "_bad_attributes_",
                         Sys.Date(), ".csv",
                         sep = ""
            ))

}

####
