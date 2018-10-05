# Species Checks--Check that the species list covers all observed species
#' @param dsn The observed data datasource
#' @param species_list_file The file path to the species list. If the dsn contains the species list, then specify the layer within the dsn.

#' @export species_list_file
#' @rdname species
species_list_check<-function(dsn, species.list.file, source = "AIM", ...) {

  ###Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs<-rlang::quos(...)

  if (source == "AIM") {

    #tblPlots provides the link between species tables (LPI, Height, Species Richness) and tblStateSpecies
    plots<-sf::read_sf(dsn=dsn, layer="tblPlots") %>% as.data.frame

    #Identify PKs to filter on
    PK<-plots %>% dplyr::filter(!!!filter_exprs) %>%
      dplyr::select(PrimaryKey, SpeciesState)

    #Gather LPI, height, and species inventory
    #PI
    lpi.tall<-gather.lpi(dsn = dsn, source = source) %>%
      #Subset to study area
      dplyr::left_join(PK, .) %>%
      #Join to species
    lpi.tall <- lpi.tall %>%  species.join(data = .,
                                           data.code = "code",
                                           species.file = species.list.file)

    #Height
    height.tall <-gather.height(dsn = dsn, source = source) %>%
      #Subset to study area
      dplyr::left_join(PK, .) %>%
      #Join to species
      species.join(data = ., data.code = "Species", species.file = species.list.file)

    #Species Inventory
    spp.inventory <-gather.species.inventory(dsn = dsn, source = source) %>%
      #Subset to study area
      dplyr::left_join(PK, .) %>%
      #Join to species
      species.join(data = ., data.code = "Species", species.file = species.list.file)


    all.species <-rbind(dplyr::select(lpi.tall, Species = code, GrowthHabit:Duration),
                        dplyr::select(height.tall, Species, GrowthHabit:Duration),
                        dplyr::select(spp.inventory, Species, GrowthHabit:Duration)) %>%
      subset(nchar(Species) >=3) %>% dplyr::distinct()

    write.csv(all.species, "all.species_AIM.csv")
    shell.exec("all.species_AIM.csv")


    #If the height was missclassified (e.g., herbaceous species in woody height, drop it)
    #convert herbaceous to "non-woody"
    height.mismatched.growth.habit<-height.tall %>%
      dplyr::filter(!toupper(GrowthHabit_measured) != toupper(GrowthHabit)|is.na(GrowthHabit))

    #Check for unassigned species info

    if(!"Noxious" %in% colnames(lpi.tall)){
      lpi.tall$Noxious <- NA
    }

    unassigned.species.info<-lpi.tall %>%
      subset(is.na(Duration)|is.na(GrowthHabitSub)|is.na(Noxious)) %>%
      #cut out species codes <3 characters because those are soil codes
      subset(nchar(code)>=3) %>%
      #Get the relevant fields
      dplyr::select(PrimaryKey, DBKey,
                    SpeciesState, LineKey, RecKey, PointLoc,
                    layer, code, GrowthHabit:CommonName)


    if(any(nrow(height.mismatched.growth.habit) >0,nrow(unassigned.species.info)>0)){
      if(nrow(height.mismatched.growth.habit) >0){
        #set column names
        column_names <- names(height.mismatched.growth.habit)[
          names(height.mismatched.growth.habit) %in%
            c("DBKey", "SpeciesState", "PrimaryKey", "LineKey", "RecKey",
              "Species", "ScientificName", "GrowthHabit_measured", "GrowthHabit",
              "GrowthHabitSub", "Duration", "Noxious")]

        #Save the mismatched growth habits to a .csv and prompt csv to open
        write.csv(dplyr::select(height.mismatched.growth.habit,
                                !!!column_names),
                  "height.mismatched.growth.habits_AIM.csv")
        shell.exec("height.mismatched.growth.habits_AIM.csv")

        warning("There are species in height with mismatched growth habits to the provided species list. See csv for details")
      }

      if(nrow(unassigned.species.info)>0) {
        #save the unassigned species information as a CSV and prompt the csv to open
        write.csv(unassigned.species.info, "incomplete.species.detail_AIM.csv")
        shell.exec("incomplete.species.detail_AIM.csv")

        #save the unassigned species info, a unique list of codes and prompt the csv to open
        write.csv(unique(unassigned.species.info$code), "incomplete.species.distinct_AIM.csv")
        shell.exec("incomplete.species.distinct_AIM.csv")
        stop("There are species in the data with incomplete species attributes See .csv output for details")

      }
    }

  }
  if (source == "LMF") {
    #tblPlots provides the link between species tables (LPI, Height, Species Richness) and tblStateSpecies
    plots<-sf::read_sf(dsn=dsn, layer="POINT") %>% as.data.frame

    #Identify PKs to filter on
    PK<-plots %>% dplyr::filter(!!!filter_exprs) %>%
      dplyr::select(PrimaryKey, SpeciesState)


    #Gather LPI, height, and species inventory
    #PI
    lpi.tall<-gather.lpi(dsn = dsn, source = source) %>%
      #Subset to study area
      dplyr::left_join(PK, .) %>%
      #Join to species
      species.join(data = ., data.code = "code", species.file = species.list.file)

    #Height
    height.tall <-gather.height(dsn = dsn, source = source) %>%
      #Subset to study area
      dplyr::left_join(PK, .) %>%
      #Join to species
      species.join(data = ., data.code = "Species", species.file = species.list.file)

    #Species Inventory
    spp.inventory <-gather.species.inventory(dsn = dsn, source = source) %>%
      #Subset to study area
      dplyr::left_join(PK, .) %>%
      #Join to species
      species.join(data = ., data.code = "Species",
                   species.file = species.list.file)



    all.species <-rbind(dplyr::select(lpi.tall, Species = code, GrowthHabit:Duration),
                        dplyr::select(height.tall, Species, GrowthHabit:Duration),
                        dplyr::select(spp.inventory, Species, GrowthHabit:Duration)) %>%
      subset(nchar(Species) >=3) %>% dplyr::distinct()

    write.csv(all.species, "all.species_LMF.csv")
    shell.exec("all.species_LMF.csv")


    #If the height was missclassified (e.g., herbaceous species in woody height, drop it)
    #convert herbaceous to "non-woody"
    height.mismatched.growth.habit<-height.tall %>%
      dplyr::filter(!toupper(GrowthHabit_measured) != toupper(GrowthHabit)|is.na(GrowthHabit))

    #Check for unassigned species info

    if(!"Noxious" %in% colnames(lpi.tall)){
      lpi.tall$Noxious <- NA
    }

    unassigned.species.info<-lpi.tall %>%
      subset(is.na(Duration)|is.na(GrowthHabitSub)|is.na(Noxious)) %>%
      #cut out species codes <3 characters because those are soil codes
      subset(nchar(code)>=3) %>%
      #Get the relevant fields
      dplyr::select(PrimaryKey, DBKey,
                    SpeciesState, PointNbr,
                    layer, code, GrowthHabit:CommonName)


    if(any(nrow(height.mismatched.growth.habit) >0,nrow(unassigned.species.info)>0)){
      if(nrow(height.mismatched.growth.habit) >0){
        #set column names
        column_names <- names(height.mismatched.growth.habit)[
          names(height.mismatched.growth.habit) %in%
            c("DBKey", "SpeciesState", "PrimaryKey", "LineKey", "RecKey",
              "Species", "ScientificName", "GrowthHabit_measured", "GrowthHabit",
              "GrowthHabitSub", "Duration", "Noxious")]

        #Save the mismatched growth habits to a .csv and prompt csv to open
        write.csv(dplyr::select(height.mismatched.growth.habit,
                                !!!column_names),
                  "height.mismatched.growth.habits_LMF.csv")
        shell.exec("height.mismatched.growth.habits_LMF.csv")

        warning("There are species in height with mismatched growth habits to the provided species list. See csv for details")
      }

      if(nrow(unassigned.species.info)>0) {
        #save the unassigned species information as a CSV and prompt the csv to open
        write.csv(unassigned.species.info, "incomplete.species.detail_LMF.csv")
        shell.exec("incomplete.species.detail_LMF.csv")

        #save the unassigned species info, a unique list of codes and prompt the csv to open
        write.csv(unique(unassigned.species.info$code), "incomplete.species.distinct_LMF.csv")
        shell.exec("incomplete.species.distinct_LMF.csv")
        stop("There are species in the data with incomplete species attributes See .csv output for details")

      }

    }



}

  # Evaluate growth habits
  species_list <- read.csv(species.list.file)

  species_list <- dplyr::mutate_all(toupper())

  growth_habit <- terradactyl::generic.codes$GrowthHabit %>% unique %>% toupper
  duration <- terradactyl::generic.codes$GrowthHabitSub %>% unique %>% toupper
  noxious <- c("YES", "NO")
  growth_habit_sub <- terradactyl::generic.codes$GrowthHabitSub %>%
    unique %>% toupper

  bad_attributes <- rbind(
    species_list %>% subset(!GrowthHabit %in% growth_habit) %>%
      dplyr::mutate(Error = "Invalid GrowthHabit"),

    species_list %>% subset(!GrowthHabitSub %in% growth_habit_sub) %>%
      dplyr::mutate(Error = "Invalid GrowthHabitSub"),

    species_list %>% subset(!Duration %in% duration) %>%
      dplyr::mutate(Error = "Invalid Duration"),

    species_list %>% subset(!GrowthHabit %in% growth_habit) %>%
      dplyr::mutate(Error = "Invalid Noxious")

  )

  write.csv(bad_attributes, "bad_attributes.csv")
  shell.exec("bad.attributes.csv")

}

####
