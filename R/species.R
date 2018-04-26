##Species
#' @param species.file Character string. The full file path (including file extension) to the csv containing the species list. If NULL then the file from the provided geodatabase will be used.
#' @param species.growth.habit.code Character. The field name for the growth habit codes in the species file.
#' @param growth.habit.file Character string. The full file path (including file extension) to the csv containing the growth habit list. If NULL then the file from the provided geodatabase will be used.
#' @param growth.habit.code Character. The field name for the growth habit codes in the growth habit file.
#' @param recorded.species.codes Vector. Species recorded so that generic.growth.habit() can identify unknown codes.
#' @param species.code Character. The field name for the species codes in the species file.
#' @param species.duration Character. the field name for the Duration field in the species file.

#Function to gather species information
gather.species<-function(species.file,#path to .csv or .gdb holding  tblSpecies
                         species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                         growth.habit.file="", #path to .csv or gdb holding tblSpeciesGrowthHabit
                         growth.habit.code="Code" #field name in growth habit file to link to GrowthHabit
){


  #check to see if the species file exists and read in the appropriate file type
  if (!file.exists(species.file)){stop("The species file does not exist")}

  #read from .csv or .gdb
  species <- switch(toupper(stringr::str_extract(species.file, pattern = "[A-z]{3}$")),
                    GDB = {suppressWarnings(sf::st_read(dsn = species.file, layer = "tblSpecies"))},
                    CSV = {read.csv(species.file, stringsAsFactors = FALSE)})
  #stop if there is no species .csv or .gdb file assigned
  if (is.null(species)){
    stop("No valid Species Table. Must be .csv or .gdb file")
  }
  #read in the growth habit information
  growth.habit<-switch(toupper(stringr::str_extract(growth.habit.file, pattern = "[A-z]{3}$")),
                       GDB = {suppressWarnings(sf::st_read(dsn = growth.habit.file, layer = "tblSpeciesGrowthHabit"))},
                       CSV = {read.csv(growth.habit.file, stringsAsFactors = FALSE)})
  #if there is no growth habit file provided, provide a warning. This is not a stop in case the growth habits were
  #assigned in the species file.
  if (is.null(growth.habit)){
    warning("No valid Growth Habit Table. Must be .csv or .gdb file")
    return(species)
  } else {
    #merge species and growth habits
    species.list <- merge(x = species,
                     y = growth.habit,
                     by.x = species.growth.habit.code,
                     by.y = growth.habit.code,
                     all.x = TRUE,
                     allow.cartesian = TRUE)
    return(species.list)}
}



##Attribute generic species growth habits, for now this assumes field names.
generic.growth.habits<-function(recorded.species.codes, #string of all species codes that occur in a dataset
                                species.list, #from  gather.species ()
                                species.code="SpeciesCode", #Species code value from species list
                                species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                                species.duration="Duration" #field name for duration

){

  #get the unknown generic codes from the recorded species list
  Symbol<-recorded.species.codes[grepl("^[A-z]{2}[0-5000]|^2", recorded.species.codes)] %>%toupper %>%unique()%>% as.character()

  Prefix<-gsub(x=Symbol, pattern="[0-9]+", replacement="") %>% as.character()

  generic.df<-data.frame(Symbol, Prefix)
  colnames(generic.df)[1]<-species.code

  generic.code.df<-merge(terradactyl::generic.species,
                         generic.df,
                         by.y="Prefix",
                         by.x="Code",
                         all.y=TRUE)

  #Merge with main species list
  species.generic<-merge(species.list, generic.code.df, all=TRUE)

  return(species.generic)
}


#Join species with field data
species.join<-function(data, #field data,
                        data.code="code", #Species field in the data
                        species.file="",#path to .csv or .gdb holding  the species table
                        species.code="SpeciesCode", #field name in species file that identifies the species code
                        species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                        species.duration="Duration", #field name in species file of the Duration assignment
                        growth.habit.file="", #path to .csv or gdb holding tblSpeciesGrowthHabit
                        growth.habit.code="Code") { #field name in growth habit file to link to GrowthHabit data table


    #Print
    print("Gathering species data")

    ##Load species data
    species<-gather.species(species.file=species.file,
                            growth.habit.file = growth.habit.file,
                            growth.habit.code = growth.habit.code,
                            species.growth.habit.code = species.growth.habit.code)

    ##Merge unknown codes
    species.generic<-generic.growth.habits(recorded.species.codes=unique(data[,colnames(data)==data.code]),
                                           species.list=species,
                                           species.code = species.code,
                                           species.growth.habit.code=species.growth.habit.code, #field name in species file of the species code to link to GrowthHabit
                                           species.duration=species.duration) #field name for duration



    #check for duplicate species
    if (nrow(species.generic[duplicated(species.generic$Symbol),])>0){
      warning("Duplicate species codes in the species file. The first species occurrence will be used.")
      print(species.generic[duplicated(species.generic$Symbol),])
    }



    #Print
    print("Merging data and species tables")

    ## Add species information to LPI table
    data.species <- merge(x = data,
                              y = species.generic,
                              by.x = data.code,
                              by.y = species.code,
                              all.x = TRUE,
                              allow.cartesian = TRUE)
    return(data.species)
  }
