#' Gather LPI data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @param species.file Character string. The full file path (including file extension) to the csv containing the species list. If NULL then the file from the provided geodatabase will be used.
#' @param species.growth.habit.code Character. The field name for the growth habit codes in the species file.
#' @param growth.habit.file Character string. The full file path (including file extension) to the csv containing the growth habit list. If NULL then the file from the provided geodatabase will be used.
#' @param growth.habit.code Character. The field name for the growth habit codes in the growth habit file.
#' @param recorded.species.codes Vector. Species recorded so that generic.growth.habit() can identify unknown codes.
#' @param species.code Character. The field name for the species codes in the species file.
#' @param species.duration Character. the field name for the Duration field in the species file.
#' @return A list of two data frames: one containing the data from the LPI pin intercepts and one containing the data from the height methd done alongside pin drops.
#' @export


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
    species <- merge(x = species,
                     y = growth.habit,
                     by.x = species.growth.habit.code,
                     by.y = growth.habit.code,
                     all.x = TRUE,
                     allow.cartesian = TRUE)
    return(species)}
  }



##Attribute generic species growth habits, for now this assumes field names.
generic.growth.habits<-function(recorded.species.codes, #string of all species codes that occur in a dataset
                                species.list, #from  gather.species ()
                                species.code="SpeciesCode", #Species code value from species list
                                species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                                species.duration="Duration" #field name for duration

                               ){

  #get the unknown generic codes from the recorded species list
  generic.codes<-recorded.species.codes[grep("^[A-z]{2}[0-5000]", recorded.species.codes)] %>%toupper %>%unique()
  #Assign duration
  Duration<-ifelse(grepl("^A",generic.codes), "Annual", "Perennial")
  #Assign Growth Habit
  GrowthHabit<-ifelse(grepl("^SH|TR|SU|SS", generic.codes), "Woody", "Non-woody")
  #Assign Growth Habit Sub-code
  GrowthHabitSub<-generic.codes %>% gsub(pattern="[[:digit:]]", replacement="") %>%
    stringr::str_replace_all("SH", "Shrub")%>%
    stringr::str_replace_all("SS", "Sub-Shrub")%>%
    stringr::str_replace_all("TR", "Tree")%>%
    stringr::str_replace_all("SU", "Succulent")%>%
      stringr::str_replace_all("PF|AF", "Forb/herb")%>%
      stringr::str_replace_all("PG|AG", "Graminoid")
  #Create data frame
  GrowthHabitCode<-generic.codes %>% gsub(pattern="[[:digit:]]", replacement="") %>%
    stringr::str_replace_all("SH", "2")%>%
    stringr::str_replace_all("SS", "3")%>%
    stringr::str_replace_all("TR", "1")%>%
    stringr::str_replace_all("SU", "4")%>%
    stringr::str_replace_all("PF|AF", "5")%>%
    stringr::str_replace_all("PG|AG", "6")

  #Assign DIMA growth habit code
  generic.growth.habit<-data.frame(SpeciesCode=generic.codes, Duration, GrowthHabit, GrowthHabitSub, GrowthHabitCode, stringsAsFactors = FALSE)

  #Remove non-standard generic codes that may be left
  suppressWarnings(generic.growth.habit$GrowthHabitCode<-as.integer(generic.growth.habit$GrowthHabitCode)) #suppressed because NAs may be introduced, but we deal with that on the next line
  generic.growth.habit<-generic.growth.habit[!is.na(generic.growth.habit$GrowthHabitCode),] %>%unique()


  #Rename fields so they are compatible with the main species list
 generic.growth.habit<-plyr::rename(generic.growth.habit, c("SpeciesCode"=species.code, "Duration"=species.duration, "GrowthHabitSub"=species.growth.habit.code))
  #Merge with main species list
  species.generic<-dplyr::bind_rows(species.list, generic.growth.habit)

  return(species.generic)
}

##Function to make tall format of LPI data
gather.lpi <- function(dsn,
                       species.characteristics = TRUE,
                       species.file="",#path to .csv or .gdb holding  the species table
                       species.code="SpeciesCode", #field name in species file that identifies the species code
                       species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                       species.duration="Duration", #field name in species file of the Duration assignment
                       growth.habit.file="", #path to .csv or gdb holding tblSpeciesGrowthHabit
                       growth.habit.code="Code") { #field name in growth habit file to link to GrowthHabit

  #Read LPI information from TerrADat
  lpi.detail <- suppressWarnings(sf::st_read(dsn=dsn, layer = "tblLPIDetail"))
  lpi.header<-suppressWarnings(sf::st_read(dsn=dsn, layer = "tblLPIHeader"))

  ## Make a tall data frame with the hit codes by layer and the checkbox designation
  lpi.hits.tall<-data.table::melt(data=lpi.detail,
                                  id.vars=c("PrimaryKey","PointLoc","PointNbr","RecKey"),
                                  measure.vars=c("TopCanopy", "SoilSurface",
                                                 colnames(lpi.detail)[grepl(pattern="^Lower[1-7]$", x=colnames(lpi.detail))]),
                                  variable.name="layer",
                                  value.name="code",
                                  na.rm=TRUE)

  #Remove all records where no hit was recorded (e.g., "None", "NA"
  lpi.hits.tall <- dplyr::filter(.data = lpi.hits.tall,
                            !is.na(code),
                            code != "",
                            code != "None",
                            !is.na(PrimaryKey),
                            !is.na(RecKey))


  ## Make a tall data framethe checkbox status by layer
  lpi.chkbox.tall <- data.table::melt(data=lpi.detail,
                                      id.vars=c("PrimaryKey", "PointLoc","PointNbr", "RecKey"),
                                      measure.vars=colnames(lpi.detail)[grepl(pattern="^Chkbox", x=colnames(lpi.detail))],
                                      variable.name="layer",
                                      value.name="chckbox")

  #Remove Woody and Herbaceous Checkbox
  lpi.chkbox.tall<-lpi.chkbox.tall[!(lpi.chkbox.tall$chckbox%in%c("ChckboxWoody", "ChckboxHerbaceous")),]

  ## Make the names in the layer variable match
  lpi.chkbox.tall$layer <- stringr::str_replace_all(string = lpi.chkbox.tall$layer,
                                                    pattern = "^Chkbox",
                                                    replacement = "")

  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Top"] <- "TopCanopy"
  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Soil"] <- "SoilSurface"

  #Print update because this function can take a while
  print("Merging LPI Header and LPI Detail tables")
  #Merge checkbox and hit data as well as the header data
  lpi.tall<-suppressWarnings(dplyr::left_join(x = lpi.hits.tall,y = lpi.chkbox.tall, all.x=TRUE,by=c("PrimaryKey", "PointLoc","PointNbr","RecKey", "layer"))%>%
    dplyr::left_join(select(lpi.header, LineKey:CheckboxLabel, PrimaryKey), ., by=c("PrimaryKey", "RecKey")))


  ## If we're adding species

  if (species.characteristics) {
    #check for the source of the species data
    if (is.null(species.file)){
      species.file<-dsn
    }
    if (is.null(species.file)){
      growth.habit.file<-dsn
    }

    #Print
    print("Gathering species data")
    ##Load species data
    species<-gather.species(species.file=species.file,
                            growth.habit.file = growth.habit.file,
                            growth.habit.code = growth.habit.code,
                            species.growth.habit.code = species.growth.habit.code)

    ##Merge unknown codes
    species.generic<-generic.growth.habits(recorded.species.codes=lpi.tall$code,
                                   species.list=species,
                                   species.code = species.code,
                                   species.growth.habit.code=species.growth.habit.code, #field name in species file of the species code to link to GrowthHabit
                                   species.duration=species.duration #field name for duration
                                   )


    #check for duplicate species
    if (nrow(species.generic[duplicated(species.generic$Symbol),]>0)){
      warning("Duplicate species codes in the species file. The first species occurrence will be used.")
      print(species.generic[duplicated(species.generic$Symbol),])
    }



    #Print
    print("Merging LPI and species tables")

    ## Add species information to LPI table
    lpi.tall.species <- merge(x = lpi.tall,
                      y = species.generic,
                      by.x = "code",
                      by.y = species.code,
                      all.x = TRUE,
                      allow.cartesian = TRUE)
    return(lpi.tall.species)
  }
    return(lpi.tall)
  ## Output the list

}

gather.lpi.lmf<-function(dsn){
  #Read LMF PINTERCEPT table
  pintercept<-sf::st_read(dsn, layer="PINTERCEPT", stringsAsFactors=FALSE)

  #For line point intercept data (cover calculations--point number 75 is recorded twice—once on each transect.
  #We only want to use it once in the calculations.
  #Prior to doing these calculations, it would be beneficial to remove one of the point 75’s from the data set.
  #Remove the nesw transect—that would be all rows in pintercept where column 6 = “nesw” AND column 7 = 75.
  pintercept<-pintercept%>%subset(!(MARK==75&TRANSECT=="nesw"))

  #Where there is a Soil hit, LMF records "None" in BASAL and leaves NONSOIL blank. Let's fill in an "S" to indicate soil
  pintercept$NONSOIL[pintercept$BASAL=="None" & pintercept$NONSOIL==""]<-"S"

  #Create a tall table
  lpi.hits.tall<-data.table::melt(data=pintercept,
                                  id.vars=c("PLOTKEY","TRANSECT","MARK","SAGEBRUSH_SHAPE"),
                                  measure.vars=c(colnames(pintercept)[grepl(pattern="^HIT[1-6]$", x=colnames(pintercept))],
                                                 "BASAL",
                                                 "NONSOIL"),
                                  variable.name="layer",
                                  value.name="code",
                                  na.rm=TRUE)

  #Remove blank fiels with no data
  lpi.hits.tall<-lpi.hits.tall %>%subset(code!="")

  #Rename "BASAL" and "NONSOIL" to "SoilSurface"
  lpi.hits.tall$layer<-stringr::str_replace_all(string=lpi.hits.tall$layer,
                                                pattern="BASAL|NONSOIL",
                                                replacement= "SoilSurface")
  #Rename "Hit1" as "TopCanopy"
  lpi.hits.tall$layer<-stringr::str_replace_all(string=lpi.hits.tall$layer,
                                                pattern="HIT1",
                                                replacement= "TopCanopy")


  #Change "PlotKey" field name to "PrimaryKey"

  lpi.hits.tall<-dplyr::rename(lpi.hits.tall, PrimaryKey=PLOTKEY, LineKey=TRANSECT, PointNbr=MARK)


  return(lpi.hits.tall)
}

## Gather Height Data
gather.height <- function(dsn,
                          species.characteristics = TRUE,
                          species.file="",#path to .csv or .gdb holding  the species table
                          species.code="SpeciesCode", #field name in species file that identifies the species code
                          species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                          species.duration="Duration", #field name in species file of the Duration assignment
                          growth.habit.file="", #path to .csv or gdb holding tblSpeciesGrowthHabit
                          growth.habit.code="Code"){
  lpi.detail <- suppressWarnings(sf::st_read(dsn=dsn, layer = "tblLPIDetail"))
  lpi.header <- suppressWarnings(sf::st_read(dsn=dsn, layer = "tblLPIHeader"))

  #we only want to carry a subset of the lpi.header fields forward
  lpi.header<-subset(lpi.header, select=c(PrimaryKey, LineKey:CheckboxLabel ))

  lpi.height.tall.woody <- dplyr::select(.data = lpi.detail,
                                            PrimaryKey,
                                            PointLoc,
                                            PointNbr,
                                            RecKey,
                                            dplyr::matches("Woody$")) %>% dplyr::mutate(type = "woody")
## Strip out the extra name stuff so woody and herbaceous variable names will match.
names(lpi.height.tall.woody) <- stringr::str_replace_all(string = names(lpi.height.tall.woody),
                                                              pattern = "Woody$",
                                                              replacement = "")

lpi.height.tall.herb <- dplyr::select(.data = lpi.detail,
                                           PrimaryKey,
                                           PointLoc,
                                           PointNbr,
                                           RecKey,
                                           dplyr::matches("Herbaceous$")) %>% dplyr::mutate(type = "herbaceous")
names(lpi.height.tall.herb) <- stringr::str_replace_all(string = names(lpi.height.tall.herb),
                                                             pattern = "Herbaceous$",
                                                             replacement = "")

lpi.height <- rbind(lpi.height.tall.woody, lpi.height.tall.herb) %>%
  merge(x=., y=lpi.header, all=TRUE, allow.cartesian = TRUE) %>% subset(., !is.na(Height))


## If we're adding species
if (species.characteristics) {
  #check for the source of the species data
  if (is.null(species.file)){
    species.file<-dsn
  }
  if (is.null(species.file)){
    growth.habit.file<-dsn
  }

  #Print
  print("Gathering species data")
  ##Load species data
  species<-gather.species(species.file=species.file,
                          growth.habit.file = growth.habit.file,
                          growth.habit.code = growth.habit.code,
                          species.growth.habit.code = species.growth.habit.code)

  ##Merge unknown codes
  species.generic<-generic.growth.habits(recorded.species.codes=lpi.height$code,
                                         species.list=species,
                                         species.code = species.code,
                                         species.growth.habit.code=species.growth.habit.code, #field name in species file of the species code to link to GrowthHabit
                                         species.duration=species.duration #field name for duration
  )


  #check for duplicate species
  if (nrow(species.generic[duplicated(species.generic$Symbol),]>0)){
    warning("Duplicate species codes in the species file. The first species occurrence will be used.")
    print(species.generic[duplicated(species.generic$Symbol),])
  }



  #Print
  print("Merging LPI and species tables")

     ## Add species info to the LPI.height table
  lpi.habit.height <- merge(x = lpi.height,
                            y = species.generic,
                            by.x = "Species",
                            by.y = species.code,
                            all.x = TRUE,
                            allow.cartesian=TRUE)

  # Remove orphaned records and duplicates, if they expist
  lpi.habit.height<-unique(lpi.habit.height)
  lpi.habit.height<-lpi.habit.height[!is.na(lpi.habit.height$PrimaryKey),]
  #Output the species level data
  return (lpi.habit.height)
}

# Remove orphaned records and duplicates, if they expist
lpi.height<-unique(lpi.height)
lpi.height<-lpi.height[!is.na(lpi.height$PrimaryKey),]
 #Output the woody/herbaceous level data
  return (lpi.height)
}



