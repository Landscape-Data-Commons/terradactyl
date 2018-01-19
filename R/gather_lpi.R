#' Gather LPI data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dima.tables A list of data frames. Recommended to use the output from\code{read.dima()}. Must contain data frames called tblLPIHeader and tblLPIDetail which have the same fields as the tables in DIMA with the same names. If \code{meta} is \code{TRUE} then it must also include data frames called tblSites, tblPlots, and tblLines which have the same fields as the tables in DIMA with the same names. If \code{species.characteristics} is \code{TRUE} then it must also include data frames called tblSpecies and tblSpeciesGrowthHabit (and optionally tblSpeciesGroups) which have the same fields as the tables in DIMA with the same names.
#' @param meta Logical. If \code{TRUE} then the site, plot, and line names and keys will be added to the output data frames from \code{dima.list} using the data frames called tblSites, tblPlots, and tblLines. Defaults to \code{TRUE}
#' @param species.characteristics Logical. If \code{TRUE} then the available species information will be added from \code{dima.list} using the data frames tblSpecies, tblSpeciesGrowthHabit, and, if available, tblSpeciesGroups. Defaults to \code{TRUE}.
#'
#' @return A list of two data frames: one containing the data from the LPI pin intercepts and one containing the data from the height methd done alongside pin drops.
#' @export


#Function to gather species information
gather.species<-function(tblSpecies, #path to .csv or .gdb holding  tblSpecies
                         tblSpeciesGrowthHabit,
                         tblSpec#path to .csv or .gdb holding  tblSpeciesGrowthHabit
                         ){
  ##Load species data
  ##Add some checks for features###
  #Read in .csv or .gdb based Species table
  if (length(grep(".csv$", tblSpecies))>0){
    species<-read.csv(tblSpecies, stringsAsFactors = FALSE)
  }else {
    species<-read.geodatabase(filepath = tblSpecies, feature.name = "tblSpecies")
  }

  #Read in .csv or .gdb based Species Growth Habit table
  if (length(grep(".csv$", tblSpeciesGrowthHabit))>0){
    growth.habit<-read.csv(tblSpeciesGrowthHabit, stringsAsFactors = FALSE)
  }else{
    growth.habit<-read.geodatabase(filepath = tblSpeciesGrowthHabit, feature.name = "tblSpeciesGrowthHabit")
  }

  ## Make a species characteristics data frame
  species <- merge(x = species,
                   y = growth.habit,
                   by.x = "GrowthHabitCode",
                   by.y = "Code",
                   all.x = TRUE,
                   allow.cartesian = TRUE)


  return(species)
}

##Attribute generic species growth habits, for now this assumes field names.
generic.growth.habits<-function(species.codes, species.list
                               ){

  generic.codes<-species.codes[grep("^[A-z]{2}[0-5000]", species.codes)] %>%toupper %>%unique()
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
  warnings()
  generic.growth.habit<-generic.growth.habit[!is.na(generic.growth.habit$GrowthHabitCode),] %>%unique()


  #Merge with main species list
  species.generic<-dplyr::bind_rows(species.list, generic.growth.habit)

  return(species.generic)
}

##Function to make tall format of LPI data
gather.lpi <- function(filepath,
                       gdb,
                       species.characteristics = TRUE,
                       tblSpecies=NA,
                       tblSpeciesGrowthHabit=NA){
  #load library
  library(arcgisbinding)
  arcgisbinding::arc.check_product()

  lpi.detail <- read.geodatabase(filepath, gdb, feature.name = "tblLPIDetail")
  lpi.header<-read.geodatabase(filepath, gdb, feature.name = "tblLPIHeader")

  ## Make a tall data frame with the hit codes by layer and the checkbox designation
  lpi.hits.tall<-data.table::melt(data=lpi.detail,
                                  id.vars=c("PrimaryKey", "PointLoc","PointNbr", "RecKey"),
                                  measure.vars=c("TopCanopy", "SoilSurface",
                                                 colnames(lpi.detail)[grepl(pattern="^Lower[1-7]$", x=colnames(lpi.detail))]),
                                  variable.name="layer",
                                  value.name="code",
                                  na.rm=TRUE)


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

  #Merge checkbox and hit data as well as the header data
  lpi.tall <- merge(x=lpi.header,
                    y=merge(x = lpi.hits.tall,y = lpi.chkbox.tall, all.x=TRUE,
                            by=c("PrimaryKey", "PointLoc","PointNbr","RecKey", "layer"),allow.cartesian = TRUE),
                    by=c("PrimaryKey", "RecKey"), all=TRUE, allow.cartesian = TRUE)


  #Remove all orphaned records
  lpi.tall<-lpi.tall[!is.na(lpi.tall$PrimaryKey),]

  ## If we're adding species

  if (species.characteristics) {
    #check for the source of the species data
    if (is.na(tblSpecies)){
      tblSpecies<-paste(filepath, gdb, sep="")
    }
    if (is.na(tblSpeciesGrowthHabit)){
      tblSpeciesGrowthHabit<-paste(filepath, gdb, sep="")
    }

    ##Load species data
    species<-gather.species(tblSpecies=tblSpecies, tblSpeciesGrowthHabit = tblSpeciesGrowthHabit)

    ##Merge unknown codes
    species<-generic.growth.habits(species.codes=lpi.tall$code, species.list=species)


    ## Add species information to LPI table
    lpi.tall.species <- merge(x = lpi.tall,
                      y = species,
                      by.x = "code",
                      by.y = "SpeciesCode",
                      all.x = TRUE,
                      allow.cartesian = TRUE)
  }

  ## Output the list
  return(lpi.tall.species)
}


## Gather Height Data
gather.height <- function(filepath,
                       gdb,
                       species.characteristics = TRUE,
                       tblSpecies=NA,
                       tblSpeciesGrowthHabit=NA){
  lpi.detail <- read.geodatabase(filepath, gdb, feature.name = "tblLPIDetail")
  lpi.header <- read.geodatabase(filepath, gdb, feature.name = "tblLPIHeader")

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
    if (is.na(tblSpecies)){
      tblSpecies<-paste(filepath, gdb, sep="")
    }
    if (is.na(tblSpeciesGrowthHabit)){
      tblSpeciesGrowthHabit<-paste(filepath, gdb, sep="")
    }

    ##Load species data
    species<-gather.species(tblSpecies=tblSpecies, tblSpeciesGrowthHabit = tblSpeciesGrowthHabit)

    ##Merge unknown codes
    species<-generic.growth.habits(species.codes=lpi.height$code, species.list=species)

     ## Add species info to the LPI.height table
  lpi.habit.height <- merge(x = lpi.height,
                            y = species,
                            by="Species",
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
lpi.height<-lpi.height[!is.na(lpi.habit.height$PrimaryKey),]
 #Output the woody/herbaceous level data
  return (lpi.height)
}

