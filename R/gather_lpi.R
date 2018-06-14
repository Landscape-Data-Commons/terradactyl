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
  lpi.hits.tall<-lpi.detail %>% dplyr::select(PrimaryKey, PointLoc, PointNbr, RecKey, ShrubShape,
                                              TopCanopy, SoilSurface,colnames(lpi.detail)[grepl(pattern="^Lower[1-7]$", x=colnames(lpi.detail))]) %>%
    tidyr::gather(key=layer, value=code, -PrimaryKey, -PointLoc, -PointNbr, -RecKey, -ShrubShape, na.rm=TRUE)

   #Remove all records where no hit was recorded (e.g., "None", "NA"
  lpi.hits.tall <- dplyr::filter(.data = lpi.hits.tall,
                            !is.na(code),
                            code != "",
                            code != "None",
                            !is.na(PrimaryKey),
                            !is.na(RecKey))


  ## Make a tall data framethe checkbox status by layer
  lpi.chkbox.tall <-lpi.detail %>% dplyr::select(PrimaryKey, PointLoc, PointNbr, RecKey, ShrubShape,
                                                 colnames(lpi.detail)[grepl(pattern="^Chkbox", x=colnames(lpi.detail))]) %>%
    tidyr::gather(key=layer, value=chckbox,-PrimaryKey, -PointLoc, -PointNbr, -RecKey, -ShrubShape, na.rm=TRUE)

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
    dplyr::left_join(select(lpi.header, LineKey:CheckboxLabel, PrimaryKey, DIMAKey), ., by=c("PrimaryKey", "RecKey")))

  #Rename ShrubShape to SAGEBRUSH_SHAPE
  lpi.tall<-dplyr::rename(lpi.tall, "SAGEBRUSH_SHAPE"=ShrubShape)

  ## If we're adding species

  if(species.characteristics){
    lpi.tall.species<-species.join(data=lpi.tall,
                                   data.code="code",
                                   species.file=species.file,#path to .csv or .gdb holding  the species table
                                   species.code=species.code, #field name in species file that identifies the species code
                                   species.growth.habit.code=species.growth.habit.code, #field name in species file of the species code to link to GrowthHabit
                                   species.duration=species.duration, #field name in species file of the Duration assignment
                                   growth.habit.file=growth.habit.file, #path to .csv or gdb holding tblSpeciesGrowthHabit
                                   growth.habit.code=growth.habit.code
                                   )
    return(lpi.tall.species)
  }
    return(lpi.tall)
  ## Output the list

}

gather.lpi.lmf<-function(dsn,
                         file.type="gdb",
                         species.characteristics = TRUE,
                         species.file="",#path to .csv or .gdb holding  the species table
                         species.code="SpeciesCode", #field name in species file that identifies the species code
                         species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                         species.duration="Duration", #field name in species file of the Duration assignment
                         growth.habit.file="", #path to .csv or gdb holding tblSpeciesGrowthHabit
                         growth.habit.code="Code" ){

  #Read  PINTERCEPT table in .txt or .gdb

  pintercept <- switch(file.type,
                    "gdb" = {suppressWarnings(sf::st_read(dsn = dsn, layer = "PINTERCEPT"))},
                    "txt" = {read.table(paste(dsn,"pintercept.txt", sep=""), stringsAsFactors = FALSE, strip.white=TRUE, header=FALSE, sep="|")})

   #if it is in a text file, there are no field names assigned.
  if (file.type=="txt"){
    colnames<-as.vector(as.data.frame(subset(terradactyl::nri.data.column.explanations, TABLE.NAME=="PINTERCEPT", select = FIELD.NAME)))
    colnames<-colnames$FIELD.NAME
    colnames<-colnames[1:ncol(pintercept)]%>% na.omit()
    names(pintercept)<-colnames
  }

  #remove any NA field names that may have been introduced
  pintercept<-pintercept[,!is.na(colnames(pintercept))]

  #We need to establish and/or fix the PLOTKEY so it exists in a single field.
  pintercept$PLOTKEY<-paste(pintercept$SURVEY, pintercept$STATE, pintercept$COUNTY, pintercept$PSU, pintercept$POINT, sep="")


  #For line point intercept data (cover calculations--point number 75 is recorded twice—once on each transect.
  #We only want to use it once in the calculations.
  #Prior to doing these calculations, it would be beneficial to remove one of the point 75’s from the data set.
  #Remove the nesw transect—that would be all rows in pintercept where column 6 = “nesw” AND column 7 = 75.
  pintercept<-pintercept%>%subset(!(MARK==75&TRANSECT=="nesw"))


  #Where there is a Soil hit, LMF records "None" in BASAL and leaves NONSOIL blank. Let's fill in an "S" to indicate soil
  levels(pintercept$NONSOIL)<-c(levels(pintercept$NONSOIL), "S")
  pintercept$NONSOIL[pintercept$BASAL=="None" & pintercept$NONSOIL==""]<-"S"


   #Create a tall table
  lpi.hits.tall<-pintercept %>% select(pintercept,-c(SURVEY:POINT))%>%
    tidyr::gather(key=layer, value=code, BASAL, NONSOIL, colnames(pintercept)[grepl(pattern="^HIT[1-6]$", x=colnames(pintercept))] )

    #Remove blank fields with no data
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

  #Convert to factor
  lpi.hits.tall<-lpi.hits.tall %>% mutate_if(is.character, funs(factor))


  if(species.characteristics){
    lpi.tall.species<-species.join(data=lpi.hits.tall,
                                   data.code="code",
                                   species.file=species.file,#path to .csv or .gdb holding  the species table
                                   species.code=species.code, #field name in species file that identifies the species code
                                   species.growth.habit.code=species.growth.habit.code, #field name in species file of the species code to link to GrowthHabit
                                   species.duration=species.duration, #field name in species file of the Duration assignment
                                   growth.habit.file=growth.habit.file, #path to .csv or gdb holding tblSpeciesGrowthHabit
                                   growth.habit.code=growth.habit.code
    )
    return(lpi.tall.species)
  }


  return(lpi.hits.tall)
}

