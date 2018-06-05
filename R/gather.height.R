#' Convert wide-format TerrADat height data to a tall, tidy format
#' @param dsn Character string. The full path to the .gdb containing the TerrADat tblLPIDetail and tblLPIHeader tables.
#'

## Gather Height Data
gather.height <- function(dsn,
                          species.characteristics = TRUE,
                          species.file="",#path to .csv or .gdb holding  the species table
                          species.code="SpeciesCode", #field name in species file that identifies the species code
                          species.growth.habit.code="GrowthHabitSub", #field name in species file of the species code to link to GrowthHabit
                          species.duration="Duration", #field name in species file of the Duration assignment
                          growth.habit.file="", #path to .csv or gdb holding tblSpeciesGrowthHabit
                          growth.habit.code="Code"){
  # Make sure the geodatabse exists
  if (!file.exists(dsn)){
    stop("dsn must be a valid filepath to a geodatabase containing tblLPIDetail and tblLPIHeader")
  }

  # Read in the LPI tables from the geodatabase
  lpi.detail <- suppressWarnings(sf::st_read(dsn=dsn, layer = "tblLPIDetail"))
  lpi.header <- suppressWarnings(sf::st_read(dsn=dsn, layer = "tblLPIHeader"))

  ## TODO: Make this an else statement
  if(colnames(lpi.header) %in% "DIMAKey"){
    levels <- rlang::quos(PrimaryKey, DIMAKey)
  } else {
    levels <- rlang::quos(PrimaryKey)
  }

  #we only want to carry a subset of the lpi.header fields forward
  lpi.header <- subset(x = lpi.header,
                       select = c(levels,
                                  LineKey:CheckboxLabel))

  lpi.height.tall.woody <- dplyr::select(.data = lpi.detail,
                                         !!!levels,
                                         PointLoc,
                                         PointNbr,
                                         RecKey,
                                         dplyr::matches("Woody$")) %>% dplyr::mutate(type = "woody")
  ## Strip out the extra name stuff so woody and herbaceous variable names will match.
  names(lpi.height.tall.woody) <- stringr::str_replace_all(string = names(lpi.height.tall.woody),
                                                           pattern = "Woody$",
                                                           replacement = "")

  lpi.height.tall.herb <- dplyr::select(.data = lpi.detail,
                                        !!!levels,
                                        PointLoc,
                                        PointNbr,
                                        RecKey,
                                        dplyr::matches("Herbaceous$")) %>% dplyr::mutate(type = "herbaceous")
  names(lpi.height.tall.herb) <- stringr::str_replace_all(string = names(lpi.height.tall.herb),
                                                          pattern = "Herbaceous$",
                                                          replacement = "")

  lpi.height <- rbind(lpi.height.tall.woody, lpi.height.tall.herb) %>%
    dplyr::full_join(x=., y=lpi.header) %>% subset(., !is.na(Height))


  ## If we're adding species
  if (species.characteristics) {

    #Print
    print("Gathering species data")
    lpi.height.species<-species.join(data=lpi.height,
                                     data.code="Species",
                                     species.file=species.file,#path to .csv or .gdb holding  the species table
                                     species.code=species.code, #field name in species file that identifies the species code
                                     species.growth.habit.code=species.growth.habit.code, #field name in species file of the species code to link to GrowthHabit
                                     species.duration=species.duration, #field name in species file of the Duration assignment
                                     growth.habit.file=growth.habit.file, #path to .csv or gdb holding tblSpeciesGrowthHabit
                                     growth.habit.code=growth.habit.code)
    lpi.height.species<-unique(lpi.height.species)
    lpi.height.species<-lpi.height.species[!is.na(lpi.height.species$PrimaryKey),]
    #Output the species level data
    return (lpi.height.species)
  }

  # Remove orphaned records and duplicates, if they expist
  lpi.height<-unique(lpi.height)
  lpi.height<-lpi.height[!is.na(lpi.height$PrimaryKey),]
  #Output the woody/herbaceous level data
  return (lpi.height)
}


#Gather Height for LMF/NRI
gather.height.lmf<-function(dsn, file.type="gdb"){

  #Read in the data as .txt or .gdb
  vegheight <- switch(file.type,
                       "gdb" = {suppressWarnings(sf::st_read(dsn, layer="PASTUREHEIGHTS", stringsAsFactors=FALSE))},
                       "txt" = {read.table(paste(dsn,"pastureheights.txt", sep=""), stringsAsFactors = FALSE, header=FALSE, sep="|", strip.white = TRUE)})

  #if it is in a text file, there are no field names assigned.
  colnames<-as.vector(as.data.frame(subset(nri.data.column.explanations, TABLE.NAME=="PASTUREHEIGHTS", select = FIELD.NAME)))
  colnames<-colnames$FIELD.NAME
  vegheight<-vegheight[1:length(colnames)]
  names(vegheight)<-colnames

  #We need to establish and/or fix the PLOTKEY so it exists in a single field.
  vegheight$PLOTKEY<-paste(vegheight$SURVEY, vegheight$STATE, vegheight$COUNTY, vegheight$PSU, vegheight$POINT, sep="")


  height.woody <- dplyr::select(.data = vegheight,
                                         PLOTKEY,
                                         TRANSECT,
                                         DISTANCE,
                                         dplyr::matches("^W")) %>% dplyr::mutate(type = "woody")
    #remove the "W" from the names
  names(height.woody)<- stringr::str_replace_all(string = names(height.woody),
                             pattern = "W",
                             replacement = "")

  height.herbaceous <- dplyr::select(.data = vegheight,
                                PLOTKEY,
                                TRANSECT,
                                DISTANCE,
                                dplyr::matches("^H")) %>% dplyr::mutate(type = "herbaceous")

  #remove the "H" from the "HPLANT" field
  names(height.herbaceous)[names(height.herbaceous)=="HPLANT"]<- "PLANT"
  height<-rbind(height.woody, height.herbaceous)

  #remove NA values
  height<-subset(height, !is.na(HEIGHT))


  #The height units are concatenated in the field, separate so that we can convert to metric appopriately
  height<-tidyr::separate(height, "HEIGHT", c("HEIGHT", "UOM"), sep=" ", extra="drop", fill="right")

  #Convert to metric
  height$HEIGHT<-suppressWarnings(as.numeric(height$HEIGHT))
  height$UOM<-"cm"

  #convert to centimeters
  height$HEIGHT<-height$HEIGHT*2.54
  height$UOM[is.na(height$UOM)|height$UOM=="in"]<-"cm"
  height$HEIGHT[height$UOM=="ft"]<-height$HEIGHT[height$UOM=="ft"]*12
  height$UOM<-"cm"


  #rename field names
  height<-dplyr::rename(height, PrimaryKey=PLOTKEY, LineKey=TRANSECT, PointNbr=DISTANCE, Height=HEIGHT,
                        Species=PLANT)

  #return height
  return(height)

}


