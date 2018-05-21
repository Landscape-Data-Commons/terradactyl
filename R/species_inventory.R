#' Gather Species Inventory
#'


gather.species.inventory<-function(dsn){
  #load raw tables
  species.inventory.detail<-suppressWarnings(sf::st_read(dsn, layer =  "tblSpecRichDetail"))
  species.inventory.header<-suppressWarnings(sf::st_read(dsn, layer =  "tblSpecRichHeader"))

  #Make Species Inventory Detail  a tall dataframe
  species.detail.tall<-tall.species(species.inventory.detail = species.inventory.detail)

  #Join with header data and strip out NA codes
  species.inventory.tall<-dplyr::left_join(species.inventory.header, species.detail.tall) %>% subset(!is.na(Species))

  return(species.inventory.tall)
}


species.count<-function(species.inventory.tall, ...){
  grouping.variables<-rlang::quos(...)

  if("DIMAKey"%in% colnames(species.inventory.tall)){
    levels<-rlang::quos(DIMAKey, PrimaryKey)
  }else
    levels<-rlang::quos(PrimaryKey)

  species.count<-species.inventory.tall %>% dplyr::count(!!!levels, !!!grouping.variables)%>%
    tidyr::unite(indicator, !!!grouping.variables, sep = ".") %>% dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))



  return(species.count)


}

tall.species <- function(species.inventory.detail) {
  tall.list <- lapply(1:nrow(species.inventory.detail), FUN = function(X, df){
    codes <- stringr::str_split(df[X, "SpeciesList"], pattern = ";")[[1]]
    output <- data.frame("PrimaryKey"=df$PrimaryKey[X],"RecKey" = df$RecKey[X], "Species" = codes)
    return(output)
  }, df = species.inventory.detail)
  output <- dplyr::bind_rows(tall.list)
  output <- dplyr::filter(output, !(Species %in% c("", NA)))
  return(output)
}


#Gather LMF data
gather.species.lmf<-function(dsn, file.type="gdb"){
  plantcensus <- switch(file.type,
                      "gdb" = {suppressWarnings(sf::st_read(dsn, layer="PLANTCENSUS", stringsAsFactors=FALSE))},
                      "txt" = {read.table(paste(dsn,"plantcensus.txt", sep=""), stringsAsFactors = FALSE, header=FALSE, sep="|", strip.white = TRUE)})


  #if it is in a text file, there are no field names assigned.
  colnames<-as.vector(as.data.frame(subset(nri.data.column.explanations, TABLE.NAME=="PLANTCENSUS", select = FIELD.NAME)))
  colnames<-colnames$FIELD.NAME
  plantcensus<-plantcensus[1:length(colnames)]
  names(plantcensus)<-colnames

  #We need to establish and/or fix the PLOTKEY so it exists in a single field.
  plantcensus$PLOTKEY<-paste(plantcensus$SURVEY, plantcensus$STATE, plantcensus$COUNTY, plantcensus$PSU, plantcensus$POINT, sep="")

  #Get species count
 species.inventory<-plantcensus %>% group_by(PLOTKEY) %>% summarize(., SpeciesCount=n()) %>% merge(., plantcensus)

  #rename fields
  species.inventory<-dplyr::rename(species.inventory, PrimaryKey=PLOTKEY,
                        Codes=CPLANT) %>%select(., -c(SURVEY:SEQNUM))

  return(species.inventory)
}

