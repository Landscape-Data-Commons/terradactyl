#' Gather Species Inventory
#'


gather.species.inventory<-function(dsn){

  species.inventory.detail<-suppressWarnings(sf::st_read(dsn, layer =  "tblSpecRichDetail"))

  species.inventory.header<-suppressWarnings(sf::st_read(dsn, layer =  "tblSpecRichHeader"))

  species.inventory<-merge(species.inventory.header, species.inventory.detail, allow.cartesian=TRUE)

  return(species.inventory)
}


species.count<-function(species.inventory.tall, ...){
  grouping.variables<-rlang::quos(...)

  species.count<-species.inventory.tall %>% select(PrimaryKey, SpeciesCount)

  return(species.count)


}

tall.species <- function(spec.rich.detail) {
  tall.list <- lapply(1:nrow(spec.rich.detail), FUN = function(X, df){
    codes <- stringr::str_split(df[X, "SpeciesList"], pattern = ";")[[1]]
    output <- data.frame("PrimaryKey"=df$PrimaryKey[X],"RecKey" = df$RecKey[X], "Codes" = codes)
    return(output)
  }, df = spec.rich.detail)
  output <- dplyr::bind_rows(tall.list)
  output <- dplyr::filter(output, !(Codes %in% c("", NA)))
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

