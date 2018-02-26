#' Gather Species Inventory
#'


gather.species.inventory<-function(dsn){

  species.inventory.detail<-suppressWarnings(sf::st_read(dsn, layer =  "tblSpecRichDetail")%>% select(-OBJECTID))

  species.inventory.header<-suppressWarnings(sf::st_read(dsn, layer =  "tblSpecRichHeader")%>% select(-OBJECTID))

  species.inventory<-merge(species.inventory.header, species.inventory.detail, allow.cartesian=TRUE)

  return(species.inventory)
}


species.count<-function(species.inventory.tall, ...){
  grouping.variables<-rlang::quos(...)

  species.count<-species.inventory.tall %>% select(PrimaryKey, SpeciesCount)

  return(species.count)


}
