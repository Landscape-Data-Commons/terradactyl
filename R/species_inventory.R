#' Gather Species Inventory
#'


gather.species.inventory<-function(filepath,
                                   gdb){

  species.inventory.detail<-read.geodatabase(filepath, gdb, feature.name = "tblSpecRichDetail")%>% select(-OBJECTID)

  species.inventory.header<-read.geodatabase(filepath, gdb, feature.name = "tblSpecRichHeader")%>% select(-OBJECTID)

  species.inventory<-merge(species.inventory.header, species.inventory.detail, allow.cartesian=TRUE)

  return(species.inventory)
}


species.count<-function(species.inventory.tall, ...){
  grouping.variables<-rlang::quos(...)

  species.count<-species.inventory.tall %>% select(PrimaryKey, SpeciesCount)

  return(species.count)


}
