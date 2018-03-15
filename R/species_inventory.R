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


