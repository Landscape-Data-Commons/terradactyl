###Rangeland Health###



gather.rangeland.health<-function(dsn){

  IIRH.header<-sf::st_read(dsn, layer="tblQualHeader")

  IIRH<-select(IIRH.header, DIMAKey, PrimaryKey, DateLoadedInDb, DateVisited=FormDate,
               HydroFunction=HFVxWRatingFinal,
               BioticIntegrity=BIVxWRatingFinal,
               SoilSiteStability=SSSVxWRatingFinal)

}
