

gather.gap<-function(dsn){


  gap.detail <- suppressWarnings(sf::st_read(dsn, layer = "tblGapDetail"))
  gap.header<-suppressWarnings(sf::st_read(dsn, layer = "tblGapHeader"))



  #Merge header and detail data together
  gap.tall<-dplyr::left_join(x=gap.header,
                  y=gap.detail)


  ##Remove all orphaned records
  gap.tall<-gap.tall[!is.na(gap.tall$PrimaryKey),]

  ##Add zero values where there is no gap present on line
  gap.tall[gap.tall$NoCanopyGaps==1,]<-gap.tall %>% dplyr::filter (NoCanopyGaps==1)%>%
    tidyr::replace_na(list(RecType="C", GapStart=0, GapEnd=0, Gap=0))

  gap.tall[gap.tall$NoBasalGaps==1,]<-gap.tall %>% dplyr::filter (NoBasalGaps==1)%>%
    tidyr::replace_na(list(RecType="B", GapStart=0, GapEnd=0, Gap=0))

return(gap.tall)
}


gather.gap.lmf<-function(dsn, file.type="gdb"){
  gintercept <- switch(file.type,
                       "gdb" = {suppressWarnings(sf::st_read(dsn = dsn, layer = "GINTERCEPT"))},
                       "txt" = {read.table(paste(dsn,"gintercept.txt", sep=""), stringsAsFactors = FALSE, strip.white=TRUE, header=FALSE, sep="|")})


  if(file.type=="txt"){
    #Add meaningful column names
  colnames<-as.vector(as.data.frame(subset(terradactyl::nri.data.column.explanations, TABLE.NAME=="GINTERCEPT", select = FIELD.NAME)))
  colnames<-colnames$FIELD.NAME
  pintercept<-gintercept[1:length(colnames)]
  names(gintercept)<-colnames
}


  #convert to metric
  gintercept$START_GAP<-gintercept$START_GAP*30.48
  gintercept$END_GAP<-gintercept$END_GAP*30.48
  gintercept$Gap<-gintercept$END_GAP-gintercept$START_GAP


  #We need to establish and/or fix the PLOTKEY so it exists in a single field.
  gintercept$PLOTKEY<-paste(gintercept$SURVEY, gintercept$STATE, gintercept$COUNTY, gintercept$PSU, gintercept$POINT, sep="")


  #check for negative values and remove
  gap<-gintercept %>% subset(Gap>=0)


  #recode gap type so that it fits the DIMA types
  gap$GAP_TYPE<-as.character(gap$GAP_TYPE)
  gap$GAP_TYPE[gap$GAP_TYPE=="peren"]<-"P"
  gap$GAP_TYPE[gap$GAP_TYPE=="canopy"]<-"C"
  gap$GAP_TYPE[gap$GAP_TYPE=="basal"]<-"B"

  #rename fields so they can be merged with a DIMA/TerrADat type
  gap<-dplyr::rename(gap, PrimaryKey=PLOTKEY, LineKey=TRANSECT, RecType=GAP_TYPE,
                     GapStart=START_GAP, GapEnd=END_GAP, SeqNo=SEQNUM)

  gap$Measure<-1 #units are metric
  gap$LineLengthAmount<-150*30.48 #line length of an NRI transect
  gap$GapMin<-12*2.54 #minimum gap size

  #Strip down fields
  gap<-select(gap, -c(SURVEY:POINT))

  return(gap)

}

