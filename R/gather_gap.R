

gather.gap<-function(dsn){


  gap.detail <- suppressWarnings(sf::st_read(dsn, layer = "tblGapDetail"))
  gap.header<-suppressWarnings(sf::st_read(dsn, layer = "tblGapHeader"))



  #Merge header and detail data together
  gap.tall<-merge(x=gap.header,
                  y=gap.detail,
                  by=c("PrimaryKey", "RecKey"),
                  all=TRUE,
                  allow.cartesian = TRUE)


  ##Remove all orphaned records
  gap.tall<-gap.tall[!is.na(gap.tall$PrimaryKey),]

}
