

gather.gap<-function(filepath,
                     gdb){
  #load library
  library(arcgisbinding)
  arcgisbinding::arc.check_product()

  gap.detail <- read.geodatabase(filepath, gdb, feature.name = "tblGapDetail")
  gap.header<-read.geodatabase(filepath, gdb, feature.name = "tblGapHeader")



  #Merge header and detail data together
  gap.tall<-merge(x=gap.header,
                  y=gap.detail,
                  by=c("PrimaryKey", "RecKey"),
                  all=TRUE,
                  allow.cartesian = TRUE)


  ##Remove all orphaned records
  gap.tall<-gap.tall[!is.na(gap.tall$PrimaryKey),]

}
