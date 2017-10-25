#' Soil Stability Indicator Calculations


soil.stability<-function(soil.stability.tall,
                         all=TRUE,
                         cover=TRUE,
                         uncovered=TRUE,
                         all.cover.type=TRUE,
                         tall=FALSE){


  soil.stability.rating<-list()

#Calculate a mean rating for all cover types
  if (all==TRUE){
    soil.stability.rating[["all"]]<-soil.stability.tall %>% dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(rating=mean(Rating)) %>% dplyr::mutate(Veg="all")%>% as.data.frame()
  }
#Calculate mean rating for all covered soil samples
  if (cover==TRUE){
    soil.stability.rating[["covered"]]<-soil.stability.tall %>% subset(Veg!="NC")%>% dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(rating=mean(Rating))%>% dplyr::mutate(Veg="covered") %>% as.data.frame()
  }
#Calculate mean rating for all uncovered soil samples
  if (uncovered==TRUE){
    soil.stability.rating[["unconvered"]]<-soil.stability.tall %>% subset(Veg=="NC")%>% dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(rating=mean(Rating))%>% dplyr::mutate(Veg="uncovered")%>% as.data.frame()
  }
#Calculate mean rating for all cover types individually
  if (all.cover.type==TRUE){
    soil.stability.rating[["all.cover.types"]]<-soil.stability.tall %>%  dplyr::group_by(PrimaryKey, Veg) %>%
      dplyr::summarize(rating=mean(Rating))%>% as.data.frame()
  }

  #merge all soil stability rating calculations
  soil.stability.rating.all<-do.call("rbind", soil.stability.rating)

#if tall=FALSE spread into a wide format
  if (!tall){
    soil.stability.rating.all<-soil.stability.rating.all %>% spread(key=Veg, value=rating)
  }


return(soil.stability.rating.all)

}
