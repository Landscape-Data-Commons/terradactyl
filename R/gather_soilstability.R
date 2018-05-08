#' Gather Soil Stability Data
#' @param dsn
#' @param source
#'
#'

gather.soil.stability<-function(dsn){


  #read in tabular data
  soil.stability.detail <- suppressWarnings(sf::st_read(dsn, layer =  "tblSoilStabDetail"))
  soil.stability.header<-suppressWarnings(sf::st_read(dsn, layer =  "tblSoilStabHeader"))


  #remove orphaned records
  soil.stability.detail<-soil.stability.detail[!is.na(soil.stability.detail$PrimaryKey),]
  #Morph soil stability detail into a tidy format

  #If DIMA Key exists, remove it
  if("DIMAKey" %in% colnames(soil.stability.detail)){
    soil.stability.detail<-dplyr::select(soil.stability.detail, -DIMAKey)
  }

  gathered<-soil.stability.detail %>%
    #Remove standard columns (In and Dip Times and Date Downloaded in DB)
    dplyr::select(., match= -dplyr::starts_with("In"), -dplyr::starts_with("Dip"), -dplyr::starts_with("DateLoaded"))%>%
    #Convert to tall format
    tidyr::gather(., key=variable, value=value, -PrimaryKey, -BoxNum, -RecKey,na.rm=TRUE)

    #Remove blank values
    gathered<-subset(gathered, value!="")

    #Separate numerical suffixes from field type
  gathered$key<-stringr::str_extract(string=gathered$variable, pattern = "^[A-z]+")
  gathered$Position<-stringr::str_extract(string=gathered$variable, pattern = "[0-9]+")

  gathered<-subset(gathered, select=-c(variable,BoxNum))

  # #Remove duplicates
  # gathered<-unique(gathered)

  #Spread the gathered data so that Line, Rating, Vegetation, and Hydro are all different variables

  soil.stability.detail.list<-lapply(X=as.list(unique(gathered$key)),
                                     FUN=function(k=as.list(unique(gathered$key)),df=gathered ){
                                     test<-  df[df$key==k,] %>% mutate(id=1:n())%>%
                                         tidyr::spread(key=key, value=value)%>% select(-id)
                                     })
  #create a single tidy dataframe
  soil.stability.detail.tidy<-purrr::reduce(soil.stability.detail.list, dplyr::full_join)%>%unique()

  soil.stability.detail.tidy$Rating<-as.numeric(soil.stability.detail.tidy$Rating)

  #Merge soil stability detail and header tables
  soil.stability.tall<-merge(soil.stability.header, soil.stability.detail.tidy, allow.cartesian = TRUE)


  #Return final merged file
  return(soil.stability.tall)
  }

gather.soil.stability.lmf<-function(dsn, file.type="gdb"){
  soildisag<-switch(file.type,
                    "gdb"={suppressWarnings(sf::st_read(dsn = dsn, layer = "SOILDISAG"))},
                    "txt" = {read.table(paste(dsn,"soildisag.txt", sep=""), stringsAsFactors = FALSE, strip.white=TRUE, header=FALSE, sep="|")}

  )
  #Add column names
  if(file.type=="txt"){
    colnames<-as.vector(as.data.frame(subset(terradactyl::nri.data.column.explanations, TABLE.NAME=="SOILDISAG", select = FIELD.NAME)))
    colnames<-colnames$FIELD.NAME
    pintercept<-soildisag[1:length(colnames)]
    names(soildisag)<-colnames
  }
  #We need to establish and/or fix the PLOTKEY so it exists in a single field.
  soildisag$PLOTKEY<-paste(soildisag$SURVEY, soildisag$STATE, soildisag$COUNTY, soildisag$PSU, soildisag$POINT, sep="")

  #conver white space to NA
  soildisag[soildisag==""]<-NA

  #Convert to tall format
  soil.tall<-dplyr::select(soildisag, -c(SURVEY:POINT)) %>% tidyr::gather(., key=variable, value=value, -PLOTKEY)

  #Remove NAs
  gathered<-soil.tall[!is.na(soil.tall$value),]

  #Separate numerical suffixes from field type
  gathered$variable<-stringr::str_extract(string=gathered$variable, pattern = "^[A-z]+")

  #Spread the gathered data so that Line, Rating, Vegetation, and Hydro are all different variables

  soil.stability.tidy<-lapply(X=as.list(unique(gathered$variable)),
                                     FUN=function(k=as.list(unique(gathered$variable)),df=gathered ){
                                       df[df$variable==k,] %>% mutate(id=1:n())%>%
                                         tidyr::spread(key=variable, value=value)%>% select(-id)
                                     })%>% purrr::reduce(merge)

  soil.stability.tidy<-dplyr::rename(soil.stability.tidy, Veg=VEG, Rating=STABILITY, PrimaryKey=PLOTKEY)
  soil.stability.tidy$Rating<-as.numeric(soil.stability.tidy$Rating)

  #Return final merged file
  return(soil.stability.tidy)


}
