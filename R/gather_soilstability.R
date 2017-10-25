#' Gather Soil Stability Data
#'
#'
#'

gather.soil.stability<-function( filepath,
                                 gdb){

  #load library
  library(arcgisbinding)
  arcgisbinding::arc.check_product()

  #read in tabular data
  soil.stability.detail <- read.geodatabase(filepath, gdb, feature.name = "tblSoilStabDetail")
  soil.stability.header<-read.geodatabase(filepath, gdb, feature.name = "tblSoilStabHeader")


  #remove orphaned records
  soil.stability.detail<-soil.stability.detail[!is.na(soil.stability.detail$PrimaryKey),]
  #Morph soil stability detail into a tidy format

  gathered<-soil.stability.detail %>%
    #Remove standard columns (In and Dip Times and Date Downloaded in DB)
    dplyr::select(., match= -dplyr::starts_with("In"), -dplyr::starts_with("Dip"), -dplyr::starts_with("DateLoaded"), -OBJECTID)%>%
    #Convert to tall format
    tidyr::gather(., key=variable, value=value, -PrimaryKey, -RecKey, -BoxNum)

    #Remove NAs
    gathered<-gathered[!is.na(gathered$value),]

    #Separate numerical suffixes from field type
  gathered$key<-stringr::str_extract(string=gathered$variable, pattern = "^[A-z]+")

  gathered<-subset(gathered, select=-c(variable,BoxNum))

  #Remove duplicates
  gathered<-unique(gathered)

  #Spread the gathered data so that Line, Rating, Vegetation, and Hydro are all different variables

  soil.stability.detail.tidy<-lapply(X=as.list(unique(gathered$key)),
                                     FUN=function(k=as.list(unique(gathered$key)),df=gathered ){
                                       df[df$key==k,] %>% mutate(id=1:n())%>%
                                         tidyr::spread(key=key, value=value)%>% select(-id)
                                     })%>% purrr::reduce(merge)

  soil.stability.detail.tidy$Rating<-as.numeric(soil.stability.detail.tidy$Rating)

  #Merge soil stability detail and header tables
  soil.stability.tall<-merge(soil.stability.header, soil.stability.detail.tidy, allow.cartesian = TRUE)


  #Return final merged file
  return(soil.stability)
  }


