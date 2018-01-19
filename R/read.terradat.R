#' Reads a TerrADat Database
#'
#'@param filename File path to the TerrADat database, include the .gdb extension
#'@param type The type of TerrADat object to be read in \code {"table} or \code {"feature"} for feature classes.
#'@param features A vector list of the tables or features to read in.
#'@param ... List of tables or feature classes to read into R
#'
#'
#'

library(arcgisbinding)
arc.check_product()
library(tidyverse)

read.geodatabase<-function(filepath, gdb=NULL, feature.name) {
  return(test<-data.table::as.data.table(arcgisbinding::arc.select(arcgisbinding::arc.open(paste(filepath, gdb, feature.name, sep = "/")))))
}


read.terradat<-function(filepath, gdb,tablelist,all.tables=FALSE ){
  if (all.tables){#All of the tables in TerrADat
    tablelist <- c("tblApplicationConstants",
                  "tblEcolSites",
                  "tblESDDominantPerennialHeight",
                  "tblGapDetail",
                  "tblGapHeader",
                  "tblLines",
                  "tblLPIDetail",
                  "tblLPIHeader",
                  "tblPeople",
                  "tblPhotos",
                  "tblPlantDenDetail",
                  "tblPlantDenHeader",
                  "tblPlantDenQuads",
                  "tblPlantDenSpecies",
                  "tblPlotCustomLookup1",
                  "tblPlotCustomLookup2",
                  "tblPlotCustomLookup3",
                  "tblPlotFormDefaults",
                  "tblPlotHistory",
                  "tblPlotNotes",
                  "tblQualDetail",
                  "tblQualHeader",
                  "tblSageEval",
                  "tblSageLek",
                  "tblSageRange",
                  "tblSites",
                  "tblSoilPitHorizons",
                  "tblSoilPits",
                  "tblSoilStabDetail",
                  "tblSoilStabHeader",
                  "tblSoilSurface",
                  "tblSpecies",
                  "tblSpeciesGrowthHabit",
                  "tblSpecRichAbundance",
                  "tblSpecRichDetail",
                  "tblSpecRichHeader")
    }
    #read in the desired tables
    terradat<-lapply(X = tablelist,
                     FUN =  function(filepath, gdb, X) {
                       return(test<-data.table::as.data.table(arcgisbinding::arc.select(arcgisbinding::arc.open(paste(filepath, gdb, X, sep = "/")))))
                     },
                     filepath = filepath,
                     gdb = gdb) %>% setNames(tablelist)
  }
