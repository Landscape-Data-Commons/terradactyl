#### DATA FRAMES ###############################################################
# This makes a list of testing data frames, organized by source that they mimic,
# the method that they reflect, and the potential edge cases in them.

# Note that the metadata/header info for specific methods are placed in the list
# that contains that method's test data and not in the list dedicated to
# metadata. The metadata lists are intended only for the metadata that apply to
# the sampling event and all methods at the sampling location.




# Initialize the list so we can write into them piecemeal.
testing_data <- list(aim = list(),
                     lmf = list(),
                     nri = list())

##### Species ------------------------------------------------------------------
# These are used in multiple situations, so they get their own section.
species_list <- list(tblNationalPlants,
                     tblStateSpecies)


##### AIM ######################################################################

###### Metadata ----------------------------------------------------------------
testing_data$aim[["metadata"]] <- list(tblLines,
                                       tblPlots)

###### LPI and heights ---------------------------------------------------------
testing_data$aim[["lpi"]] <- list(tblLPIHeader,
                                  tblLPIDetail)

###### Gap ---------------------------------------------------------------------
testing_data$aim[["gap"]] <- list(tblGapHeader,
                                  tblGapDetail)

###### Species Inventory -------------------------------------------------------
testing_data$aim[["species_inventory"]] <- list(tblSpecRichHeader,
                                                tblSpecRichDetail)

###### Soil stability ----------------------------------------------------------
testing_data$aim[["soil_stability"]] <- list(tblSoilStabHeader,
                                             tblSoilStabDetail)

###### Rangeland Health --------------------------------------------------------
testing_data$aim[["rangeland_health"]] <- list(tblIIRH)




##### LMF ######################################################################

###### Metadata ----------------------------------------------------------------
testing_data$lmf[["metadata"]] <- list(POINT,
                                       COUNTYNM,
                                       STATENM,
                                       POINTCOORDINATES,
                                       GPS,
                                       ESFSG)

###### LPI and heights ---------------------------------------------------------
testing_data$lmf[["lpi"]] <- list(PINTERCEPT,
                                  PASTUREHEIGHTS)

###### Gap ---------------------------------------------------------------------
testing_data$lmf[["gap"]] <- list(GINTERCEPT)

###### Species Inventory -------------------------------------------------------
testing_data$lmf[["species_inventory"]] <- list(PLANTCENSUS)

###### Soil stability ----------------------------------------------------------
testing_data$lmf[["soil_stability"]] <- list(SOILDISAG)

###### Rangeland Health --------------------------------------------------------
testing_data$lmf[["rangeland_health"]] <- list(RANGEHEALTH)





##### NRI ######################################################################
###### Headers -----------------------------------------------------------------
###### LPI and heights ---------------------------------------------------------
###### Gap ---------------------------------------------------------------------
###### Species Inventory -------------------------------------------------------
###### Soil stability ----------------------------------------------------------
###### Rangeland Health --------------------------------------------------------

