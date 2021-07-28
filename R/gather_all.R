#' Gather tall tables for gap, vegetation height, LPI, plot characterization, 
#' IIRH, soil horizon, soil pit summary, soil stability, and species inventory.
#'
#' @description Given wide format AIM/LMF data, gather gap, 
#' vegetation height, LPI, plot characterization, IIRH, soil horizon, 
#' soil pit summary, soil stability, and species inventory data. Missing
#' tables will be skipped. AIM-type and LMF-type data will both be processed.
#' @param dsn Character string. The full filepath and filename (including file 
#' extension) of the geodatabase or text file containing thes table of interest. 
#' This field is unnecessary if you provide dflist.
#' @param dflist Named list of data frames containing monitoring data. Tables 
#' must be named as expected by the individual gather_functions.
#' @param outfolder Character string. Name of a folder to save all output to.
#' If the specified folder does not exist, the function will create it.
#' @importFrom magrittr %>%
#' @name gather_all
#' @family <gather>
#' @return A list of tall data frames containing reformatted input data.
#' @examples
#' gather_all(dsn = "Path/To/AIM-LMF_Geodatabase.gdb", outfolder = "output")
#' 
#' names <- sf::st_layers(dsn = "Path/To/AIM-LMF_Geodatabase.gdb")$name
#' all_data <- sapply(names, function(n){
#'   sf::st_read(dsn = "Path/To/AIM-LMF_Geodatabase.gdb", 
#'   layer = n, quiet = T)
#' })
#' gather_all(dflist = all_data, outfolder = "output")

gather_all <- function(dsn = NULL, dflist = NULL, outfolder) {
  # prep ####
  # most people wouldnt put the trailing f-slash on a folder name so add it in
  if(substr(outfolder, nchar(outfolder), nchar(outfolder)) != "/") {
    outfolder <- paste0(outfolder, "/")
  }
  
  if(!dir.exists(outfolder)) dir.create(outfolder)
  
  # if both dsn and dflist are provided, drop dsn
  if(!is.null(dflist) & !is.null(dsn)){
    dsn <- NULL
    print("Both dsn and dflist were provided. Dsn will be ignored")
  }
  
  # extract names, check against these before trying to load data
  if(is.null(dflist)){
    names_rda <- NULL
  } else {
    names_rda <- names(dflist)
  }
  if(is.null(dsn)){
    names_gdb <-NULL
  } else {
    names_gdb <- sf::st_layers(dsn) %>% unlist()
  }
  
  # pull tables out of dflist if supplied, so the lack of NULL inputs dont mess up the functions
  # if dflist is NULL, all of these should be NULL
  tblGapDetail <- dflist[["tblGapDetail"]]
  tblGapHeader <- dflist[["tblGapHeader"]]
  tblLPIDetail <- dflist[["tblLPIDetail"]]
  tblLPIHeader <- dflist[["tblLPIHeader"]]
  tblSoilStabDetail <- dflist[["tblSoilStabDetail"]]
  tblSoilStabHeader <- dflist[["tblSoilStabHeader"]]
  tblQualDetail <- dflist[["tblQualDetail"]]
  tblQualHeader <- dflist[["tblQualHeader"]]
  tblSoilPitHorizons <- dflist[["tblSoilPitHorizons"]]
  tblSoilPits <- dflist[["tblSoilPits"]]
  tblSpecRichDetail <- dflist[["tblSpecRichDetail"]]
  tblSpecRichHeader <- dflist[["tblSpecRichHeader"]]
  tblPlots <- dflist[["tblPlots"]]
  
  GINTERCEPT <- dflist[["GINTERCEPT"]]
  POINT <- dflist[["POINT"]]
  PASTUREHEIGHTS <- dflist[["PASTUREHEIGHTS"]]
  RANGEHEALTH <- dflist[["RANGEHEALTH"]]
  PINTERCEPT <- dflist[["PINTERCEPT"]]
  SOILDISAG <- dflist[["SOILDISAG"]]
  PLANTCENSUS <- dflist[["PLANTCENSUS"]]
  SOILHORIZON <- dflist[["SOILHORIZON"]]
  POINTCOORDINATES <- dflist[["POINTCOORDINATES"]]
  GPS <- dflist[["GPS"]]
  
  # Gap ####
  print("Preparing Gap data")
  if(("tblGapDetail" %in% names_rda & "tblGapHeader" %in% names_rda) |
     ("tblGapDetail" %in% names_gdb & "tblGapHeader" %in% names_gdb)){
    gap_aim <- gather_gap(dsn = dsn, source = "AIM", 
                          tblGapDetail = tblGapDetail,
                          tblGapHeader = tblGapHeader)
  } else {
    gap_aim <- NULL
    print("tblGapDetail and/or tblGapHeader not found. AIM gap will not be prepared.")
  }
  
  if(("GINTERCEPT" %in% names_rda & "POINT" %in% names_rda) |
     ("GINTERCEPT" %in% names_gdb & "POINT" %in% names_gdb)){
    gap_lmf <- gather_gap(dsn = dsn, source = "LMF", 
                          GINTERCEPT = GINTERCEPT, 
                          POINT = POINT)
  } else {
    gap_lmf <- NULL
    print("GINTERCEPT and/or POINT not found. LMF gap will not be prepared.")
  }
  
  gap_tall <- dplyr::bind_rows(gap_aim, gap_lmf)
  saveRDS(gap_tall,
          file = paste(outfolder, "gap_tall.Rdata", sep = ""))
  rm(gap_aim, gap_lmf)
  invisible(gc())
  
  # Soil stability ####
  print("Preparing Soil Stability data")
  
  if(("tblSoilStabDetail" %in% names_rda & "tblSoilStabHeader" %in% names_rda) |
     ("tblSoilStabDetail" %in% names_gdb & "tblSoilStabHeader" %in% names_gdb)){
    soilstab_aim <- gather_soil_stability(dsn = dsn, source = "AIM",
                                          tblSoilStabDetail = tblSoilStabDetail,
                                          tblSoilStabHeader = tblSoilStabHeader)
  } else {
    soilstab_aim <- NULL
    print("tblSoilStabDetail and/or tblSoilStabHeader not found. AIM Soil Stability will not be prepared.")
  }
  
  if(("SOILDISAG" %in% names_rda) |
     ("SOILDISAG" %in% names_gdb)){
    soilstab_lmf <- gather_soil_stability(dsn = dsn, source = "LMF",
                                          SOILDISAG = SOILDISAG)
  } else {
    soilstab_lmf <- NULL
    print("SOILDISAG not found. LMF Soil Stability will not be prepared.")
  }
  
  soilstab_tall <- dplyr::bind_rows(soilstab_aim, soilstab_lmf)
  saveRDS(soilstab_tall,
          file = paste(outfolder, "soilstab_tall.Rdata", sep = "")
  )
  rm(soilstab_aim, soilstab_lmf)
  invisible(gc())
  
  # LPI ####
  print("Preparing LPI data")
  
  if(("tblLPIDetail" %in% names_rda & "tblLPIHeader" %in% names_rda) |
     ("tblLPIDetail" %in% names_gdb & "tblLPIHeader" %in% names_gdb)){
    lpi_aim <- gather_lpi(dsn = dsn, file_type = "gdb", source = "AIM",
                          tblLPIDetail = tblLPIDetail, tblLPIHeader = tblLPIHeader)
  } else {
    lpi_aim <- NULL
    print("tblLPIDetail and/or tblLPIHeader not found. AIM LPI will not be prepared.")
  }
  
  if(("PINTERCEPT" %in% names_rda) |
     ("PINTERCEPT" %in% names_gdb)){
    lpi_lmf <- gather_lpi(dsn = dsn, file_type = "gdb", source = "LMF", 
                          PINTERCEPT = PINTERCEPT)
  } else {
    lpi_lmf <- NULL
    print("PINTERCEPT not found. LMF LPI will not be prepared.")
  }
  
  lpi_tall <- dplyr::bind_rows(lpi_aim, lpi_lmf)
  saveRDS(lpi_tall,
          file = paste(outfolder, "lpi_tall.Rdata", sep = "")
  )
  rm(lpi_aim, lpi_lmf)
  invisible(gc())
  
  # Height ####
  print("Preparing Vegetation Height data")
  if(("tblLPIDetail" %in% names_rda & "tblLPIHeader" %in% names_rda) |
     ("tblLPIDetail" %in% names_gdb & "tblLPIHeader" %in% names_gdb)){
    height_aim <- gather_height(dsn = dsn, file_type = "gdb", source = "AIM",
                                tblLPIDetail = tblLPIDetail, tblLPIHeader = tblLPIHeader)
  } else {
    height_aim <- NULL
    print("tblLPIDetail and/or tblLPIHeader not found. AIM Height will not be prepared.")
  }
  
  if(("PASTUREHEIGHTS" %in% names_rda) |
     ("PASTUREHEIGHTS" %in% names_gdb)){
    
    height_lmf <- gather_height(dsn = dsn, file_type = "gdb", source = "LMF",
                                PASTUREHEIGHTS = PASTUREHEIGHTS)
  } else {
    height_lmf <- NULL
    print("PASTUREHEIGHTS not found. LMF Height will not be prepared.")
  }
  
  
  height_tall <- dplyr::bind_rows(height_aim, height_lmf)
  saveRDS(height_tall,
          file = paste(outfolder, "height_tall.Rdata", sep = ""))
  rm(height_lmf, height_aim)
  invisible(gc())
  
  # Species inventory ####
  print("Preparing Species Inventory data")
  if(("tblSpecRichDetail" %in% names_rda & "tblSpecRichHeader" %in% names_rda) |
     ("tblSpecRichDetail" %in% names_gdb & "tblSpecRichHeader" %in% names_gdb)){
    spp_inventory_aim <- gather_species_inventory(dsn = dsn, source = "AIM", 
                                                  tblSpecRichDetail = tblSpecRichDetail,
                                                  tblSpecRichHeader = tblSpecRichHeader)
    
  } else {
    spp_inventory_aim <- NULL
    print("tblSpecRichDetail and/or tblSpecRichHeader not found. AIM Species Inventory will not be prepared.")
  }
  if(("PLANTCENSUS" %in% names_rda) |
     ("PLANTCENSUS" %in% names_gdb)){
    
    spp_inventory_lmf <- gather_species_inventory(dsn = dsn, source = "LMF", 
                                                  PLANTCENSUS = PLANTCENSUS,
                                                  file_type = "gdb")
  } else {
    spp_inventory_lmf <- NULL
    print("PLANTCENSUS not found. LMF Species Inventory will not be prepared.")
  }
  
  spp_inventory_tall <- dplyr::bind_rows(spp_inventory_aim, spp_inventory_lmf)
  saveRDS(spp_inventory_tall,
          file = paste(outfolder, "spp_inventory_tall.Rdata", sep = ""))
  rm(spp_inventory_aim, spp_inventory_lmf)
  invisible(gc())
  
  # soil horizons ####
  print("Preparing Soil Horizon data")
  
  if(("tblSoilPitHorizons" %in% names_rda) |
     ("tblSoilPitHorizons" %in% names_gdb)){
    
    hz_aim <- gather_soil_horizon(dsn = dsn, source = "AIM",
                                  tblSoilPitHorizons = tblSoilPitHorizons)
  } else {
    hz_aim <- NULL
    print("tblSoilPitHorizons not found. AIM Horizons will not be prepared.")
  }
  if(("SOILHORIZON" %in% names_rda) |
     ("SOILHORIZON" %in% names_gdb)){
    
    hz_lmf <- gather_soil_horizon(dsn = dsn, source = "LMF", SOILHORIZON = SOILHORIZON)
  } else {
    hz_lmf <- NULL
    print("SOILHORIZON not found. LMF Horizons will not be prepared.")
  }
  hz_tall <- dplyr::bind_rows(hz_aim, hz_lmf)
  saveRDS(hz_tall, file = paste0(outfolder, "soil_horizons_tall.rdata"))
  rm(hz_aim, hz_lmf)
  invisible(gc())
  
  # soil summary ####
  print("Preparing Soil Pit Summary data")
  if(("tblSoilPitHorizons" %in% names_rda & "tblSoilPits" %in% names_rda) |
     ("tblSoilPitHorizons" %in% names_gdb & "tblSoilPits" %in% names_gdb)){
    pit_aim <- gather_soil_summary(dsn = dsn, source = "AIM", 
                                   tblSoilPitHorizons = tblSoilPitHorizons,
                                   tblSoilPits = tblSoilPits)
  } else {
    pit_aim <- NULL
    print("tblSoilPitHorizons and/or tblSoilPits not found. AIM Pit Summary will not be prepared.")
  }
  if(("SOILHORIZON" %in% names_rda) |
     ("SOILHORIZON" %in% names_gdb)){
    pit_lmf <- gather_soil_summary(dsn = dsn, source = "LMF", SOILHORIZON = SOILHORIZON)
  } else {
    pit_lmf <- NULL
    print("SOILHORIZON not found. LMF Pit Summary will not be prepared.")
  }
  pit_tall <- dplyr::bind_rows(pit_aim, pit_lmf)
  saveRDS(pit_tall, file = paste0(outfolder, "soil_summary_tall.Rdata"))
  rm(pit_aim, pit_lmf)
  invisible(gc())
  
  # iirh ####
  print("Preparing Rangeland Health data")
  if(("tblQualDetail" %in% names_rda & "tblQualHeader" %in% names_rda) |
     ("tblQualDetail" %in% names_gdb & "tblQualHeader" %in% names_gdb)){
    
    iirh_aim <- gather_rangeland_health(dsn = dsn, source = "AIM",
                                        tblQualDetail = tblQualDetail,
                                        tblQualHeader = tblQualHeader)
  } else {
    iirh_aim <- NULL
    print("tblQualDetail and/or tblQualHeader not found. AIM Rangeland Health will not be prepared.")
  }
  if(("RANGEHEALTH" %in% names_rda) |
     ("RANGEHEALTH" %in% names_gdb)){
    
    iirh_lmf <- gather_rangeland_health(dsn = dsn, source = "LMF",
                                        RANGEHEALTH = RANGEHEALTH)
    
  } else {
    iirh_lmf <- NULL
    print("RANGEHEALTH not found. LMF Rangeland Health will not be prepared.")
  }
  iirh_tall <- dplyr::bind_rows(iirh_aim, iirh_lmf)
  saveRDS(iirh_tall, file = paste0(outfolder, "rangeland_health_tall.Rdata"))
  rm(iirh_aim, iirh_lmf)
  invisible(gc())
  
  # # header ####
  # header_aim <- gather_header(dsn = dsn, source = "AIM")
  # header_lmf <- gather_header(dsn = dsn, source = "LMF")
  # header <- dplyr::bind_rows(header_aim, header_lmf)
  # saveRDS(header,
  #         file = paste(outfolder, "header.Rdata", sep = "")
  # )
  # 
  # plot characterization ####
  print("Preparing Plot Characterization data")
  if(("tblPlots" %in% names_rda) |
     ("tblPlots" %in% names_gdb)){
    
    plotchar_aim <- gather_plot_characterization(dsn = dsn,
                                                 source = "AIM",
                                                 tblPlots = tblPlots)
    
  } else {
    plotchar_aim <- NULL
    print("tblPlots not found. AIM Plot Characterization will not be prepared.")
  }
  if(("POINT" %in% names_rda & "POINTCOORDINATES" %in% names_rda & "GPS" %in% names_rda) |
     ("POINT" %in% names_gdb & "POINTCOORDINATES" %in% names_gdb & "GPS" %in% names_gdb)){
    
    plotchar_lmf <- gather_plot_characterization(dsn = dsn,
                                                 source = "LMF",
                                                 POINT = POINT,
                                                 POINTCOORDINATES = POINTCOORDINATES,
                                                 GPS = GPS)
  } else {
    plotchar_lmf <- NULL
    print("POINT and/or GPS and/or POINTCOORDINATES not found. LMF Plot Characterization will not be prepared.")
  }
  plotchar_tall <- dplyr::bind_rows(plotchar_aim, plotchar_lmf)
  saveRDS(plotchar_tall, file = paste0(outfolder, "plot_characterization.Rdata"))
  rm(plotchar_aim, plotchar_lmf)
  invisible(gc())
  
  # output ####
  
  list_out <- list(
    gap_tall, height_tall, hz_tall, lpi_tall, pit_tall, plotchar_tall, 
    soilstab_tall, spp_inventory_tall
  )
  
  names(list_out) <- c("Gap", "VegHeight", "SoilHorizons", "LPI", "SoilPitSummary",
                       "PlotCharacterization", "SoilStability", "SpeciesInventory")
  
  return(list_out)
  #####
}
