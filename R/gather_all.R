#' Gather tall tables for gap, vegetation height, LPI, plot characterization,
#' IIRH, soil horizon, soil pit summary, soil stability, and species inventory.
#'
#' @description Given wide format AIM/LMF data, gather gap,
#' vegetation height, LPI, header, IIRH, soil horizon,
#' soil pit summary, soil stability, and species inventory data. Missing
#' tables will be skipped. AIM-type and LMF-type data will both be processed.
#' @param dsn Character string. The full filepath and filename (including file
#' extension) of the geodatabase or text file containing thes table of interest.
#' This field is unnecessary if you provide dflist.
#' @param dflist Named list of data frames containing monitoring data. Tables
#' must be named as expected by the individual gather_functions.
#' @param outfolder Character string. Name of a folder to save all output to.
#' If the specified folder does not exist, the function will create it.
#' @param outtype Vector specifying output format, accepting "csv" and "rdata".
#' Defaults to writing both.
#' @param verbose True/False. When true, displays progress information, and
#' reports missing input data.
#' @param doLPI True/False. When false, LPI data will not be gathered. LPI data
#' is large and the gather process is RAM-intensive. This function will function
#' with fewer resources if LPI is run in batches, external to this wrapper.
#' @importFrom magrittr %>%
#' @name gather_all
#' @family <gather>
#' @return A list of tall data frames containing reformatted input data.
#' @examples
#' gather_all(dsn = "Path/To/AIM-LMF_Geodatabase.gdb", outfolder = "output")
#'
#' names <- sf::st_layers(dsn = "Path/To/AIM-LMF_Geodatabase.gdb")$name
#' all_data <- sapply(names, function(n){## Gather Height Data
#'   sf::st_read(dsn = "Path/To/AIM-LMF_Geodatabase.gdb",
#'   layer = n, quiet = T)
#' })
#' gather_all(dflist = all_data, outfolder = "output")


## Gather All Data
#' @export gather_all
#' @rdname gather_all

gather_all <- function(dsn = NULL, dflist = NULL, outfolder, outtype = c("csv", "rdata"),
                       verbose = TRUE, doLPI = TRUE) {
  # prep ####
  outtype <- tolower(outtype)

  if(substr(outfolder, nchar(outfolder), nchar(outfolder)) != "/") {
    outfolder <- paste0(outfolder, "/")
  }

  if(!dir.exists(outfolder)) dir.create(outfolder)

  # if neither dsn or dflist are provided, stop
  if(is.null(dflist) & is.null(dsn)) stop("Provide either dsn or dflist")

  # if both dsn and dflist are provided, drop dsn
  if(!is.null(dflist) & !is.null(dsn)){
    dsn <- NULL
    if(verbose) print("Both dsn and dflist were provided. Dsn will be ignored")
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

  rm(dflist)

  # Gap ####
  if(("tblGapDetail" %in% names_rda & "tblGapHeader" %in% names_rda) |
     ("tblGapDetail" %in% names_gdb & "tblGapHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM gap")
    gap_aim <- gather_gap(dsn = dsn, source = "AIM",
                          tblGapDetail = tblGapDetail,
                          tblGapHeader = tblGapHeader)
  } else {
    gap_aim <- NULL
    if(verbose) print("tblGapDetail and/or tblGapHeader not found. Skipping AIM Gap.")
  }

  if(("GINTERCEPT" %in% names_rda & "POINT" %in% names_rda) |
     ("GINTERCEPT" %in% names_gdb & "POINT" %in% names_gdb)){
    if(verbose) print("Gathering LMF gap")
    gap_lmf <- gather_gap(dsn = dsn, source = "LMF",
                          GINTERCEPT = GINTERCEPT,
                          POINT = POINT)
  } else {
    gap_lmf <- NULL
    if(verbose) print("GINTERCEPT and/or POINT not found. Skipping LMF Gap.")
  }

  gap_tall <- dplyr::bind_rows(gap_aim, gap_lmf)
  if(1 <= nrow(gap_tall)){
    if("csv" %in% outtype){
      write.csv(gap_tall,
                file = paste(outfolder, "gap_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(gap_tall,
              file = paste0(outfolder, "gap_tall.rdata"))
    }

  }
  rm(gap_aim, gap_lmf)
  invisible(gc())

  # Soil stability ####
  if(("tblSoilStabDetail" %in% names_rda & "tblSoilStabHeader" %in% names_rda) |
     ("tblSoilStabDetail" %in% names_gdb & "tblSoilStabHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM soil stability")
    soilstab_aim <- gather_soil_stability(dsn = dsn, source = "AIM",
                                          tblSoilStabDetail = tblSoilStabDetail,
                                          tblSoilStabHeader = tblSoilStabHeader)
  } else {
    soilstab_aim <- NULL
    if(verbose) print("tblSoilStabDetail and/or tblSoilStabHeader not found. Skipping AIM Soil Stability.")
  }

  if(("SOILDISAG" %in% names_rda) |
     ("SOILDISAG" %in% names_gdb)){
    if(verbose) print("Gathering LMF soil stability")
    soilstab_lmf <- gather_soil_stability(dsn = dsn, source = "LMF",
                                          SOILDISAG = SOILDISAG)
  } else {
    soilstab_lmf <- NULL
    if(verbose) print("SOILDISAG not found. Skipping LMF Soil Stability.")
  }

  soilstab_tall <- dplyr::bind_rows(soilstab_aim, soilstab_lmf)
  if(1 <= nrow(soilstab_tall)){

    if("csv" %in% outtype){
      write.csv(soilstab_tall,
                file = paste(outfolder, "soil_stability_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(soilstab_tall,
              file = paste0(outfolder, "soil_stability_tall.rdata"))
    }
  }
  rm(soilstab_aim, soilstab_lmf)
  invisible(gc())

  # LPI ####
  if(doLPI == T){
    if(("tblLPIDetail" %in% names_rda & "tblLPIHeader" %in% names_rda) |
       ("tblLPIDetail" %in% names_gdb & "tblLPIHeader" %in% names_gdb)){
      if(verbose) print("Gathering AIM LPI")
      lpi_aim <- gather_lpi(dsn = dsn, file_type = "gdb", source = "AIM",
                            tblLPIDetail = tblLPIDetail, tblLPIHeader = tblLPIHeader)}



      if(("PINTERCEPT" %in% names_rda) |
         ("PINTERCEPT" %in% names_gdb)){
        if(verbose) print("Gathering LMF LPI")
        lpi_lmf <- gather_lpi(dsn = dsn, file_type = "gdb", source = "LMF",
                              PINTERCEPT = PINTERCEPT)
      } else {
        lpi_lmf <- NULL
        if(verbose) print("PINTERCEPT not found. Skipping LMF LPI.")
      }

      lpi_tall <- dplyr::bind_rows(lpi_aim, lpi_lmf)
      if(1 <= nrow(lpi_tall)){
        if("csv" %in% outtype){
          write.csv(lpi_tall,
                    file = paste(outfolder, "lpi_tall.csv", sep = ""), row.names = F)
        }
        if("rdata" %in% outtype){
          saveRDS(lpi_tall,
                  file = paste0(outfolder, "lpi_tall.rdata"))
        }
      }
      rm(lpi_aim, lpi_lmf)
      invisible(gc())

      # Height ####
      if(("tblLPIDetail" %in% names_rda & "tblLPIHeader" %in% names_rda) |
         ("tblLPIDetail" %in% names_gdb & "tblLPIHeader" %in% names_gdb)){
        if(verbose) print("Gathering AIM Height")
        height_aim <- gather_height(dsn = dsn, file_type = "gdb", source = "AIM",
                                    tblLPIDetail = tblLPIDetail, tblLPIHeader = tblLPIHeader)
      } else {
        height_aim <- NULL
        if(verbose) print("tblLPIDetail and/or tblLPIHeader not found. Skipping AIM Height.")
      }

      if(("PASTUREHEIGHTS" %in% names_rda) |
         ("PASTUREHEIGHTS" %in% names_gdb)){
        if(verbose) print("Gathering LMF Height")
        height_lmf <- gather_height(dsn = dsn, file_type = "gdb", source = "LMF",
                                    PASTUREHEIGHTS = PASTUREHEIGHTS)
      } else {
        height_lmf <- NULL
        if(verbose) print("PASTUREHEIGHTS not found. Skipping LMF Height.")
      }

      height_tall <- dplyr::bind_rows(height_aim, height_lmf)
      if(1 <= nrow(height_tall)){
        if("csv" %in% outtype){
          write.csv(height_tall,
                    file = paste(outfolder, "height_tall.csv", sep = ""), row.names = F)
        }
        if("rdata" %in% outtype){
          saveRDS(height_tall,
                  file = paste0(outfolder, "height_tall.rdata"))
        }
      }
      rm(height_lmf, height_aim)
      invisible(gc())


      } else {
      lpi_aim <- NULL
      if(verbose) print("tblLPIDetail and/or tblLPIHeader not found. Skipping AIM LPI.")
    }

} else {
  print("doLPI is false, skipping all lpi")
}

  # Species inventory ####
  if(("tblSpecRichDetail" %in% names_rda & "tblSpecRichHeader" %in% names_rda) |
     ("tblSpecRichDetail" %in% names_gdb & "tblSpecRichHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM species inventory")
    spp_inventory_aim <- gather_species_inventory(dsn = dsn, source = "AIM",
                                                  tblSpecRichDetail = tblSpecRichDetail,
                                                  tblSpecRichHeader = tblSpecRichHeader)

  } else {
    spp_inventory_aim <- NULL
    if(verbose) print("tblSpecRichDetail and/or tblSpecRichHeader not found. Skipping AIM Species Inventory.")
  }
  if(("PLANTCENSUS" %in% names_rda) |
     ("PLANTCENSUS" %in% names_gdb)){
    if(verbose) print("Gathering LMF species inventory")
    spp_inventory_lmf <- gather_species_inventory(dsn = dsn, source = "LMF",
                                                  PLANTCENSUS = PLANTCENSUS,
                                                  file_type = "gdb")
  } else {
    spp_inventory_lmf <- NULL
    if(verbose) print("PLANTCENSUS not found. Skipping LMF Species Inventory.")
  }

  spp_inventory_tall <- dplyr::bind_rows(spp_inventory_aim, spp_inventory_lmf)
  if(1 <= nrow(spp_inventory_tall)){

    if("csv" %in% outtype){
      write.csv(spp_inventory_tall,
                file = paste(outfolder, "species_inventory_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(spp_inventory_tall,
              file = paste0(outfolder, "species_inventory_tall.rdata"))
    }
  }
  rm(spp_inventory_aim, spp_inventory_lmf)
  invisible(gc())

  # soil horizons ####
  if(("tblSoilPitHorizons" %in% names_rda) |
     ("tblSoilPitHorizons" %in% names_gdb)){
    if(verbose) print("Gathering AIM soil horizon data")
    hz_aim <- gather_soil_horizon(dsn = dsn, source = "AIM",
                                  tblSoilPitHorizons = tblSoilPitHorizons)
  } else {
    hz_aim <- NULL
    if(verbose) print("tblSoilPitHorizons not found. Skipping AIM Horizons.")
  }
  if(("SOILHORIZON" %in% names_rda) |
     ("SOILHORIZON" %in% names_gdb)){
    if(verbose) print("Gathering LMF soil horizon data")
    hz_lmf <- gather_soil_horizon(dsn = dsn, source = "LMF", SOILHORIZON = SOILHORIZON)
  } else {
    hz_lmf <- NULL
    if(verbose) print("SOILHORIZON not found. Skipping LMF Horizons.")
  }
  hz_tall <- dplyr::bind_rows(hz_aim, hz_lmf)
  if(1 <= nrow(hz_tall)){

    if("csv" %in% outtype){
      write.csv(hz_tall,
                file = paste(outfolder, "soil_horizons_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(hz_tall,
              file = paste0(outfolder, "soil_horizons_tall.rdata"))
    }

  }
  rm(hz_aim, hz_lmf)
  invisible(gc())

  # soil summary ####
  if(("tblSoilPitHorizons" %in% names_rda & "tblSoilPits" %in% names_rda) |
     ("tblSoilPitHorizons" %in% names_gdb & "tblSoilPits" %in% names_gdb)){
    if(verbose) print("Gathering AIM soil summary")
    pit_aim <- gather_soil_summary(dsn = dsn, source = "AIM",
                                   tblSoilPitHorizons = tblSoilPitHorizons,
                                   tblSoilPits = tblSoilPits)
  } else {
    pit_aim <- NULL
    if(verbose) print("tblSoilPitHorizons and/or tblSoilPits not found. Skipping AIM Soil Summary.")
  }
  if(("SOILHORIZON" %in% names_rda) |
     ("SOILHORIZON" %in% names_gdb)){
    if(verbose) print("Gathering LMF soil summary")
    pit_lmf <- gather_soil_summary(dsn = dsn, source = "LMF", SOILHORIZON = SOILHORIZON)
  } else {
    pit_lmf <- NULL
    if(verbose) print("SOILHORIZON not found. Skipping LMF soil Summary.")
  }
  pit_tall <- dplyr::bind_rows(pit_aim, pit_lmf)
  if(1 <= nrow(pit_tall)){
    if("csv" %in% outtype){
      write.csv(pit_tall,
                file = paste(outfolder, "pit_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(pit_tall,
              file = paste0(outfolder, "pit_tall.rdata"))
    }
  }

  rm(pit_aim, pit_lmf)
  invisible(gc())

  # iirh ####
  if(("tblQualDetail" %in% names_rda & "tblQualHeader" %in% names_rda) |
     ("tblQualDetail" %in% names_gdb & "tblQualHeader" %in% names_gdb)){
    if(verbose) print("Gathering AIM IIRH data")
    iirh_aim <- gather_rangeland_health(dsn = dsn, source = "AIM",
                                        tblQualDetail = tblQualDetail,
                                        tblQualHeader = tblQualHeader)
  } else {
    iirh_aim <- NULL
    if(verbose) print("tblQualDetail and/or tblQualHeader not found. Skipping AIM Rangeland Health.")
  }
  if(("RANGEHEALTH" %in% names_rda) |
     ("RANGEHEALTH" %in% names_gdb)){
    if(verbose) print("Gathering LMF IIRH data")
    iirh_lmf <- gather_rangeland_health(dsn = dsn, source = "LMF",
                                        RANGEHEALTH = RANGEHEALTH)

  } else {
    iirh_lmf <- NULL
    if(verbose) print("RANGEHEALTH not found. Skipping LMF Rangeland Health.")
  }
  iirh_tall <- dplyr::bind_rows(iirh_aim, iirh_lmf)
  if(1 <= nrow(iirh_tall)){
    if("csv" %in% outtype){
      write.csv(iirh_tall,
                file = paste(outfolder, "rangeland_health_tall.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(iirh_tall,
              file = paste0(outfolder, "rangeland_health_tall.rdata"))
    }  }
  rm(iirh_aim, iirh_lmf)
  invisible(gc())

  # header ####
  if(("tblPlots" %in% names_rda & "tblLPIHeader" %in% names_rda) |
     ("tblPlots" %in% names_gdb & "tblLPIHeader" %in% names_rda)){
    if(verbose) print("Gathering AIM Header")
    header_aim <- gather_header(dsn = dsn, source = "AIM", tblPlots = tblPlots, tblLPIHeader = tblLPIHeader)

#
#     if(verbose) print("Gathering AIM plot characterization")
#     plotchar_aim <- gather_plot_characterization(dsn = dsn,
#                                                  source = "AIM",
#                                                  tblPlots = tblPlots)

  } else {
    header_aim <- NULL

    if(verbose) print("tblPlots not found. Skipping AIM header.")
  }
  if(("POINT" %in% names_rda) |
     ("POINT" %in% names_gdb)){
    if(verbose) print("Gathering LMF header")
    header_lmf <- gather_header(dsn = dsn,
                                  source = "LMF")
  } else {
    header_lmf <- NULL
    if(verbose) print("POINT not found. Skipping LMF header.")
  }

  header_tall <- dplyr::bind_rows(header_aim, header_lmf)
  if(1 <= nrow(header_tall)){
    if("csv" %in% outtype){
      write.csv(header_tall,
                file = paste(outfolder, "header.csv", sep = ""), row.names = F)
    }
    if("rdata" %in% outtype){
      saveRDS(header_tall,
              file = paste0(outfolder, "header.rdata"))
    }  }
  rm(header_aim, header_lmf)
  invisible(gc())

  # # output ####
  # if(doLPI == T){
  #
  # list_out <- list(
  #   gap_tall, height_tall, hz_tall, lpi_tall, pit_tall, header_tall,
  #   soilstab_tall, spp_inventory_tall
  # )
  #
  # names(list_out) <- c("Gap", "VegHeight", "SoilHorizons", "LPI", "SoilPitSummary",
  #                      "Header",
  #                      "SoilStability", "SpeciesInventory")
  #
  # } else {
  #   list_out <- list(
  #     gap_tall, hz_tall, #pit_tall,
  #     header_tall,
  #     soilstab_tall, spp_inventory_tall
  #   )
  #
  #   names(list_out) <- c("Gap", "SoilHorizons", "SoilPitSummary",
  #                        "Header", "SoilStability", "SpeciesInventory")
  # }

  # return(list_out)
}
