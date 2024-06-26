% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accumulated_species.R
\name{accumulated_species}
\alias{accumulated_species}
\title{Accumulated species presence, cover, and height across Line-point intercept, Vegetation height, and Species inventory methods.}
\usage{
accumulated_species(
  header,
  lpi_tall = NULL,
  height_tall = NULL,
  spp_inventory_tall = NULL,
  species_file = "",
  dead = TRUE,
  source = c("TerrADat", "AIM", "LMF", "NRI"),
  ...,
  generic_species_file = NULL
)
}
\arguments{
\item{header}{File path to header rdata file}

\item{lpi_tall}{File path to LPI rdata file}

\item{height_tall}{File path to height rdata file}

\item{spp_inventory_tall}{File path to species inventory rdata file}

\item{species_file}{File path to species file if you want species attributes or updated species. Geodatabase or csv allowed.}

\item{...}{Optional filtering expression to subset the number of plots}
}
\description{
Accumulated species presence, cover, and height across Line-point intercept, Vegetation height, and Species inventory methods.
}
\examples{
# Get a list of all species occurring on a plot across methods (LPI, height, species inventory)
# This method also adds cover and height by species. Be aware that sample sizes may be insufficient to make an accurate estimate
accumulated_species <- accumulated_species(lpi_tall = "~/AIM/Data/lpi_tall.rdata",
                                                      spp_inventory_tall = "~/AIM/Data/spp_inventory_tall.rdata",
                                                       height_tall = "~/AIM/Data/height_tall.rdata",
                                                       header = "~/AIM/Data/header.rdata",
                                                       species_file = "species_file.csv",
                                                       SpeciesState \%in\% "NM")
}
