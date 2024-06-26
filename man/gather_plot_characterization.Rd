% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gather_plot_characterization.R
\name{gather_plot_characterization}
\alias{gather_plot_characterization}
\alias{gather_plot_characterization_terradat}
\alias{gather_plot_characterization_lmf}
\title{Convert plot data into a tall, tidy data frame}
\usage{
gather_plot_characterization_terradat(dsn = NULL, tblPlots = NULL)

gather_plot_characterization_lmf(
  dsn = NULL,
  POINT = NULL,
  POINTCOORDINATES = NULL,
  GPS = NULL,
  ESFSG = NULL,
  file_type = NULL
)

gather_plot_characterization(
  dsn = NULL,
  source,
  tblPlots = NULL,
  POINT = NULL,
  POINTCOORDINATES = NULL,
  GPS = NULL,
  ESFSG = NULL,
  file_type = "gdb"
)
}
\arguments{
\item{dsn}{Character string. The full filepath and filename (including file
extension) of the geodatabase or text file containing the table of interest.
This field is unnecessary if you provide either tblPlots (AIM/DIMA/TerrADat)
or POINT (LMF/NRI).}

\item{tblPlots}{Dataframe of the data structure tblPlots from the
DIMA database with the addition of PrimaryKey and DBKey fields. Use when data
source is AIM, DIMA, or TerrADat; alternately provide dsn.}

\item{POINT}{Dataframe of the data structure PINTERCEPT from the LMF/NRI
database with the addition of PrimaryKey and DBKey fields. Use when source
is LMF or NRI; alternately provide dsn.}

\item{POINTCOORDINATES}{Dataframe of the data structure POINTCOORDINATES from the LMF/NRI
database with the addition of PrimaryKey and DBKey fields. Use when source
is LMF or NRI; alternately provide dsn.}

\item{GPS}{Dataframe of the data structure GPS from the LMF/NRI
database with the addition of PrimaryKey and DBKey fields. Use when source
is LMF or NRI; alternately provide dsn.
#' @param file_type Character string that denotes the source file type of the
LMF/NRI data, \code{"gdb"} or \code{"txt"}. Not necessary for
AIM/DIMA/TerrADat, or if POINT, POINTCOORDINATES, and GPS are provided.}

\item{source}{Character string. The data source format,
\code{"AIM", "TerrADat", "DIMA", "LMF", "NRI"} (case independent).}
}
\value{
A tall data frame containing plot characterization data
}
\description{
Given wide format plot data, create a tall format data frame
usable by other terradactyl functions.
}
\examples{
gather_plot_characterization(dsn = "Path/To/AIM_Geodatabase.gdb",
                             source = "AIM")
gather_plot_characterization(dsn = "Path/To/LMF_Geodatabase.gdb",
                             source = "LMF")

aim_plots <- read.csv("Path/To/tblPlots.csv")
gather_plot_characterization(source = "AIM",
                             tblPlots = aim_plots)

lmf_pintercept <- read.csv("Path/To/PINTERCEPT.csv")
lmf_pointcoords <- read.csv("Path/To/POINTCOORDINATES.csv")
lmf_gps <- read.csv("Path/To/GPS.csv")
gather_plot_characterization(source = "LMF",
                             PINTERCEPT = lmf_pintercept,
                             POINTCOORDINATES = lmf_pointcoords,
                             GPS = lmf_gps)
}
\seealso{
Other <gather>: 
\code{\link{gather_all}()},
\code{\link{gather_gap}()},
\code{\link{gather_height}()},
\code{\link{gather_lpi}()},
\code{\link{gather_rangeland_health}()},
\code{\link{gather_soil_horizon}()},
\code{\link{gather_soil_stability}()},
\code{\link{gather_species_inventory}()}
}
\concept{<gather>}
