% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_model_RAP.R
\name{data_model_RAP}
\alias{data_model_RAP}
\title{RAP data model
Build data inputs for the Rangeland Analysis Platform.}
\usage{
data_model_RAP(lpi_species, header)
}
\arguments{
\item{lpi_species}{Dataframe in environment. An LPI tall table generated from \code{gather_lpi()} and \code{species_join()}.}

\item{header}{Dataframe in environment. Header table generated from \code{gather_header()}.}
}
\description{
RAP data model
Build data inputs for the Rangeland Analysis Platform.
}
\examples{
RAP_inputs <- terradactyl::data_model_RAP(lpi_species,
                                          header)

}
