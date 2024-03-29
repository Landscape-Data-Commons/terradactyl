% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/species.R, R/species_QC.R
\name{gather_species}
\alias{gather_species}
\alias{generic_growth_habits}
\alias{species_join}
\alias{species_list_check}
\alias{species_list_compare}
\title{Gather species attribute data}
\usage{
gather_species(
  species_file,
  species_growth_habit_code = "GrowthHabitSub",
  growth_habit_file = "",
  growth_habit_code = "Code"
)

generic_growth_habits(
  data,
  data_code = "code",
  species_list,
  species_code = "SpeciesCode",
  species_growth_habit_code = "GrowthHabitSub",
  species_duration = "Duration"
)

species_join(
  data,
  data_code = "code",
  species_file,
  species_code = "SpeciesCode",
  species_growth_habit_code = "GrowthHabitSub",
  species_duration = "Duration",
  growth_habit_file = "",
  growth_habit_code = "Code",
  overwrite_generic_species = FALSE,
  generic_species_file = "",
  by_species_key = TRUE
)

species_list_check(dsn_tall, species_list_file, ...)

species_list_compare(species_file, folder)
}
\arguments{
\item{species_file}{Character string. The full file path (including file extension)
to the file containing the species list OR the species list as a data frame.}

\item{species_growth_habit_code}{Character. The field name for the growth habit
codes in the species file. Defaults to \code{"GrowthHabitSub"}}

\item{growth_habit_file}{Character string. The full file path (including file extension)
to the file containing the growth habit list. If \code{""} we assume the species list contains those values. Defaults to \code{""}.}

\item{growth_habit_code}{Character. The field name for the growth habit codes
in the growth habit file. Defaults to \code{"Code"}}

\item{data}{Dataframe containing species data}

\item{data_code}{Character. The field name with the species codes in the data.}

\item{species_list}{Dataframe. Species list output from \code{}}

\item{species_code}{Character. The field name for the species codes in the species file.}

\item{species_duration}{Character. the field name for the Duration field in the species file.}

\item{generic_species_file}{Character. The full file path (including file extension)to the file containing the species list.}

\item{by_species_key}{Logical. If \code{TRUE} then the join will attempt to use the variable \code{"SpeciesState"} if it exists. Defaults to \code{TRUE}.}

\item{dsn_tall}{The observed data data source}

\item{species_list_file}{The file path to the species list. If the dsn contains the species list, then specify the layer within the dsn.}
}
\description{
Gather species attributes and join to species observations.

Quality control species attribute lists prior to running indicator calculations.
}
