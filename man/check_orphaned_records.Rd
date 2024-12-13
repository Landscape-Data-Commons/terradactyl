% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_qc.R
\name{check_orphaned_records}
\alias{check_orphaned_records}
\title{Identify records without matches in a second data frame}
\usage{
check_orphaned_records(x, y, joining_variables, symmetric = TRUE)
}
\arguments{
\item{x}{Data frame. The data frame which will always be checked for records which do not correspond to any records in \code{y}. Must contain the variables specified in \code{joining_variables}.}

\item{y}{Data frame. The data frame against which \code{x} will be checked. Must contain the variables specified in \code{joining_variables}. If \code{symmetric} is \code{TRUE} then this will also be checked against \code{x}.}

\item{joining_variables}{Character vector or list of character vectors. The variables to use for the join \code{x} between \code{y}. If the variable names are the same in both \code{x} and \code{y}, then a single character vector containing the names of the variables will work. Otherwise, it should be formatted as a list of two character vectors, one for the variable names in \code{x} and one for \code{y}. In the case that you are providing a list, make sure that the variables containing the same values are in the same order between the two vectors. Naming the vectors in the list is optional but recommended: if the names of the vectors in the list are not \code{"x"} and \code{"y"}, then the function will make a best guess, defaulting to assuming that the first vector is for \code{x} and the second for \code{y}.}

\item{symmetric}{Logical. If \code{TRUE} then orphaned records will be found for both \code{x} and \code{y}. If \code{FALSE} then orphaned records will only be found for \code{x}. Defaults to \code{TRUE}.}
}
\value{
If \code{symmetric} is \code{TRUE}, then a named list of two data frames containing all orphaned records from \code{x} and \code{y}. If \code{symmetric} is \code{FALSE} then a single data frame of all orphaned records from \code{x}. If no orphaned records were found, the returned data frame(s) will contain no values.
}
\description{
Given two data frames and a set of variables which should join them, find the records in one or both data frames which have no corresponding records in the other.
}
\examples{
# detail_data <- data.frame(detail_key = rep(x = c(1, 2), times = 3),
#                           detail_value = rep(x = c("a", "b", "c"), times = 2))
# header_data <- data.frame(header_key = rep(x = c(1, 2, 3), times = 2),
#                           header_value = rep(x = c("x", "y", "z"), times = 2))
# check_orphaned_records(x = detail_data,
#                        y = header_data,
#                        joining_variables = list(x = "detail_key",
#                                                 y = "header_key"))
}
