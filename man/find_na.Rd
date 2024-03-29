% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_na}
\alias{find_na}
\title{Finding variables including missing values}
\usage{
find_na(.data, index = TRUE, rate = FALSE)
}
\arguments{
\item{.data}{a data.frame or a \code{\link{tbl_df}}.}

\item{index}{logical. When representing the information of a variable including
missing values, specify whether or not the variable is represented by an index.
Returns an index if TRUE or a variable names if FALSE.}

\item{rate}{logical. If TRUE, returns the percentage of missing values
in the individual variable.}
}
\value{
Information on variables including missing values.
}
\description{
Find the variable that contains the missing value in the object
that inherits the data.frame or data.frame.
}
\examples{
find_na(jobchange)

find_na(jobchange, index = FALSE)

find_na(jobchange, rate = TRUE)

## using dplyr -------------------------------------
library(dplyr)

# Perform simple data quality diagnosis of variables with missing values.
jobchange \%>\%
  select(find_na(.)) \%>\%
  diagnose()
}
\seealso{
\code{\link{imputate_na}}, \code{\link{find_outliers}}.
}
