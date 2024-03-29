% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnose.R
\name{diagnose_category}
\alias{diagnose_category}
\alias{diagnose_category.data.frame}
\alias{diagnose_category.grouped_df}
\title{Diagnose data quality of categorical variables}
\usage{
diagnose_category(.data, ...)

\method{diagnose_category}{data.frame}(
  .data,
  ...,
  top = 10,
  type = c("rank", "n")[2],
  add_character = TRUE,
  add_date = TRUE
)

\method{diagnose_category}{grouped_df}(
  .data,
  ...,
  top = 10,
  type = c("rank", "n")[2],
  add_character = TRUE,
  add_date = TRUE
)
}
\arguments{
\item{.data}{a data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.}

\item{...}{one or more unquoted expressions separated by commas.
You can treat variable names like they are positions.
Positive values select variables; negative values to drop variables.
If the first expression is negative, diagnose_category() will automatically
start with all variables.
These arguments are automatically quoted and evaluated in a context where
column names represent column positions.
They support unquoting and splicing.}

\item{top}{an integer. Specifies the upper top rows or rank to extract.
Default is 10.}

\item{type}{a character string specifying how result are extracted.
"rank" that extract top n ranks by decreasing frequency. 
In this case, if there are ties in rank, more rows than the number specified 
by the top argument are returned.
Default is "n" extract only top n rows by decreasing frequency. 
If there are too many rows to be returned because there are too many ties, 
you can adjust the returned rows appropriately by using "n".}

\item{add_character}{logical. Decide whether to include text variables in the
diagnosis of categorical data. The default value is TRUE, which also includes character variables.}

\item{add_date}{ogical. Decide whether to include Date and POSIXct variables in the
diagnosis of categorical data. The default value is TRUE, which also includes character variables.}
}
\value{
an object of tbl_df.
}
\description{
The diagnose_category() produces information for
diagnosing the quality of the variables of data.frame or tbl_df.
}
\details{
The scope of the diagnosis is the occupancy status of the levels
in categorical data. If a certain level of occupancy is close to 100%,
then the removal of this variable in the forecast model will have to be
considered. Also, if the occupancy of all levels is close to 0%, this
variable is likely to be an identifier.
}
\section{Categorical diagnostic information}{

The information derived from the categorical data diagnosis is as follows.

\itemize{
\item variables : variable names
\item levels: level names
\item N : number of observation
\item freq : number of observation at the levels
\item ratio : percentage of observation at the levels
\item rank : rank of occupancy ratio of levels
}

See vignette("diagonosis") for an introduction to these concepts.
}

\examples{
\donttest{
# Diagnosis of categorical variables
diagnose_category(jobchange)

# Select the variable to diagnose
diagnose_category(jobchange, education_level, company_type)

# Using pipes ---------------------------------
library(dplyr)

# Diagnosis of all categorical variables
jobchange \%>\%
  diagnose_category()

# Positive values select variables
jobchange \%>\%
 diagnose_category(company_type, job_chnge)
 
# Negative values to drop variables
jobchange \%>\%
  diagnose_category(-company_type, -job_chnge)
  
# Top rank levels with top argument
jobchange \%>\%
  diagnose_category(top = 2)
  
# Using pipes & dplyr -------------------------
# Extraction of level that is more than 60\% of categorical data
jobchange \%>\%
  diagnose_category()  \%>\%
  filter(ratio >= 60)

# All observations of enrollee_id have a rank of 1. 
# Because it is a unique identifier. Therefore, if you select up to the top rank 3, 
# all records are displayed. It will probably fill your screen.

# extract rows that less than equal rank 3
# default of type argument is "n"
jobchange \%>\% 
  diagnose_category(enrollee_id, top = 3)

# extract rows that less than equal rank 3
jobchange \%>\% 
  diagnose_category(enrollee_id, top = 3, type = "rank")
 
# extract only 3 rows
jobchange \%>\% 
  diagnose_category(enrollee_id, top = 3, type = "n")

# Using group_by ------------------------------
# Calculate the diagnosis of 'company_type' variable by 'job_chnge' using group_by()
jobchange \%>\%
  group_by(job_chnge) \%>\% 
  diagnose_category(company_type) 
}   
  
}
\seealso{
\code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose.data.frame}}, \code{\link{diagnose_numeric.data.frame}}, \code{\link{diagnose_outlier.data.frame}}.
}
