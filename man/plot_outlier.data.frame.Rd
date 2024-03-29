% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnose.R
\name{plot_outlier}
\alias{plot_outlier}
\alias{plot_outlier.data.frame}
\title{Plot outlier information of numerical data diagnosis}
\usage{
plot_outlier(.data, ...)

\method{plot_outlier}{data.frame}(
  .data,
  ...,
  col = "steelblue",
  typographic = TRUE,
  base_family = NULL
)
}
\arguments{
\item{.data}{a data.frame or a \code{\link{tbl_df}}.}

\item{...}{one or more unquoted expressions separated by commas.
You can treat variable names like they are positions.
Positive values select variables; negative values to drop variables.
If the first expression is negative, plot_outlier() will automatically start
with all variables.
These arguments are automatically quoted and evaluated in a context
where column names represent column positions.
They support unquoting and splicing.}

\item{col}{a color to be used to fill the bars. The default is "steelblue".}

\item{typographic}{logical. Whether to apply focuses on typographic elements to ggplot2 visualization.}

\item{base_family}{character. The name of the base font family to use 
for the visualization. If not specified, the font defined in dlookr is applied. (See details)
The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.}
}
\description{
The plot_outlier() visualize outlier information
for diagnosing the quality of the numerical data.
}
\details{
The scope of the diagnosis is the provide a outlier information.
Since the plot is drawn for each variable, if you specify more than
one variable in the ... argument, the specified number of plots are drawn.

The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
"NanumSquare", "Noto Sans Korean". If you want to use a different font, 
use it after loading the Google font with import_google_font().
}
\section{Outlier diagnostic information}{

The plot derived from the numerical data diagnosis is as follows.

\itemize{
\item With outliers box plot
\item Without outliers box plot
\item With outliers histogram
\item Without outliers histogram
}

See vignette("diagonosis") for an introduction to these concepts.
}

\examples{
\donttest{
# Visualization of all numerical variables
plot_outlier(heartfailure)

# Select the variable to diagnose using the col argument
plot_outlier(heartfailure, cpk_enzyme, sodium, col = "gray")

# Not allow typographic argument
plot_outlier(heartfailure, cpk_enzyme, typographic = FALSE)

# Using pipes & dplyr -------------------------
library(dplyr)

# Visualization of numerical variables with a ratio of
# outliers greater than 5\%
heartfailure \%>\%
  plot_outlier(heartfailure \%>\%
    diagnose_outlier() \%>\%
    filter(outliers_ratio > 5) \%>\%
    select(variables) \%>\%
    pull())
}

}
\seealso{
\code{\link{plot_outlier.tbl_dbi}}, \code{\link{diagnose_outlier.data.frame}}.
}
