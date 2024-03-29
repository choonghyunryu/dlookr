% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{transform}
\alias{transform}
\title{Data Transformations}
\usage{
transform(
  x,
  method = c("zscore", "minmax", "log", "log+1", "sqrt", "1/x", "x^2", "x^3", "Box-Cox",
    "Yeo-Johnson")
)
}
\arguments{
\item{x}{numeric vector for transformation.}

\item{method}{method of transformations.}
}
\value{
An object of transform class.
Attributes of transform class is as follows.
\itemize{
  \item method : method of transformation data.
  \itemize{
    \item Standardization
      \itemize{
        \item "zscore" : z-score transformation. (x - mu) / sigma
        \item "minmax" : minmax transformation. (x - min) / (max - min)
      }
    \item Resolving Skewness
    \itemize{
      \item "log" : log transformation. log(x)
      \item "log+1" : log transformation. log(x + 1). Used for values that contain 0.
      \item "sqrt" : square root transformation.
      \item "1/x" : 1 / x transformation
      \item "x^2" : x square transformation
      \item "x^3" : x^3 square transformation
      \item "Box-Cox" : Box-Box transformation
      \item "Yeo-Johnson" : Yeo-Johnson transformation
    }
  }
}
}
\description{
Performs variable transformation for standardization and resolving skewness
of numerical variables.
}
\details{
transform() creates an transform class.
The `transform` class includes original data, transformed data,
and method of transformation.

See vignette("transformation") for an introduction to these concepts.
}
\examples{
\donttest{
# Standardization ------------------------------
creatinine_minmax <- transform(heartfailure$creatinine, method = "minmax")
creatinine_minmax
summary(creatinine_minmax)
plot(creatinine_minmax)

# Resolving Skewness  --------------------------
creatinine_log <- transform(heartfailure$creatinine, method = "log")
creatinine_log
summary(creatinine_log)

plot(creatinine_log)

plot(creatinine_log, typographic = FALSE)

# Using dplyr ----------------------------------
library(dplyr)

heartfailure \%>\%
  mutate(creatinine_log = transform(creatinine, method = "log+1")) \%>\%
  lm(sodium ~ creatinine_log, data = .)
}
   
}
\seealso{
\code{\link{summary.transform}}, \code{\link{plot.transform}}.
}
