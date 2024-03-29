% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{summary.transform}
\alias{summary.transform}
\alias{print.transform}
\title{Summarizing transformation information}
\usage{
\method{summary}{transform}(object, ...)

\method{print}{transform}(x, ...)
}
\arguments{
\item{object}{an object of class "transform", usually, a result of a call to transform().}

\item{...}{further arguments passed to or from other methods.}

\item{x}{an object of class "transform",usually, a result of a call to transform().}
}
\description{
print and summary method for "transform" class.
}
\details{
summary.transform compares the distribution of data before and after data transformation.
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
}

}
\seealso{
\code{\link{transform}}, \code{\link{plot.transform}}.
}
