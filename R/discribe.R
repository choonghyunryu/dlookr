#' Compute descriptive statistic
#'
#' @description The describe() compute descriptive statistic of numeric
#' variable for exploratory data analysis.
#'
#' @details This function is useful when used with the \code{\link{group_by}} function
#' of the dplyr package.
#' If you want to calculate the statistic by level of the categorical data
#' you are interested in, rather than the whole statistic, you can use
#' grouped_df as the group_by() function.
#'
#' @section Descriptive statistic information:
#' The information derived from the numerical data describe is as follows.
#'
#' \itemize{
#' \item n : number of observations excluding missing values
#' \item na : number of missing values
#' \item mean : arithmetic average
#' \item sd : standard devation
#' \item se_mean : standrd error mean. sd/sqrt(n)
#' \item IQR : interquartile range (Q3-Q1)
#' \item skewness : skewness
#' \item kurtosis : kurtosis
#' \item p25 : Q1. 25\% percentile
#' \item p50 : median. 50\% percentile
#' \item p75 : Q3. 75\% percentile
#' \item p01, p05, p10, p20, p30 : 1\%, 5\%, 20\%, 30\% percentiles
#' \item p40, p60, p70, p80 : 40\%, 60\%, 70\%, 80\% percentiles
#' \item p90, p95, p99, p100 : 90\%, 95\%, 99\%, 100\% percentiles
#'
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, describe() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @return An object of the same class as .data.
#' @seealso \code{\link{diagnose_numeric}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Describe descriptive statistics of numerical variables
#' describe(carseats)
#'
#' # Select the variable to describe
#' describe(carseats, Sales, Price)
#' describe(carseats, -Sales, -Price)
#' describe(carseats, 5)
#'
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(carseats, ShelveLoc, US)
#' describe(gdata, "Income")
#'
#' # Using pipes ---------------------------------
#' # Positive values select variables
#' carseats %>%
#'  describe(Sales, CompPrice, Income)
#'
#' # Negative values to drop variables
#' carseats %>%
#'  describe(-Sales, -CompPrice, -Income)
#'
#' # Using pipes & dplyr -------------------------
#' # Find the statistic of all numerical variables by 'ShelveLoc' and 'US',
#' # and extract only those with 'ShelveLoc' variable level is "Good".
#' carseats %>%
#'  group_by(ShelveLoc, US) %>%
#'    describe() %>%
#'    filter(ShelveLoc == "Good")
#'
#' # extract only those with 'Urban' variable level is "Yes",
#' # and find 'Sales' statistics by 'ShelveLoc' and 'US'
#' carseats %>%
#'  filter(Urban == "Yes") %>%
#'  group_by(ShelveLoc, US) %>%
#'  describe(Sales)
describe <- function(.data, ...) {
  UseMethod("describe")
}


#' @method describe data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
describe.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  describe_impl(.data, vars)
}

#' @import tibble
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom RcmdrMisc numSummary
#' @importFrom tidyr unnest
describe_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !is.tibble(df)) df <- as.tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  num_summary <- function(x) {
    stats <- c("mean", "sd", "se(mean)", "IQR", "quantiles",
      "skewness", "kurtosis")
    quant <- c(0, .01, .05, .1, .2, .25, .3, .4, .5,
      .6, .7, .75, .8, .9, .95, .99, 1)
    vname <- c("mean", "sd", "se_mean", "IQR", "skewness", "kurtosis",
      "p00", "p01", "p05", "p10", "p20", "p25", "p30", "p40", "p50",
      "p60", "p70", "p75", "p80", "p90", "p95", "p99", "p100")

    result <- RcmdrMisc::numSummary(x, statistics = stats,
      quantiles = quant)

    numsum <- as.tibble(result$table)
    names(numsum) <- vname

    add_column(numsum, n = result$n,
      na = ifelse(is.null(result$NAs), 0, result$NAs),
      .before = 1)
  }

  statistic <- lapply(vars[idx_numeric],
    function(x) num_summary(pull(df, x)))

  tibble(variable = vars[idx_numeric], statistic) %>%
    tidyr::unnest()
}


#' @method describe grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
describe.grouped_df <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  describe_group_impl(.data, vars)
}

#' @import tibble
#' @import dplyr
#' @importFrom RcmdrMisc numSummary
#' @importFrom purrr map_df
#' @importFrom tidyr unnest
describe_group_impl <- function(df, vars, margin) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !is.tibble(df)) df <- as.tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  num_summary <- function(x, .data, vars) {
    n <- length(x)

    stats <- c("mean", "sd", "se(mean)", "IQR", "quantiles",
      "skewness", "kurtosis")

    quant <- c(0, .01, .05, .1, .2, .25, .3, .4, .5,
      .6, .7, .75, .8, .9, .95, .99, 1)

    vname <- c("mean", "sd", "se_mean", "IQR", "skewness", "kurtosis",
      "p00", "p01", "p05", "p10", "p20", "p25", "p30", "p40", "p50",
      "p60", "p70", "p75", "p80", "p90", "p95", "p99", "p100")

    if (n <= 3) {
      stats <- stats[-c(6:7)]
    }

    result <- RcmdrMisc::numSummary(.data[x + 1, vars],
      statistics = stats, quantiles = quant)

    numsum <- as.tibble(result$table)

    if (n <= 3) {
      numsum <- cbind(numsum[, 1:4], skewness = NA,
        kurtosis = NA, numsum[, 5:21])
    }

    names(numsum) <- vname

    add_column(numsum, n = result$n,
      na = ifelse(is.null(result$NAs), 0, result$NAs),
      .before = 1)
  }

  call_summary <- function(vars) {
    statistic <- purrr::map_df(attr(df, "indices"),
      num_summary, df, vars)

    dplyr::bind_cols(tibble(variable = rep(vars, nrow(statistic))),
      as.tibble(attr(df, "labels")), statistic)
  }

  statistic <- lapply(vars[idx_numeric], function(x) call_summary(x))

  desc <- tibble(statistic) %>%
    tidyr::unnest()

  desc
}

