#' @rdname describe.data.frame
#' @export
describe <- function(.data, ...) {
  UseMethod("describe", .data)
}


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
#' \item sd : standard deviation
#' \item se_mean : standard error mean. sd/sqrt(n)
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
#' @param .data a data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
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
#' @seealso \code{\link{describe.tbl_dbi}}, \code{\link{diagnose_numeric.data.frame}}.
#' @export
#' @examples
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#'
#' # Describe descriptive statistics of numerical variables
#' describe(heartfailure2)
#'
#' # Select the variable to describe
#' describe(heartfailure2, sodium, platelets)
#' describe(heartfailure2, -sodium, -platelets)
#' describe(heartfailure2, 5)
#'
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(heartfailure2, hblood_pressure, death_event)
#' describe(gdata, "creatinine")
#'
#' # Using pipes ---------------------------------
#' # Positive values select variables
#' heartfailure2 %>%
#'  describe(platelets, sodium, creatinine)
#'
#' # Negative values to drop variables
#' heartfailure2 %>%
#'  describe(-platelets, -sodium, -creatinine)
#'
#' # Using pipes & dplyr -------------------------
#' # Find the statistic of all numerical variables by 'hblood_pressure' and 'death_event',
#' # and extract only those with 'hblood_pressure' variable level is "Yes".
#' heartfailure2 %>%
#'  group_by(hblood_pressure, death_event) %>%
#'    describe() %>%
#'    filter(hblood_pressure == "Yes")
#'
#' # extract only those with 'smoking' variable level is "Yes",
#' # and find 'creatinine' statistics by 'hblood_pressure' and 'death_event'
#' heartfailure2 %>%
#'  filter(smoking == "Yes") %>%
#'  group_by(hblood_pressure, death_event) %>%
#'  describe(creatinine)
#' @method describe data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
#' @rdname describe.data.frame
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

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  num_summary <- function(x) {
    stats <- c("mean", "sd", "se(mean)", "IQR", "quantiles",
      "skewness", "kurtosis")
    quant <- c(0, .01, .05, .1, .2, .25, .3, .4, .5,
      .6, .7, .75, .8, .9, .95, .99, 1)
    vname <- c("mean", "sd", "se_mean", "IQR", "skewness", "kurtosis",
      "p00", "p01", "p05", "p10", "p20", "p25", "p30", "p40", "p50",
      "p60", "p70", "p75", "p80", "p90", "p95", "p99", "p100")
    
    cnt_complete <- sum(complete.cases(x))
    
    numsum <- matrix(NA, ncol = length(vname) + 2, nrow = 1,
                     dimnames = list(NULL, paste0("C", seq(length(vname) + 2))))
    numsum <- as_tibble(numsum)
    
    if (cnt_complete >= 4) {
      result <- RcmdrMisc::numSummary(x, statistics = stats,
                                      quantiles = quant)
      numsum[, 3:(length(vname) + 2)] <- as_tibble(result$table)
    } else {
      if (cnt_complete <= 2) {
        result <- RcmdrMisc::numSummary(x, statistics = stats[1:5],
                                        quantiles = quant)
        numsum[, 3:6] <- as_tibble(result$table)[, 1:4]
        numsum[, 9:(length(vname) + 2)] <- as_tibble(result$table)[, 5:21]
      } else if (cnt_complete <= 3) {
        result <- RcmdrMisc::numSummary(x, statistics = stats[1:6],
                                        quantiles = quant)
        numsum[, 3:7] <- as_tibble(result$table)[, 1:5]
        numsum[, 9:(length(vname) + 2)] <- as_tibble(result$table)[, 6:22]
      }
    }
    
    names(numsum)[3:(length(vname) + 2)] <- vname

    numsum[, 1] <- cnt_complete
    numsum[, 2] <- length(x) - cnt_complete

    names(numsum)[1:2] <- c("n", "na")
    
    numsum
  }

  statistic <- lapply(vars[idx_numeric],
                      function(x) num_summary(pull(df, x)))

  tibble(variable = vars[idx_numeric], statistic) %>%
    tidyr::unnest(cols = c(statistic))
}

#' @rdname describe.data.frame
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

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  num_summary <- function(x, .data, vars) {
    stats <- c("mean", "sd", "se(mean)", "IQR", "quantiles",
      "skewness", "kurtosis")

    quant <- c(0, .01, .05, .1, .2, .25, .3, .4, .5,
      .6, .7, .75, .8, .9, .95, .99, 1)

    vname <- c("mean", "sd", "se_mean", "IQR", "skewness", "kurtosis",
      "p00", "p01", "p05", "p10", "p20", "p25", "p30", "p40", "p50",
      "p60", "p70", "p75", "p80", "p90", "p95", "p99", "p100")

    if (utils::packageVersion("dplyr") >= "0.8.0") flag <- 0
    else flag <- 1
    
    cnt_complete <- sum(complete.cases(.data[x + flag, vars]))
    
    if (cnt_complete <= 2) {
      stats <- stats[-c(6:7)]
    } else if (cnt_complete <= 3) {
      stats <- stats[-c(7)]
    }
    
    result <- RcmdrMisc::numSummary(.data[x + flag, vars],
      statistics = stats, quantiles = quant)

    numsum <- as_tibble(result$table)

    if (cnt_complete <= 2) {
      numsum <- cbind(numsum[, 1:4], skewness = NA,
        kurtosis = NA, numsum[, 5:21])
    } else if (cnt_complete <= 3) {
      numsum <- cbind(numsum[, 1:5], kurtosis = NA, numsum[, 5:21])
    }

    names(numsum) <- vname

    add_column(numsum, n = result$n,
      na = ifelse(is.null(result$NAs), 0, result$NAs),
      .before = 1)
  }

  call_summary <- function(vars) {
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      statistic <- purrr::map_df(attr(df, "groups") %>% 
                                   select(tidyselect::matches("\\.rows")) %>% 
                                   pull, num_summary, df, vars)
      
      glables <- attr(df, "groups") %>% 
        select(-tidyselect::matches("\\.rows"))
    } else {
      statistic <- purrr::map_df(attr(df, "indices"),
                                 num_summary, df, vars)
      
      glables <- attr(df, "labels")
    }  

    dplyr::bind_cols(tibble(variable = rep(vars, nrow(statistic))),
      as_tibble(glables), statistic)
  }

  statistic <- lapply(vars[idx_numeric], function(x) call_summary(x))

  desc <- tibble(statistic) %>%
    tidyr::unnest(cols = c(statistic))

  desc
}

