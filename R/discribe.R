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
#' @param statistics character. the name of the descriptive statistic to calculate. The defaults is c("mean", "sd", "se_mean", "IQR", "skewness", "kurtosis", "quantiles")
#' @param quantiles numeric. list of quantiles to calculate. The values of elements must be between 0 and 1. and to calculate quantiles, you must include "quantiles" in the statistics argument value. The default is c(0, .01, .05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99, 1).
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
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#'
#' # Describe descriptive statistics of numerical variables
#' describe(heartfailure2)
#'
#' # Select the variable to describe
#' describe(heartfailure2, sodium, platelets, statistics = c("mean", "sd", "quantiles"))
#' describe(heartfailure2, -sodium, -platelets)
#' describe(heartfailure2, 5, statistics = c("mean", "sd", "quantiles"), quantiles = c(0.01, 0.1))
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
#'   describe(platelets, sodium, creatinine)
#'
#' # Negative values to drop variables
#' heartfailure2 %>%
#'   describe(-platelets, -sodium, -creatinine)
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
#'   filter(smoking == "Yes") %>%
#'   group_by(hblood_pressure, death_event) %>%
#'   describe(creatinine)
#' }
#' 
#' @method describe data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
#' @rdname describe.data.frame
describe.data.frame <- function(.data, ..., statistics = NULL, quantiles = NULL) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  describe_impl(.data, vars, statistics, quantiles)
}

#' @import tibble
#' @import dplyr
describe_impl <- function(df, vars, statistics, quantiles) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  if (is.null(statistics))
    statistics <- c("mean", "sd", "se_mean", "IQR", "skewness",
                    "kurtosis", "quantiles")
  
  if (is.null(quantiles))
    quantiles <- c(0, .01, .05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
                   0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
  quantiles <- unique(quantiles)

  num_summarise(df[, vars][idx_numeric], statistics, quantiles)
}

#' @rdname describe.data.frame
#' @method describe grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
describe.grouped_df <- function(.data, ..., statistics = NULL, quantiles = NULL) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  describe_group_impl(.data, vars, statistics, quantiles)
}

#' @import tibble
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom tibble is_tibble as_tibble
#' @importFrom tidyselect matches
describe_group_impl <- function(df, vars, statistics, quantiles, margin) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  if (is.null(statistics))
    statistics <- c("mean", "sd", "se_mean", "IQR", "skewness",
                    "kurtosis", "quantiles")
  
  if (is.null(quantiles))
    quantiles <- c(0, .01, .05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
                   0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
  quantiles <- unique(quantiles)
  
  .data <- df[, vars][idx_numeric]
  
  suppressWarnings(
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      gdf <- attr(df, "groups")
      n_group <- ncol(gdf) - 1
      
      statistic <- purrr::map_df(seq(nrow(gdf)), function(x) 
        gdf[x, ] %>% select(-tidyselect::matches("\\.rows")) %>% 
          cbind(num_summarise(.data[gdf$.rows[[x]], ], statistics, quantiles))
      )
    } else {
      gdf_index <- attr(df, "indices")
      glables <- attr(df, "labels")
      n_group <- ncol(glables)
      
      statistic <- purrr::map_df(seq(nrow(glables)), function(x) 
        glables[x, ] %>% 
          cbind(num_summarise(.data[gdf_index[[x]], ], statistics, quantiles))
      )
    } 
  )
  
  statistic %>% 
    select(n_group + 1, seq(n_group), (n_group + 1):ncol(statistic))%>% 
    tibble::as_tibble() %>% 
    arrange(variable)
}

