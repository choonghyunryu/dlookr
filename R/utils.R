#' Extracting a class of variables
#'
#' @description The get_class() gets class of variables in data.frame or tbl_df.
#'
#' @param df a data.frame or objects inheriting from data.frame
#' @return a data.frame
#' Variables of data.frame is as follows.
#' \itemize{
#' \item variable : variables name
#' \item class : class of variables
#' }
#' @seealso \code{\link{find_class}}.
#' @export
#' @examples
#' \dontrun{
#' # data.frame
#' get_class(iris)
#'
#' # tbl_df
#' get_class(ggplot2::diamonds)
#'
#' library(dplyr)
#' ggplot2::diamonds %>%
#'   get_class() %>% 
#'   filter(class %in% c("integer", "numeric"))
#' }
#' @export
get_class <- function(df) {
  vars <- sapply(names(df), function(.x.) class(pull(df, .x.))[1])

  data.frame(variable = names(vars), class = vars, row.names = NULL)
}


#' Extract variable names or indices of a specific class
#'
#' @description The find_class() extracts variable information having a
#' certain class from an object inheriting data.frame.
#'
#' @param df a data.frame or objects inheriting from data.frame
#' @param type character. Defines a group of classes to be searched.
#' "numerical" searches for "numeric" and "integer" classes,
#' "categorical" searches for "factor" and "ordered" classes.
#' "categorical2" adds "character" class to "categorical".
#' "date_categorical" adds result of "categorical2" and "Date", "POSIXct".
#' "date_categorical2" adds result of "categorical" and "Date", "POSIXct".
#' @param index logical. If TRUE is return numeric vector that is variables index.
#' and if FALSE is return character vector that is variables name.
#' default is TRUE.
#'
#' @return character vector or numeric vector.
#' The meaning of vector according to data type is as follows.
#' \itemize{
#' \item character vector : variables name
#' \item numeric vector : variables index
#' }
#' @seealso \code{\link{get_class}}.
#' @export
#' @examples
#' \dontrun{
#' # data.frame
#' find_class(iris, "numerical")
#' find_class(iris, "numerical", index = FALSE)
#' find_class(iris, "categorical")
#' find_class(iris, "categorical", index = FALSE)
#'
#' # tbl_df
#' find_class(ggplot2::diamonds, "numerical")
#' find_class(ggplot2::diamonds, "numerical", index = FALSE)
#' find_class(ggplot2::diamonds, "categorical")
#' find_class(ggplot2::diamonds, "categorical", index = FALSE)
#'
#' # type is "categorical2"
#' iris2 <- data.frame(iris, char = "chars",
#'                     stringsAsFactors = FALSE)
#' find_class(iris2, "categorical", index = FALSE)
#' find_class(iris2, "categorical2", index = FALSE)
#' }
#' @importFrom methods is
#' @export
find_class <- function(df, type = c("numerical", "categorical", "categorical2",
                                    "date_categorical", "date_categorical2"), 
                       index = TRUE) {
  if (!is.data.frame(df)) {
    stop("The argument 'df' is not an object inheriting from data.frame.")
  }

  clist <- sapply(seq(df), function(.x.) is(pull(df, .x.))[1])

  type <- match.arg(type)

  if (type == "numerical") {
    idx <- which(clist %in% c("integer", "numeric"))
  } else if (type == "categorical") {
    idx <- which(clist %in% c("factor", "ordered", "labelled"))
  } else if (type == "categorical2") {
    idx <- which(clist %in% c("factor", "ordered", "labelled", "character"))
  } else if (type == "date_categorical") {
    idx <- which(clist %in% c("factor", "ordered", "labelled", "Date", "POSIXct"))
  } else if (type == "date_categorical2") {
    idx <- which(clist %in% c("factor", "ordered", "labelled", "character",
                              "Date", "POSIXct"))
  }

  if (!index) idx <- names(df)[idx]

  idx
}


#' Finding variables including missing values
#'
#' @description
#' Find the variable that contains the missing value in the object
#' that inherits the data.frame or data.frame.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param index logical. When representing the information of a variable including
#' missing values, specify whether or not the variable is represented by an index.
#' Returns an index if TRUE or a variable names if FALSE.
#' @param rate logical. If TRUE, returns the percentage of missing values
#' in the individual variable.
#' @return Information on variables including missing values.
#' @seealso \code{\link{imputate_na}}, \code{\link{find_outliers}}.
#' @examples
#' \dontrun{
#' find_na(jobchange)
#'
#' find_na(jobchange, index = FALSE)
#'
#' find_na(jobchange, rate = TRUE)
#'
#' ## using dplyr -------------------------------------
#' library(dplyr)
#'
#' # Perform simple data quality diagnosis of variables with missing values.
#' jobchange %>%
#'   select(find_na(.)) %>%
#'   diagnose()
#' }
#' @importFrom purrr map_lgl map_dbl
#' @export
#'
find_na <- function(.data, index = TRUE, rate = FALSE) {
  if (rate) {
    idx <- .data %>%
      map_dbl(function(x) sum(is.na(x)) / length(x) * 100) %>%
      round(3)
  } else {
    idx <- .data %>%
      map_lgl(function(x) any(is.na(x))) %>%
      which()
    
    names(idx) <- NULL

    if (!index) idx <- names(.data)[idx]
  }

  idx
}


#' Finding variables including outliers
#'
#' @description
#' Find the numerical variable that contains outliers in the object
#' that inherits the data.frame or data.frame.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param index logical. When representing the information of a variable including
#' outliers, specify whether or not the variable is represented by an index.
#' Returns an index if TRUE or a variable names if FALSE.
#' @param rate logical. If TRUE, returns the percentage of outliers
#' in the individual variable.
#' @return Information on variables including outliers.
#' @seealso \code{\link{find_na}}, \code{\link{imputate_outlier}}.
#' @examples
#' \dontrun{
#' find_outliers(heartfailure)
#'
#' find_outliers(heartfailure, index = FALSE)
#'
#' find_outliers(heartfailure, rate = TRUE)
#'
#' ## using dplyr -------------------------------------
#' library(dplyr)
#'
#' # Perform simple data quality diagnosis of variables with outliers.
#' heartfailure %>%
#'   select(find_outliers(.)) %>%
#'   diagnose()
#' }
#' @importFrom purrr map_lgl map_dbl
#' @importFrom methods is
#' @export
#'
find_outliers <- function(.data, index = TRUE, rate = FALSE) {
  numeric_flag <- sapply(seq(.data),
    function(.x.) is(pull(.data, .x.))[1] == "numeric")

  if (rate) {
    idx <- .data %>%
      .[, numeric_flag] %>%
      map_dbl(function(x) length(boxplot.stats(x)$out) / length(x) * 100) %>%
      round(3)
  } else {
    idx <- .data %>%
      .[, numeric_flag] %>%
      map_lgl(function(x) length(boxplot.stats(x)$out) > 0) %>%
      which()
    
    idx <- which(numeric_flag)[idx]
    
    if (!index) idx <- names(.data)[idx]
  }

  idx
}


#' Finding skewed variables
#'
#' @description
#' Find the numerical variable that skewed variable
#' that inherits the data.frame or data.frame.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param index logical. When representing the information of a skewed variable,
#' specify whether or not the variable is represented by an index.
#' Returns an index if TRUE or a variable names if FALSE.
#' @param value logical. If TRUE, returns the skewness value
#' in the individual variable.
#' @param thres Returns a skewness threshold value that has an absolute skewness
#' greater than thres. The default is NULL to ignore the threshold.
#' but, If value = TRUE, default to 0.5.
#' @return Information on variables including skewness.
#' @seealso \code{\link{find_na}}, \code{\link{find_outliers}}.
#' @examples
#' \dontrun{
#' find_skewness(heartfailure)
#'
#' find_skewness(heartfailure, index = FALSE)
#'
#' find_skewness(heartfailure, thres = 0.1)
#'
#' find_skewness(heartfailure, value = TRUE)
#'
#' find_skewness(heartfailure, value = TRUE, thres = 0.1)
#'
#' ## using dplyr -------------------------------------
#' library(dplyr)
#'
#' # Perform simple data quality diagnosis of skewed variables
#' heartfailure %>%
#'   select(find_skewness(.)) %>%
#'   diagnose()
#' }
#' @importFrom purrr map_lgl map_dbl
#' @importFrom methods is
#' @export
find_skewness <- function(.data, index = TRUE, value = FALSE, thres = NULL) {
  numeric_flag <- sapply(seq(.data),
    function(.x.) is(pull(.data, .x.))[1] == "numeric")

  if (value) {
    idx <- .data %>%
      .[, numeric_flag] %>%
      map_dbl(skewness) %>%
      round(3)
    if (!is.null(thres)) idx <- idx[abs(idx) >= thres & !is.na(idx)]
  } else {
    if (is.null(thres)) thres <- 0.5

    idx <- .data %>%
      .[, numeric_flag] %>%
      map_lgl(function(x) abs(skewness(x)) >= thres) %>%
      which()

    idx <- which(numeric_flag)[idx]
    
    if (!index) idx <- names(.data)[idx]
  }

  idx
}


#' Skewness of the data
#'
#' @description
#' This function calculated skewness of given data.
#'
#' @param x a numeric vector.
#' @param na.rm logical. Determine whether to remove missing values and calculate them. 
#' The default is TRUE.
#' @return numeric. calculated skewness.
#' @seealso \code{\link{kurtosis}}, \code{\link{find_skewness}}.
#' @examples
#' set.seed(123)
#' skewness(rnorm(100))
#' @export
skewness <- function(x,  na.rm = TRUE) {
  if (na.rm) 
    x <- x[!is.na(x)]
  
  n <- length(x)
  
  (sum((x - mean(x)) ^ 3) / n) / (sum((x - mean(x)) ^ 2) / n) ^ (3 / 2)
}


#' Kurtosis of the data
#'
#' @description
#' This function calculated kurtosis of given data.
#'
#' @param x a numeric vector.
#' @param na.rm logical. Determine whether to remove missing values and calculate them. 
#' The default is TRUE.
#' @return numeric. calculated kurtosis
#' @seealso \code{\link{skewness}}.
#' @examples
#' set.seed(123)
#' kurtosis(rnorm(100))
#' @export
kurtosis <- function(x,  na.rm = FALSE) {
  if (na.rm) 
    x <- x[!is.na(x)]
  
  n <- length(x)
  
  n * sum((x - mean(x)) ^ 4) / (sum((x - mean(x)) ^ 2) ^ 2)
}


#' Finding Users Machine's OS
#'
#' @description
#' Get the operating system that users machines.
#'
#' @return OS names. "windows" or "osx" or "linux"
#'
#' @examples
#' get_os()
#'
#' @export
get_os <- function() {
  system_info <- Sys.info()

  if (!is.null(system_info)) {
    os <- system_info['sysname']

    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type

    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


#' Calculate the entropy
#'
#' @description
#' Calculate the Shannon's entropy.
#' 
#' @param x a numeric vector.
#'
#' @return numeric. entropy
#'
#' @examples
#' set.seed(123)
#' x <- sample(1:10, 20, replace = TRUE)
#' 
#' entropy(x)
#'
#' @export
entropy <- function(x) {
  x <- x / sum(x)
  -sum(ifelse(x > 0, x * log2(x), 0))
}


#' Finding percentile
#'
#' @description
#' Find the percentile of the value specified in numeric vector.
#'
#' @param x numeric. a numeric vector.
#' @param value numeric. a scalar to find percentile value from vector x.
#' @param from numeric. Start interval in logic to find percentile value. default to 0.
#' @param to numeric. End interval in logic to find percentile value. default to 1.
#' @param eps numeric. Threshold value for calculating the approximate value in recursive 
#' calling logic to find the percentile value. (epsilon). default to 1e-06.
#' @return list.
#' Components of list. is as follows.
#' \itemize{
#' \item percentile : numeric. Percentile position of value. It has a value between [0, 100].
#' \item is_outlier : logical. Whether value is an outlier.
#' }
#' @examples
#' \dontrun{
#' carat <- ggplot2::diamonds$carat
#' 
#' quantile(carat)
#' 
#' get_percentile(carat, value = 0.5)
#' get_percentile(carat, value = median(diamonds$carat))
#' get_percentile(carat, value = 1)
#' get_percentile(carat, value = 7)
#' }
#' @importFrom stats quantile
#' @export
get_percentile <- function(x, value, from = 0, to = 1, eps = 1e-06) {
  N <- 5
  coef <- 1.5
  
  breaks <- seq(from, to, length.out = N)
  percentile <- quantile(x, probs = breaks)
  
  iqr <- diff(percentile[c(2, 4)])
  outlier <- as.logical(value < (percentile[2L] - coef * iqr) | value > (percentile[4L] + coef * iqr))
  
  cutoff <- as.numeric(sub("%", "", names(percentile))) / 100
  
  for (i in seq(N)) {
    if (value >= percentile[i]) from <- max(from, cutoff[i])
    if (value <= percentile[i]) to <- min(to, cutoff[i])
  }
  
  if ((to - from) <= eps) {
    result <- mean(from, to) * 100
    return(list(percentile = result, is_outlier = outlier))
  } else {
    get_percentile(x, value, from = from, to = to)
  }
}


# for replace reshape2::melt()
#' @importFrom purrr map_dbl
#' @importFrom tibble is_tibble
get_melt <- function(x) {
  if (is.data.frame(x)) {
    if (tibble::is_tibble(x))
      x <- as.data.frame(x)
    
    df <- data.frame(
      Var1 = factor(rep(names(x), times = nrow(x)), levels = colnames(x)),
      Var2 = as.integer(rep(row.names(x), each = ncol(x))),
      value = numeric(nrow(x) * ncol(x))
    ) 
  } else if (is.matrix(x)) {
    df <- data.frame(
      Var1 = factor(rep(colnames(x), times = nrow(x)), levels = colnames(x)),
      Var2 = as.integer(rep(row.names(x), each = ncol(x))),
      value = numeric(nrow(x) * ncol(x))
    ) 
  }
  
  df$value <- seq(nrow(df)) %>% 
    purrr::map_dbl(function(i) x[df$Var2[i], df$Var1[i]])
  
  df
} 

# for replace DMwR::knnImputation()
#' @importFrom stats dist aggregate
imputation_knn <- function (data, k = 10) 
{
  weight_center <- function(x, weight) {
    if (is.numeric(x)) {
      sum(x * weight / sum(weight))
    } else {
      agg <- aggregate(weight, list(x), sum) 
      pos <- agg[, 2] %>% which.max()
      levels(agg[, 1])[pos]
    }  
  }
  
  n_row <- nrow(data)
  n_col <- ncol(data)
  
  idx_category <- find_class(data, "categorical")
  flag_category <- length(idx_category) > 0
  idx_numeric <- find_class(data, "numerical")
  
  dm <- data
  dm <- dm %>% 
    mutate_at(idx_numeric, scale)   
  
  if (flag_category) {
    dm <- dm %>% 
      mutate_at(idx_category, as.integer) 
  }
  
  mat_data <- as.matrix(dm)
  complete_not <- which(!complete.cases(mat_data))
  complete_yes <- setdiff(seq(n_row), complete_not)
  
  if (length(complete_not) == 0) 
    warning("Data did not include missing values.")
  
  mat_data_complete <- mat_data[complete_yes, ]
  
  if (nrow(mat_data_complete) < k) 
    stop("Not sufficient complete cases for computing neighbors.")
  
  for (i in complete_not) {
    idx_na <- which(is.na(mat_data[i, ]))
    idx_category_nona <- setdiff(idx_category, idx_na)
    
    dist <- scale(mat_data_complete, mat_data[i, ], FALSE)
    
    if (length(idx_category_nona)) {
      dist[, idx_category_nona] <- ifelse(dist[, idx_category_nona] > 0, 1, 
                                          dist[, idx_category_nona])
    }
    
    dist <- dist[, -idx_na]
    dist <- sqrt(drop(dist^2 %*% rep(1, ncol(dist))))
    idx_shortest <- order(dist)[seq(k)]
    
    for (j in idx_na) 
      data[i, j] <- weight_center(data[complete_yes, j][idx_shortest], 
                                  exp(-dist[idx_shortest]))
  }
  
  data
}


#' @import dplyr
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom purrr map_df
num_summarise <- function (.data, 
                           statistics = c("mean", "sd", "se_mean", "IQR", 
                                          "skewness", "kurtosis", "quantiles"), 
                           quantiles = c(0, 0.25, 0.5, 0.75, 1)) {
  if (missing(statistics)) 
    statistics <- c("mean", "sd", "quantiles", "IQR")
  
  stats <- c("mean", "sd", "se_mean", "IQR", "skewness", 
             "kurtosis", "quantiles")
  
  statistics <- base::intersect(statistics, stats)
  
  n <- function(x, ...) {
    apply(as.matrix(x), 2, function(x) sum(!is.na(x)))
  }
  
  na <- function(x, ...) {
    apply(as.matrix(x), 2, function(x) sum(is.na(x)))
  }
  
  mean <- function(x, ...) {
    colMeans(as.matrix(x), na.rm = TRUE)
  }
  
  sd <- function(x, ...) {
    apply(as.matrix(x), 2, stats::sd, na.rm = TRUE)
  }
  
  IQR <- function(x, ...) {
    apply(as.matrix(x), 2, stats::IQR, na.rm = TRUE)
  }
  
  se_mean <- function(x, ...) {
    x <- as.matrix(x)
    
    n <- colSums(!is.na(x))
    sd(x) / sqrt(n)
  }
  
  # modified e1071::skewness
  skewness <- function(x, ...) {
    if (is.vector(x))  {
      if (any(ina <- is.na(x))) {
        x <- x[!ina]
      }
      
      n <- length(x)
      if (n < 3) 
        return(NA)
      
      residual <- x - mean(x)
      y <- sqrt(n) * sum(residual ^ 3) / (sum(residual ^ 2) ^ (3 / 2))
      
      return(y * sqrt(n * (n - 1)) / (n - 2))
    }
    
    apply(x, 2, skewness)
  }
  
  # modified e1071::kurtosis  
  kurtosis <- function(x, ...) {
    if (is.vector(x)) {
      if (any(ina <- is.na(x))) {
        x <- x[!ina]
      }
      
      n <- length(x)
      if (n < 4) 
        return(NA)
      
      residual <- x - mean(x)
      r <- n * sum(residual ^ 4) / (sum(residual ^ 2) ^ 2)
      
      return(((n + 1) * (r - 3) + 6) * (n - 1)/((n - 2) * (n - 3)))
    }
    
    apply(x, 2, kurtosis)
  }
  
  .data <- as.data.frame(.data)
  
  if ("quantiles" %in% statistics & length(quantiles) >= 1) {
    df_quantiles <- .data %>% 
      apply(2, quantile, probs = quantiles, na.rm = TRUE) %>% 
      t() %>% 
      tibble::as_tibble()
    
    names(df_quantiles) <- paste0("p", quantiles * 100) %>% 
      ifelse(nchar(.) < 3, sub("p", "p0", .), .)
  } else {
    df_quantiles <- NULL
  }
  
  statistics <- c("n", "na", setdiff(statistics, "quantiles"))
  
  df_stats <- statistics %>% 
    sapply(
      function(x) {
        do.call(x, list(.data))
      }  
    ) 
  
  if (is.vector(df_stats)) 
    df_stats <- as.numeric(df_stats) %>% 
    t() %>% 
    as.data.frame()
  
  names(df_stats) <- statistics
  
  bind_cols(data.frame(variable = names(.data)), 
            df_stats %>% as.data.frame(), 
            df_quantiles) %>% 
    mutate_at(vars(matches("^n")), as.integer) %>% 
    tibble::as_tibble()
}