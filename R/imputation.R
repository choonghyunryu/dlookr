#' Impute Missing Values
#'
#' @description
#' Missing values are imputed with some representative values and
#' statistical methods.
#'
#' @details
#' imputate_na() creates an imputation class.
#' The `imputation` class includes missing value position, imputed value,
#' and method of missing value imputation, etc.
#' The `imputation` class compares the imputed value with the original value
#' to help determine whether the imputed value is used in the analysis.
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param xvar variable name to replace missing value.
#' @param yvar target variable.
#' @param method method of missing values imputation.
#' @param seed integer. the random seed used in mice. only used "mice" method.
#' @param print_flag logical. If TRUE, mice will print running log on console.
#' Use print_flag=FALSE for silent computation. Used only when method is "mice".
#' @param no_attrs logical. If TRUE, return numerical variable or categorical variable. 
#' else If FALSE, imputation class.
#' @return An object of imputation class. or numerical variable or categorical variable. 
#' if no_attrs is FALSE then return imputation class, else no_attrs is TRUE then return
#' numerical vector or factor.
#' Attributes of imputation class is as follows.
#' \itemize{
#' \item var_type : the data type of predictor to replace missing value.
#' \item method : method of missing value imputation.
#' \itemize{
#'   \item predictor is numerical variable.
#'   \itemize{
#'     \item "mean" : arithmetic mean.
#'     \item "median" : median.
#'     \item "mode" : mode.
#'     \item "knn" : K-nearest neighbors.
#'     \item "rpart" : Recursive Partitioning and Regression Trees.
#'     \item "mice" : Multivariate Imputation by Chained Equations.
#'   }
#'   \item predictor is categorical variable.
#'   \itemize{
#'     \item "mode" : mode.
#'     \item "rpart" : Recursive Partitioning and Regression Trees.
#'     \item "mice" : Multivariate Imputation by Chained Equations.
#'   }
#' }
#' \item na_pos : position of missing value in predictor.
#' \item seed : the random seed used in mice. only used "mice" method.
#' \item type : "missing values". type of imputation.
#' \item message : a message tells you if the result was successful.
#' \item success : Whether the imputation was successful.
#' }
#' @seealso \code{\link{imputate_outlier}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' 
#' # Replace the missing value of the platelets variable with median
#' imputate_na(heartfailure2, platelets, method = "median")
#' 
#' # Replace the missing value of the platelets variable with rpart
#' # The target variable is death_event.
#' imputate_na(heartfailure2, platelets, death_event, method = "rpart")
#' 
#' # Replace the missing value of the smoking variable with mode
#' imputate_na(heartfailure2, smoking, method = "mode")
#' 
#' # Replace the missing value of the smoking variable with mice
#' # The target variable is death_event.
#' imputate_na(heartfailure2, smoking, death_event, method = "mice")
#' 
#' ## using dplyr -------------------------------------
#' library(dplyr)
#' 
#' # The mean before and after the imputation of the platelets variable
#' heartfailure2 %>%
#'   mutate(platelets_imp = imputate_na(heartfailure2, platelets, death_event, 
#'                                      method = "knn", no_attrs = TRUE)) %>%
#'   group_by(death_event) %>%
#'   summarise(orig = mean(platelets, na.rm = TRUE),
#'             imputation = mean(platelets_imp))
#' 
#' # If the variable of interest is a numerical variable
#' platelets <- imputate_na(heartfailure2, platelets, death_event, method = "rpart")
#' platelets
#' summary(platelets)
#' 
#' # plot(platelets)
#' 
#' # If the variable of interest is a categorical variable
#' smoking <- imputate_na(heartfailure2, smoking, death_event, method = "mice")
#' smoking
#' summary(smoking)
#' 
#' # plot(smoking)
#' }
#' 
#' @export
#'
imputate_na <- function(.data, xvar, yvar, method, seed, print_flag, no_attrs) {
  UseMethod("imputate_na")
}

#' @method imputate_na data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang enquo
#' @export
imputate_na.data.frame <- function(.data, xvar, yvar = NULL,
  method = c("mean", "median", "mode", "rpart", "knn", "mice"), seed = NULL,
  print_flag = TRUE, no_attrs = FALSE) {
  tryCatch(vars <- tidyselect::vars_select(names(.data), !! rlang::enquo(xvar)),
    error = function(e) {
      pram <- as.character(substitute(xvar))
      stop(sprintf("Column %s is unknown", pram))
    }, finally = NULL)

  tryCatch(target <- tidyselect::vars_select(names(.data), !! rlang::enquo(yvar)),
    error = function(e) {
      pram <- as.character(substitute(yvar))
      stop(sprintf("Column %s is unknown", pram))
    }, finally = NULL)

  method <- match.arg(method)

  imputate_na_impl(.data, vars, target, method, seed, print_flag, no_attrs)
}


#' @import tibble
#' @import dplyr
#' @importFrom stats predict
#' @importFrom methods is
imputate_na_impl <- function(df, xvar, yvar, method, seed = NULL, 
                             print_flag = TRUE, no_attrs = FALSE) {
  type <- ""

  if (is(pull(df, xvar))[1] %in% c("integer", "numeric")) {
    type <- "numerical"
  } else if (is(pull(df, xvar))[1] %in% c("factor", "ordered")) {
    if (method %in% c("mean", "median", "knn")) {
      stop(sprintf("Categorical variable(%s) not support %s method",
        xvar, method))
    }

    type <- "categorical"
  }

  data <- pull(df, xvar)
  na_pos <- which(is.na(data))

  na_flag <- length(na_pos) > 0
  if (!na_flag) {
    warning(sprintf("There are no missing values in %s.", xvar))
  }

  get_mean <- function() {
    ifelse(is.na(data), mean(data, na.rm = TRUE), data)
  }

  get_median <- function() {
    ifelse(is.na(data), median(data, na.rm = TRUE), data)
  }

  get_mode <- function() {
    tab <- table(data)

    if (length(tab) == 0) {
      return(rep(NA, length(data)))
    }
    
    if (type == "numerical")
      mode_value <- as.numeric(names(tab)[which.max(tab)])
    else if (type == "categorical") {
      mode_value <- levels(data)[which.max(tab)]
    }

    data[is.na(data)] <- mode_value
    data
  }

  get_knn <- function(x, y) {
    complete_order <- names(sort(apply(df[, setdiff(names(df), y)], 2, 
                                       function(x) sum(complete.cases(x)) / length(x))))
  
    complete_cnt <- length(which(complete.cases(df)))
    
    while(complete_cnt <= 10) {
      complete_order <- complete_order[-1]
      complete_cnt <- length(which(complete.cases(df[, complete_order])))
    }
    
    if (!x %in% complete_order) {
      return(rep(NA, length(data)))
    }

    impute <- imputation_knn(df[, complete_order])

    pred <- impute[, x]

    ifelse(is.na(data), pred, data)
  }

  get_rpart <- function(x, y) {
    if (type == "numerical") {
      method <- "anova"
      pred_type <- "vector"
    } else if (type == "categorical") {
      method <- "class"
      pred_type <- "class"
    }

    complete_flag <- apply(df, 2, function(x) sum(complete.cases(x)) != 0)
    complete_flag <- names(complete_flag[complete_flag])
    
    if (!x %in% complete_flag) {
      return(rep(NA, length(data)))
    }
    
    if (requireNamespace("rpart", quietly = TRUE)) {
      model <- rpart::rpart(sprintf("`%s` ~ .", x),
                            data = df[!is.na(pull(df, x)), setdiff(intersect(names(df), complete_flag), y)],
                            method = method, na.action = na.omit)
    } else {
      stop("Package 'rpart' needed for this function to work. Please install it.", 
           call. = FALSE)
    }

    pred <- predict(model, df[is.na(pull(df, x)), !names(df) %in% y],
      type = pred_type)

    data[is.na(data)] <- pred
    data
  }

  get_mice <- function(x, y, seed = NULL, print_flag = TRUE) {
    if (is.null(seed))
      seed <<- sample(seq(1e5), size = 1)

    if (!na_flag) {
      data <- pull(df, x)
    } else {
      suppressWarnings(RNGversion("3.5.0"))
      set.seed(seed = seed)
      
      if (requireNamespace("mice", quietly = TRUE)) {
        model <- mice::mice(df[, !names(df) %in% y], method = "rf", printFlag = print_flag)
      } else {
        stop("Package 'mice' needed for this function to work. Please install it.", 
             call. = FALSE)
      }

      if (all(is.na(model$imp[[x]]))) {
        return(rep(NA, length(data)))
      }
      
      if (type == "numerical") {
        pred <- apply(model$imp[[x]], 1, mean)

        data[is.na(data)] <- pred
      } else if (type == "categorical") {
        pred <- apply(model$imp[[x]], 1,
          function(x) unique(x)[which.max(table(x))])

        data[is.na(data)] <- pred
      }
    }

    data
  }

  if (method == "mean")
    result <- get_mean()
  else if (method == "median")
    result <- get_median()
  else if (method == "mode")
    result <- get_mode()
  else if (method == "knn")
    result <- get_knn(xvar, yvar)
  else if (method == "rpart")
    result <- get_rpart(xvar, yvar)
  else if (method == "mice")
    result <- get_mice(xvar, yvar, seed, print_flag)

  if (!no_attrs) {
    attr(result, "var_type") <- type
    attr(result, "method") <- method
    attr(result, "na_pos") <- na_pos
    attr(result, "seed") <- seed
    attr(result, "type") <- "missing values"
    if (all(is.na(result))) {
      msg <- "All values returned as NA. The data is not good enough for a imputation."
      warning(msg)
      
      attr(result, "message") <- msg
      attr(result, "success") <- FALSE
    } else {
      attr(result, "message") <- "complete imputation"
      attr(result, "success") <- TRUE
    }
    
    class(result) <- append("imputation", class(result))    
  }

  result
}


#' Impute Outliers
#'
#' @description
#' Outliers are imputed with some representative values and statistical methods.
#'
#' @details
#' imputate_outlier() creates an imputation class.
#' The `imputation` class includes missing value position, imputed value,
#' and method of missing value imputation, etc.
#' The `imputation` class compares the imputed value with the original value
#' to help determine whether the imputed value is used in the analysis.
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param xvar variable name to replace missing value.
#' @param method method of missing values imputation.
#' @param no_attrs logical. If TRUE, return numerical variable or categorical variable. 
#' else If FALSE, imputation class. 
#' @return An object of imputation class. or numerical variable. 
#' if no_attrs is FALSE then return imputation class, else no_attrs is TRUE then return
#' numerical vector.
#' Attributes of imputation class is as follows.
#' \itemize{
#' \item method : method of missing value imputation.
#' \itemize{
#'   \item predictor is numerical variable
#'   \itemize{
#'     \item "mean" : arithmetic mean
#'     \item "median" : median
#'     \item "mode" : mode
#'     \item "capping" : Impute the upper outliers with 95 percentile,
#'     and Impute the bottom outliers with 5 percentile.
#'   }
#' }
#' \item outlier_pos : position of outliers in predictor.
#' \item outliers : outliers. outliers corresponding to outlier_pos.
#' \item type : "outliers". type of imputation.
#' }
#' @seealso \code{\link{imputate_na}}.
#' @examples
#' # Replace the outliers of the sodium variable with median.
#' imputate_outlier(heartfailure, sodium, method = "median")
#' 
#' # Replace the outliers of the sodium variable with capping.
#' imputate_outlier(heartfailure, sodium, method = "capping")
#' 
#' ## using dplyr -------------------------------------
#' library(dplyr)
#' 
#' # The mean before and after the imputation of the sodium variable
#' heartfailure %>%
#'   mutate(sodium_imp = imputate_outlier(heartfailure, sodium, 
#'                                       method = "capping", no_attrs = TRUE)) %>%
#'   group_by(death_event) %>%
#'   summarise(orig = mean(sodium, na.rm = TRUE),
#'             imputation = mean(sodium_imp, na.rm = TRUE))
#'             
#' # If the variable of interest is a numerical variables
#' sodium <- imputate_outlier(heartfailure, sodium)
#' sodium
#' summary(sodium)
#' 
#' # plot(sodium)
#' @export
imputate_outlier <- function(.data, xvar, method, no_attrs) {
  UseMethod("imputate_outlier")
}

#' @method imputate_outlier data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang enquo
#' @export
imputate_outlier.data.frame <- function(.data, xvar,
  method = c("capping", "mean", "median", "mode"), no_attrs = FALSE) {
  tryCatch(vars <- tidyselect::vars_select(names(.data), !! rlang::enquo(xvar)),
    error = function(e) {
      pram <- as.character(substitute(xvar))
      stop(sprintf("Column %s is unknown", pram))
    }, finally = NULL)

  method <- match.arg(method)

  imputate_outlier_impl(.data, vars, method, no_attrs)
}

#' @import dplyr
#' @importFrom methods is
#' @importFrom grDevices boxplot.stats
imputate_outlier_impl <- function(df, xvar, method, no_attrs = FALSE) {
  if (!is(pull(df, xvar))[1] %in% c("integer", "numeric")) {
    stop(sprintf("Categorical variable(%s) not support imputate_outlier()",
      xvar))
  }

  data <- pull(df, xvar)
  outliers <- boxplot.stats(data)$out
  outlier_pos <- which(data %in% outliers)
  outliers <- data[outlier_pos]

  outlier_flag <- length(outlier_pos) > 0

  get_mean <- function(x) {
    data[outlier_pos] <- mean(data, na.rm = TRUE)
    data
  }

  get_median <- function() {
    data[outlier_pos] <- median(data, na.rm = TRUE)
    data
  }

  get_mode <- function() {
    tab <- table(data)

    mode_value <- as.numeric(names(tab)[which.max(tab)])

    data[outlier_pos] <- mode_value
    data
  }

  get_capping <- function() {
    hinges <- quantile(data, probs = c(0.25, 0.75), na.rm = TRUE)
    caps <- quantile(data, probs = c(0.05, 0.95), na.rm = TRUE)

    whisker <- 1.5 * diff(hinges)

    data[data < (hinges[1] - whisker)] <- caps[1]
    data[data > (hinges[2] + whisker)] <- caps[2]
    data
  }

  if (method == "mean")
    result <- get_mean()
  else if (method == "median")
    result <- get_median()
  else if (method == "mode")
    result <- get_mode()
  else if (method == "capping")
    result <- get_capping()

  if (!no_attrs) {
    attr(result, "method") <- method
    attr(result, "var_type") <- "numerical"
    attr(result, "outlier_pos") <- outlier_pos
    attr(result, "outliers") <- outliers
    attr(result, "type") <- "outliers"
    
    if (!outlier_flag) {
      msg <- sprintf("There are no outliers in %s.", xvar)
      warning(msg)
      
      attr(result, "message") <- msg
      attr(result, "success") <- FALSE
    } else {
      attr(result, "message") <- "complete imputation"
      attr(result, "success") <- TRUE
    }
    
    class(result) <- append("imputation", class(result))
  }

  result
}

#' Summarizing imputation information
#'
#' @description print and summary method for "imputation" class.
#' @param object an object of class "imputation", usually, a result of a call to imputate_na() or
#' imputate_outlier().
#' @param ... further arguments passed to or from other methods.
#' @details
#' summary.imputation() tries to be smart about formatting two kinds of imputation.
#'
#' @seealso \code{\link{imputate_na}}, \code{\link{imputate_outlier}}, \code{\link{summary.imputation}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#'
#' # Impute missing values -----------------------------
#' # If the variable of interest is a numerical variables
#' platelets <- imputate_na(heartfailure2, platelets, death_event, method = "rpart")
#' platelets
#' summary(platelets)
#' plot(platelets)
#'
#' # If the variable of interest is a categorical variables
#' smoking <- imputate_na(heartfailure2, smoking, death_event, method = "mice")
#' smoking
#' summary(smoking)
#' 
#' # plot(smoking)
#'
#' # Impute outliers ----------------------------------
#' # If the variable of interest is a numerical variable
#' platelets <- imputate_outlier(heartfailure2, platelets, method = "capping")
#' platelets
#' summary(platelets)
#' 
#' # plot(platelets)
#' }
#' @method summary imputation
#' @importFrom tidyr gather
#' @export
summary.imputation <- function(object, ...) {
  success <- attr(object, "success")
  
  if (!success) {
    message("imputation object isn't success.")
    return()
  }
  
  type <- attr(object, "type")
  method <- attr(object, "method")
  var_type <- attr(object, "var_type")

  original <- object

  if (type == "missing values") {
    na_pos <- attr(object, "na_pos")
    seed <- attr(object, "seed")

    original[na_pos] <- NA
  } else if (type == "outliers") {
    outlier_pos <- attr(object, "outlier_pos")
    outliers <- attr(object, "outliers")

    original[outlier_pos] <- outliers
  }

  if (var_type == "numerical") {
    original <- as.numeric(original)
    object <- as.numeric(object)
  } else if (var_type == "categorical") {
    original <- factor(original)
    object <- factor(object)
  }

  dframe <- data.frame(original = original,
    imputation = object) %>%
    tidyr::gather()

  if (var_type == "numerical") {
    smmry <- dframe %>%
      group_by(key) %>%
      describe("value") %>%
      select(which(!names(.) %in% c("variable", "key"))) %>% 
      t

    smmry <- smmry[, 2:1]
    colnames(smmry) <- c("Original", "Imputation")
  } else if (var_type == "categorical") {
    tab_freq <- xtabs(~ value + key, dframe, addNA = TRUE)
    tab_relat <- round(prop.table(tab_freq, 2) * 100, 2)

    smmry <- cbind(tab_freq, tab_relat)
    smmry <- smmry[, c(2, 1, 4, 3)]
    colnames(smmry) <- c("original", "imputation",
      "original_percent", "imputation_percent")
  }

  if (method %in% c("mean", "median", "mode", "capping")) {
    cat(sprintf("Impute %s with %s\n\n", type, method))
  } else if (method %in% c("knn", "rpart", "mice")) {
    if (method == "knn") {
      met <- "K-Nearest Neighbors"
      met <- sprintf("%s\n - method : knn", met)
    } else if (method == "rpart") {
      met <- "Recursive Partitioning and Regression Trees"
      met <- sprintf("%s\n - method : rpart", met)
    } else if (method == "mice") {
      met <- "Multivariate Imputation by Chained Equations"
      met <- sprintf("%s\n - method : mice", met)
      met <- sprintf("%s\n - random seed : %s", met, seed)
    }
    cat(sprintf("* Impute %s based on %s\n\n", type, met))
  }

  cat("* Information of Imputation (before vs after)\n")
  print(smmry)

  invisible(smmry)
}


#' Visualize Information for an "imputation" Object
#'
#' @description
#' Visualize two kinds of plot by attribute of `imputation` class.
#' The imputation of a numerical variable is a density plot,
#' and the imputation of a categorical variable is a bar plot.
#'
#' @param x an object of class "imputation", usually, a result of a call to imputate_na()
#' or imputate_outlier().
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. 
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' only applies when the model argument is TRUE, and is used for ... of the plot.lm() function.
#' @seealso \code{\link{imputate_na}}, \code{\link{imputate_outlier}}, \code{\link{summary.imputation}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#'
#' # Impute missing values -----------------------------
#' # If the variable of interest is a numerical variables
#' platelets <- imputate_na(heartfailure2, platelets, death_event, method = "rpart")
#' platelets
#' summary(platelets)
#' 
#' plot(platelets)
#'
#' # If the variable of interest is a categorical variables
#' smoking <- imputate_na(heartfailure2, smoking, death_event, method = "mice")
#' smoking
#' summary(smoking)
#' 
#' plot(smoking)
#'
#' # Impute outliers ----------------------------------
#' # If the variable of interest is a numerical variable
#' platelets <- imputate_outlier(heartfailure2, platelets, method = "capping")
#' platelets
#' summary(platelets)
#' 
#' plot(platelets)
#' }
#' @method plot imputation
#' @import ggplot2
#' @import hrbrthemes
#' @importFrom tidyr gather
#' @export
plot.imputation <- function(x, typographic = TRUE, base_family = NULL, ...) {
  type <- attr(x, "type")
  var_type <- attr(x, "var_type")
  method <- attr(x, "method")
  
  original <- x
  
  if (type == "missing values") {
    na_pos <- attr(x, "na_pos")
    seed <- attr(x, "seed")
    
    original[na_pos] <- NA
  } else if (type == "outliers") {
    outlier_pos <- attr(x, "outlier_pos")
    outliers <- attr(x, "outliers")
    
    original[outlier_pos] <- outliers
  }
  
  if (method == "mice") {
    method <- sprintf("%s (seed = %s)", method, seed)
  }
  
  if (var_type == "numerical") {
    suppressWarnings({p <- data.frame(original = original, imputation = x) %>%
      tidyr::gather() %>%
      ggplot(aes(x = value, color = key)) +
      geom_density(na.rm = TRUE) +
      labs(title = sprintf("imputation method : %s", method))}) +
      theme_grey(base_family = base_family)
    
    if (typographic) {
      p <- p +
        theme_typographic(base_family) +
        scale_color_ipsum() +
        theme(
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)
        )
    }
    
    suppressWarnings(p)
  } else if (var_type == "categorical") {
    suppressWarnings({p <- data.frame(original = original, imputation = x) %>%
      tidyr::gather() %>%
      ggplot(aes(x = value, fill = key)) +
      geom_bar(position = "dodge") +
      labs(title = sprintf("imputation method : %s", method),
            x = "level", y = "frequency")}) +
      theme_grey(base_family = base_family)
    
    if (typographic) {
      p <- p +
        theme_typographic(base_family) +
        scale_fill_ipsum() +
        theme(
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)
        )  
    }
    
    suppressWarnings(p)
  }
}

