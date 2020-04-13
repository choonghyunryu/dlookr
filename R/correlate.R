#' @rdname correlate.data.frame
#' @export
correlate <- function(.data, ...) {
  UseMethod("correlate", .data)
}


#' @rdname plot_correlate.data.frame
#' @export
plot_correlate <- function(.data, ...) {
  UseMethod("plot_correlate", .data)
}


#' Compute the correlation coefficient between two numerical data
#'
#' @description The correlate() compute pearson's the correlation
#' coefficient of the numerical data.
#'
#' @details This function is useful when used with the group_by() function of the dplyr package.
#' If you want to compute by level of the categorical data you are interested in,
#' rather than the whole observation, you can use \code{\link{grouped_df}} as the group_by() function.
#' This function is computed stats::cor() function by use = "pairwise.complete.obs" option.
#'
#' @section Correlation coefficient information:
#' The information derived from the numerical data compute is as follows.
#'
#' \itemize{
#' \item var1 : names of numerical variable
#' \item var2 : name of the corresponding numeric variable
#' \item coef_corr : pearson's correlation coefficient
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param method a character string indicating which correlation coefficient (or covariance) is 
#' to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, correlate() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @seealso \code{\link{cor}}, \code{\link{correlate.tbl_dbi}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Correlation coefficients of all numerical variables
#' correlate(carseats)
#'
#' # Select the variable to compute
#' correlate(carseats, Sales, Price)
#' correlate(carseats, -Sales, -Price)
#' correlate(carseats, "Sales", "Price")
#' correlate(carseats, 1)
#' # Non-parametric correlation coefficient by kendall method
#' correlate(carseats, Sales, method = "kendall")
#'  
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(carseats, ShelveLoc, US)
#' correlate(gdata, "Sales")
#' correlate(gdata)
#'
#' # Using pipes ---------------------------------
#' # Correlation coefficients of all numerical variables
#' carseats %>%
#'  correlate()
#' # Positive values select variables
#' carseats %>%
#'  correlate(Sales, Price)
#' # Negative values to drop variables
#' carseats %>%
#'  correlate(-Sales, -Price)
#' # Positions values select variables
#' carseats %>%
#'  correlate(1)
#' # Positions values select variables
#' carseats %>%
#'  correlate(-1, -2, -3, -5, -6)
#' # Non-parametric correlation coefficient by spearman method
#' carseats %>%
#'  correlate(Sales, Price, method = "spearman")
#'  
#' # ---------------------------------------------
#' # Correlation coefficient
#' # that eliminates redundant combination of variables
#' carseats %>%
#'  correlate() %>%
#'  filter(as.integer(var1) > as.integer(var2))
#'
#' carseats %>%
#'  correlate(Sales, Price) %>%
#'  filter(as.integer(var1) > as.integer(var2))
#'
#' # Using pipes & dplyr -------------------------
#' # Compute the correlation coefficient of Sales variable by 'ShelveLoc'
#' # and 'US' variables. And extract only those with absolute
#' # value of correlation coefficient is greater than 0.5
#' carseats %>%
#'  group_by(ShelveLoc, US) %>%
#'  correlate(Sales) %>%
#'  filter(abs(coef_corr) >= 0.5)
#'
#' # extract only those with 'ShelveLoc' variable level is "Good",
#' # and compute the correlation coefficient of 'Sales' variable
#' # by 'Urban' and 'US' variables.
#' # And the correlation coefficient is negative and smaller than 0.5
#' carseats %>%
#'  filter(ShelveLoc == "Good") %>%
#'  group_by(Urban, US) %>%
#'  correlate(Sales) %>%
#'  filter(coef_corr < 0) %>%
#'  filter(abs(coef_corr) > 0.5)
#' @method correlate data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
correlate.data.frame <- function(.data, ..., method = c("pearson", "kendall", 
                                                   "spearman")) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  correlate_impl(.data, vars, method)
}


#' @import tibble
#' @importFrom stats cor
correlate_impl <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)

  idx_numeric <- find_class(df, type = "numerical")

  suppressWarnings(M <- cor(df[, names(df)[idx_numeric]],
    use = "pairwise.complete.obs", method = method))

  m <- as.vector(M)
  tab <- as_tibble(expand.grid(var1 = row.names(M),
    var2 = row.names(M)))
  add_column(tab, coef_corr = m) %>%
    filter(var1 != var2) %>%
    filter(var1 %in% vars)
}


#' @method correlate grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
correlate.grouped_df <- function(.data, ..., method = c("pearson", "kendall", 
                                                   "spearman")) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  correlate_group_impl(.data, vars, method)
}

#' @importFrom tidyr unnest
#' @importFrom stats cor
#' @import tibble
correlate_group_impl <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)

  idx_numeric <- find_class(df, type = "numerical")

  call_corr <- function(pos, df, vars) {
    
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      idx <- (attr(df, "groups") %>% 
        select(tidyselect::matches("\\.rows")) %>% 
        pull)[[pos]]
      
      label <- data.frame((attr(df, "groups") %>% 
                 select(-tidyselect::matches("\\.rows")))[pos, ])
      
    } else {
      idx <- attr(df, "indices")[[pos]] + 1
      
      label <- data.frame(attr(df, "labels")[pos, ])
    }  
    
    suppressWarnings(M <- cor(df[idx, names(df)[idx_numeric]],
      use = "pairwise.complete.obs", method = method))
    m <- as.vector(M)
    
    tab <- expand.grid(var1 = row.names(M),
      var2 = row.names(M))
    tab <- data.frame(tab, coef_corr = m) %>%
      filter(var1 != var2) %>%
      filter(var1 %in% vars)
    
    corrs <- merge(label, tab)
    
    if (utils::packageVersion("dplyr") < "0.8.0")
      names(corrs)[seq(attr(df, "vars"))] <- attr(df, "vars")
    
    corrs
  }

  if (utils::packageVersion("dplyr") >= "0.8.0") {
    cnt <- nrow(attr(df, "groups")) 
    
    idx <- seq(cnt)
    idx <- idx[sapply(pull(attr(df, "groups")[, ".rows"]), length) > 0]
  } else {
    cnt <- nrow(attr(df, "labels"))
    
    idx <- seq(cnt)
  } 

  suppressWarnings(statistic <- lapply(idx, call_corr, df, vars))

  suppressWarnings(
    tibble(statistic) %>%
      tidyr::unnest()
  )  
}


#' Visualize correlation plot of numerical data
#'
#' @description The plot_correlate() visualize correlation plot
#' for find relationship between two numerical variables.
#'
#' @details The scope of the visualization is the provide a correlation information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param method a character string indicating which correlation coefficient (or covariance) is 
#' to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_correlate() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @seealso \code{\link{plot_correlate.tbl_dbi}}, \code{\link{plot_outlier.data.frame}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Visualize correlation plot of all numerical variables
#' plot_correlate(carseats)
#'
#' # Select the variable to compute
#' plot_correlate(carseats, Sales, Price)
#' plot_correlate(carseats, -Sales, -Price)
#' plot_correlate(carseats, "Sales", "Price")
#' plot_correlate(carseats, 1)
#' plot_correlate(carseats, Sales, Price, method = "spearman")
#' 
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(carseats, ShelveLoc, US)
#' plot_correlate(gdata, "Sales")
#' plot_correlate(gdata)
#'
#' # Using pipes ---------------------------------
#' # Visualize correlation plot of all numerical variables
#' carseats %>%
#'   plot_correlate()
#' # Positive values select variables
#' carseats %>%
#'   plot_correlate(Sales, Price)
#' # Negative values to drop variables
#' carseats %>%
#'   plot_correlate(-Sales, -Price)
#' # Positions values select variables
#' carseats %>%
#'   plot_correlate(1)
#' # Positions values select variables
#' carseats %>%
#'   plot_correlate(-1, -2, -3, -5, -6)
#'
#' # Using pipes & dplyr -------------------------
#' # Visualize correlation plot of 'Sales' variable by 'ShelveLoc'
#' # and 'US' variables.
#' carseats %>%
#' group_by(ShelveLoc, US) %>%
#' plot_correlate(Sales)
#'
#' # Extract only those with 'ShelveLoc' variable level is "Good",
#' # and visualize correlation plot of 'Sales' variable by 'Urban'
#' # and 'US' variables.
#' carseats %>%
#'  filter(ShelveLoc == "Good") %>%
#'  group_by(Urban, US) %>%
#'  plot_correlate(Sales)
#' @method plot_correlate data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
plot_correlate.data.frame <- function(.data, ..., method = c("pearson", "kendall", 
                                                             "spearman")) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  plot_correlate_impl(.data, vars, method)
}


#' @importFrom corrplot corrplot
#' @importFrom stats cor
plot_correlate_impl <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)

  idx_numeric <- find_class(df, type = "numerical")

  M <- cor(df[, names(df)[idx_numeric]],
    use = "pairwise.complete.obs", method = method)

  M2 <- M[row.names(M) %in% vars, ]

  if (!is.matrix(M2)) {
    M2 <- t(as.matrix(M2))
    row.names(M2) <- vars
  }

  corrplot::corrplot(M2, method = "ellipse", diag = FALSE,
    tl.srt = 45, type = "upper", mar = c(0, 0, 1, 0))
}


#' @method plot_correlate grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos warn
#' @export
plot_correlate.grouped_df <- function(.data, ..., method = c("pearson", "kendall", 
                                                             "spearman")) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  plot_correlate_group_impl(.data, vars, method)
}


#' @importFrom stats cor
plot_correlate_group_impl <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)

  idx_numeric <- find_class(df, type = "numerical")

  plot_correlate <- function(pos, df, var) {
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      cnt <- length(pull(attr(df, "groups")[, ".rows"])[[pos]])
      #cnt <- cnt[cnt > 0]
    } else {
      cnt <- attr(df, "group_sizes")[pos]      
    }  
    
    if (cnt <= 5) {
      if (utils::packageVersion("dplyr") >= "0.8.0") {
        vars <- names(attr(df, "groups") %>% select(-tidyselect::matches("\\.rows")))
        values <- sapply((attr(df, "groups") %>% select(-tidyselect::matches("\\.rows"))), as.character)[pos, ]
      } else {
        vars <- names(attr(df, "labels")[pos, ])
        values <- attr(df, "labels")[pos, ]
      }  

      rlang::warn(sprintf("Passed a group with no more than five observations.\n(%s)",
        paste(vars, "==", values, collapse = " and ")))
    } else {
      if (utils::packageVersion("dplyr") >= "0.8.0") {
        idx <- (attr(df, "groups") %>% select(tidyselect::matches("\\.rows")) %>% pull)[[pos]]
        
        values <- sapply((attr(df, "groups") %>% select(-tidyselect::matches("\\.rows"))), as.character)[pos, ]
        label <- paste(names(attr(df, "groups") %>% select(-tidyselect::matches("\\.rows"))), "==", values, collapse = ",")
      } else {
        idx <- attr(df, "indices")[[pos]] + 1
        
        label <- sapply(attr(df, "labels")[pos, ], as.character)
        label <- paste(names(attr(df, "labels")), "==", label, collapse = ",")
      }  

      M <- cor(df[idx, names(df)[idx_numeric]],
        use = "pairwise.complete.obs", method = method)

      M2 <- M[row.names(M) %in% vars, ]

      if (any(complete.cases(M2))) {
        if (!is.matrix(M2)) {
          M2 <- t(as.matrix(M2))
          row.names(M2) <- vars
        }
        
        corrplot::corrplot(M2, method = "ellipse", diag = FALSE, tl.srt = 45, 
                           type = "upper", mar = c(0, 0, 1, 0), title = label)
      } 
    }
  }

  if (utils::packageVersion("dplyr") >= "0.8.0") {
    cnt <- nrow(attr(df, "groups")) 
    
    idx <- seq(cnt)
    idx <- idx[sapply(pull(attr(df, "groups")[, ".rows"]), length) > 0]
  } else {
    cnt <- nrow(attr(df, "labels"))
    
    idx <- seq(cnt)
  } 
  
  tmp <- lapply(idx, plot_correlate, df, vars)
}

