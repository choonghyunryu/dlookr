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
#' @description The correlate() compute Pearson's the correlation
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
#' \item coef_corr : Pearson's correlation coefficient
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
#' # Correlation coefficients of all numerical variables
#' correlate(heartfailure)
#'
#' # Select the variable to compute
#' correlate(heartfailure, creatinine, sodium)
#' correlate(heartfailure, -creatinine, -sodium)
#' correlate(heartfailure, "creatinine", "sodium")
#' correlate(heartfailure, 1)
#' # Non-parametric correlation coefficient by kendall method
#' correlate(heartfailure, creatinine, method = "kendall")
#'  
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(heartfailure, smoking, death_event)
#' correlate(gdata, "creatinine")
#' correlate(gdata)
#'
#' # Using pipes ---------------------------------
#' # Correlation coefficients of all numerical variables
#' heartfailure %>%
#'  correlate()
#' # Positive values select variables
#' heartfailure %>%
#'  correlate(creatinine, sodium)
#' # Negative values to drop variables
#' heartfailure %>%
#'  correlate(-creatinine, -sodium)
#' # Positions values select variables
#' heartfailure %>%
#'  correlate(1)
#' # Positions values select variables
#' heartfailure %>%
#'  correlate(-1, -3, -5, -7)
#' # Non-parametric correlation coefficient by spearman method
#' heartfailure %>%
#'  correlate(creatinine, sodium, method = "spearman")
#'  
#' # ---------------------------------------------
#' # Correlation coefficient
#' # that eliminates redundant combination of variables
#' heartfailure %>%
#'  correlate() %>%
#'  filter(as.integer(var1) > as.integer(var2))
#'
#' heartfailure %>%
#'  correlate(creatinine, sodium) %>%
#'  filter(as.integer(var1) > as.integer(var2))
#'
#' # Using pipes & dplyr -------------------------
#' # Compute the correlation coefficient of Sales variable by 'smoking'
#' # and 'death_event' variables. And extract only those with absolute
#' # value of correlation coefficient is greater than 0.2
#' heartfailure %>%
#'  group_by(smoking, death_event) %>%
#'  correlate(creatinine) %>%
#'  filter(abs(coef_corr) >= 0.2)
#'
#' # extract only those with 'smoking' variable level is "Yes",
#' # and compute the correlation coefficient of 'Sales' variable
#' # by 'hblood_pressure' and 'death_event' variables.
#' # And the correlation coefficient is negative and smaller than 0.5
#' heartfailure %>%
#'  filter(smoking == "Yes") %>%
#'  group_by(hblood_pressure, death_event) %>%
#'  correlate(creatinine) %>%
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
#' # Visualize correlation plot of all numerical variables
#' plot_correlate(heartfailure)
#'
#' # Select the variable to compute
#' plot_correlate(heartfailure, creatinine, sodium)
#' plot_correlate(heartfailure, -creatinine, -sodium)
#' plot_correlate(heartfailure, "creatinine", "sodium")
#' plot_correlate(heartfailure, 1)
#' plot_correlate(heartfailure, creatinine, sodium, method = "spearman")
#' 
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(heartfailure, smoking, death_event)
#' plot_correlate(gdata, "creatinine")
#' plot_correlate(gdata)
#'
#' # Using pipes ---------------------------------
#' # Visualize correlation plot of all numerical variables
#' heartfailure %>%
#'   plot_correlate()
#' # Positive values select variables
#' heartfailure %>%
#'   plot_correlate(creatinine, sodium)
#' # Negative values to drop variables
#' heartfailure %>%
#'   plot_correlate(-creatinine, -sodium)
#' # Positions values select variables
#' heartfailure %>%
#'   plot_correlate(1)
#' # Positions values select variables
#' heartfailure %>%
#'   plot_correlate(-1, -3, -5, -7)
#'
#' # Using pipes & dplyr -------------------------
#' # Visualize correlation plot of 'creatinine' variable by 'smoking'
#' # and 'death_event' variables.
#' heartfailure %>%
#' group_by(smoking, death_event) %>%
#' plot_correlate(creatinine)
#'
#' # Extract only those with 'smoking' variable level is "Yes",
#' # and visualize correlation plot of 'creatinine' variable by 'hblood_pressure'
#' # and 'death_event' variables.
#' heartfailure %>%
#'  filter(smoking == "Yes") %>%
#'  group_by(hblood_pressure, death_event) %>%
#'  plot_correlate(creatinine)
#'  
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


#' @import ggplot2
#' 
plot_correlate_impl <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)
  
  df %>% 
    correlate(method = method) %>% 
    filter(var2 %in% vars) %>% 
    ggplot(aes(var1, var2, fill = coef_corr, label = round(coef_corr, 2))) +
    geom_tile(col = "black") + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         limits = c(-1, 1)) +
    geom_text() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "Correlation\nCoefficient") + 
    coord_equal() +
    theme_typographic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1),
          panel.grid.major = element_blank())
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


#' @import ggplot2
#' @import dplyr
#' @importFrom utils packageVersion
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

      df_corr <- df[idx, ] %>% 
        correlate(method = method) %>% 
        filter(var2 %in% vars)
        
      if (NROW(df_corr) > 0) {
        p <- df_corr %>%   
          ggplot(aes(var1, var2, fill = coef_corr, label = round(coef_corr, 2))) +
          geom_tile(col = "black") + 
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                               limits = c(-1, 1)) +
          geom_text() +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_discrete(expand = c(0, 0)) +
          labs(title = label, 
               fill = "Correlation\nCoefficient") + 
          coord_equal() +
          theme_typographic() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(angle = 40, hjust = 1),
                panel.grid.major = element_blank())
        
        print(p)        
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
  
  invisible(lapply(idx, plot_correlate, df, vars))
}

