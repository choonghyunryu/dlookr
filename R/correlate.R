#' @rdname correlate.data.frame
#' @export
correlate <- function(.data, ...) {
  UseMethod("correlate", .data)
}


#' @rdname dlookr-deprecated
#' @export
plot_correlate <- function(.data, ...) {
  .Deprecated("plot.correlate")
  
  UseMethod("plot_correlate", .data)
}


#' Compute the correlation coefficient between two variable
#'
#' @description The correlate() compute the correlation coefficient for numerical or categorical data.
#'
#' @details This function is useful when used with the group_by() function of the dplyr package.
#' If you want to compute by level of the categorical data you are interested in,
#' rather than the whole observation, you can use \code{\link{grouped_df}} as the group_by() function.
#' This function is computed stats::cor() function by use = "pairwise.complete.obs" option for numerical variable.
#' And support categorical variable with theil's U correlation coefficient and Cramer's V correlation coefficient.
#'
#' @section Correlation coefficient information:
#' It returns data.frame with the following variables.:
#'
#' \itemize{
#' \item var1 : names of numerical variable
#' \item var2 : name of the corresponding numeric variable
#' \item coef_corr : Correlation coefficient
#' }
#' 
#' When method = "cramer", data.frame with the following variables is returned.
#' \itemize{
#' \item var1 : names of numerical variable
#' \item var2 : name of the corresponding numeric variable
#' \item chisq : the value the chi-squared test statistic
#' \item df : the degrees of freedom of the approximate chi-squared distribution of the test statistic
#' \item pval : the p-value for the test
#' \item coef_corr : theil's U correlation coefficient (Uncertainty Coefficient).
#' }
#' 
#'
#' @param .data a data.frame or a \code{\link{grouped_df}} or a tbl_dbi.
#' @param method a character string indicating which correlation coefficient (or covariance) is 
#' to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' For numerical variables, one of "pearson" (default), "kendall", or 
#' "spearman": can be used as an abbreviation.
#' For categorical variables, "cramer" and "theil" can be used. "cramer" 
#' computes Cramer's V statistic, "theil" computes Theil's U statistic.
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
#' @seealso \code{\link{cor}}, \code{\link{summary.correlate}}, \code{\link{plot.correlate}}.
#' @export
#' @examples
#' \donttest{
#' # Correlation coefficients of all numerical variables
#' tab_corr <- correlate(heartfailure)
#' tab_corr
#' 
#' mat_corr <- summary(tab_corr)
#' mat_corr
#' 
#' plot(tab_corr)
#' 
#' # Select the variable to compute
#' correlate(heartfailure, "creatinine", "sodium")
#' 
#' # Non-parametric correlation coefficient by kendall method
#' correlate(heartfailure, creatinine, method = "kendall")
#' 
#' # theil's U correlation coefficient (Uncertainty Coefficient)
#' tab_corr <- correlate(heartfailure, anaemia, hblood_pressure, method = "theil")
#' tab_corr
#' 
#' summary(tab_corr)   
#' plot(tab_corr)
#'    
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(heartfailure, smoking, death_event)
#' correlate(gdata)
#'
#' # Using pipes ---------------------------------
#' # Correlation coefficients of all numerical variables
#' heartfailure %>%
#'   correlate()
#'   
#' # Non-parametric correlation coefficient by spearman method
#' heartfailure %>%
#'   correlate(creatinine, sodium, method = "spearman")
#'  
#' # ---------------------------------------------
#' # Correlation coefficient
#' # that eliminates redundant combination of variables
#' heartfailure %>%
#'   correlate() %>%
#'   filter(as.integer(var1) > as.integer(var2))
#'
#' # Using pipes & dplyr -------------------------
#' # Compute the correlation coefficient of 'creatinine' variable by 'smoking'
#' # and 'death_event' variables. And extract only those with absolute
#' # value of correlation coefficient is greater than 0.2
#' heartfailure %>%
#'   group_by(smoking, death_event) %>%
#'   correlate(creatinine) %>%
#'   filter(abs(coef_corr) >= 0.2)
#'
#' # extract only those with 'smoking' variable level is "Yes",
#' # and compute the correlation coefficient of 'Sales' variable
#' # by 'hblood_pressure' and 'death_event' variables.
#' # And the correlation coefficient is negative and smaller than 0.5
#' heartfailure %>%
#'   filter(smoking == "Yes") %>%
#'   group_by(hblood_pressure, death_event) %>%
#'   correlate(creatinine) %>%
#'   filter(coef_corr < 0) %>%
#'   filter(abs(coef_corr) > 0.5)
#' }
#' 
#' @method correlate data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
correlate.data.frame <- function(.data, ..., 
                                 method = c("pearson", "kendall", "spearman", 
                                            "cramer", "theil")) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  if (method %in% c("pearson", "kendall", "spearman")) {
    result <- correlate_impl_num(.data, vars, method)
  } else if (method %in% c("cramer", "theil")) {
    result <- correlate_impl_cat(.data, vars, method)
  }  
  
  result
}


#' @import tibble
#' @importFrom stats cor
correlate_impl_num <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)

  idx_numeric <- find_class(df, type = "numerical")

  if (length(idx_numeric) < 2) 
    stop("Fewer numerical variables for which to compute correlation coefficients.")
  
  suppressWarnings(M <- cor(df[, names(df)[idx_numeric]],
    use = "pairwise.complete.obs", method = method))

  m <- as.vector(M)
  tab <- as_tibble(expand.grid(var1 = row.names(M),
    var2 = row.names(M)))
  tab_corr <- add_column(tab, coef_corr = m) %>%
    filter(var1 != var2) %>%
    filter(var1 %in% vars)
  
  
  attr(tab_corr, "type") <- "generic"
  attr(tab_corr, "variable") <- "numeric"  
  attr(tab_corr, "method") <- method
  
  class(tab_corr) <- append("correlate", class(tab_corr))
  
  tab_corr
}


#' @import tibble
#' @importFrom stats cor
correlate_impl_cat <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)
  
  name_categorical <- find_class(df, type = "categorical", index = FALSE)
  df <- df[, name_categorical]
  
  tab <- as_tibble(
    expand.grid(
      var1 = name_categorical,
      var2 = name_categorical)
    ) %>% 
    filter(var1 != var2) %>%
    filter(var1 %in% vars)
  
  if (NROW(tab) < 1) 
    stop("Fewer categorical variables for which to compute correlation coefficients.")
  
  tab_corr <- tab %>% 
    NROW() %>% 
    seq() %>%
    purrr::map_df({
      function(x) {
        do.call(method, list(df, tab$var1[x], tab$var2[x]))
      }
    })
  
  attr(tab_corr, "type") <- "generic"
  attr(tab_corr, "variable") <- "categorical"  
  attr(tab_corr, "method") <- method
  
  class(tab_corr) <- append("correlate", class(tab_corr))
  
  tab_corr
}


#' @rdname correlate.data.frame
#' @method correlate grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
correlate.grouped_df <- function(.data, ..., 
                                 method = c("pearson", "kendall", "spearman", 
                                            "cramer", "theil")) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  if (method %in% c("pearson", "kendall", "spearman")) {
    result <- correlate_group_impl_num(.data, vars, method)
  } else if (method %in% c("cramer", "theil")) {
    result <- correlate_group_impl_cat(.data, vars, method)
  }  

  result
}

#' @importFrom tidyr unnest
#' @importFrom stats cor
#' @import tibble
correlate_group_impl_num <- function(df, vars, method) {
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
    tab_corr <- tibble(statistic) %>%
      tidyr::unnest()
  )  
  
  attr(tab_corr, "type") <- "group"
  attr(tab_corr, "group_variable") <- setdiff(names(tab_corr), 
                                              c("var1", "var2", "coef_corr"))  
  attr(tab_corr, "variable") <- "numeric"  
  attr(tab_corr, "method") <- method
  
  class(tab_corr) <- append("correlate", class(tab_corr))
  
  tab_corr
}


#' @importFrom tidyr unnest
#' @importFrom purrr map_df
#' @importFrom stats cor
#' @import tibble
correlate_group_impl_cat <- function(df, vars, method) {
  if (length(vars) == 0) vars <- names(df)
  
  # use type = "categorical2", because in DBMS data, factor convert to character. 
  name_categorical <- find_class(df, type = "categorical2", index = FALSE)
  df <- df %>% 
    select(name_categorical)
  
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
    
    tab <- as_tibble(
      expand.grid(
        var1 = name_categorical,
        var2 = name_categorical)
    ) %>% 
      filter(var1 != var2) %>%
      filter(var1 %in% vars)
    
    if (NROW(tab) < 1) 
      stop("Fewer categorical variables for which to compute correlation coefficients.")
    
    tab_corr <- tab %>% 
      NROW() %>% 
      seq() %>%
      purrr::map_df({
        function(x) {
          do.call(method, list(df[idx, ] %>% ungroup(), tab$var1[x], tab$var2[x]))
        }
      })
    
    corrs <- merge(label, tab_corr)
    
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
    tab_corr <- tibble(statistic) %>%
      tidyr::unnest()
  )  
  
  attr(tab_corr, "type") <- "group"
  attr(tab_corr, "group_variable") <- setdiff(names(tab_corr), 
                                              c("var1", "var2", "coef_corr"))  
  attr(tab_corr, "variable") <- "categorical"  
  attr(tab_corr, "method") <- method
  
  class(tab_corr) <- append("correlate", class(tab_corr))
  
  tab_corr
}


#' Summarizing Correlation Coefficient
#'
#' @description summary method for "correlate" class.
#' @param object an object of class "correlate", usually, a result of a call 
#' to correlate().
#' @param ... further arguments passed to or from other methods.
#' @details
#' summary.correlate compares the correlation coefficient by variables.
#'
#' @seealso \code{\link{correlate}}, \code{\link{plot.correlate}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Correlation type is "generic" ===============================
#' # Correlation coefficients of all numerical variables
#' corr_tab <- correlate(heartfailure)
#' corr_tab
#' 
#' # summary correlate class 
#' mat <- summary(corr_tab)
#' mat
#' 
#' # Select the variable to compute
#' corr_tab <- correlate(heartfailure, creatinine, sodium)
#' corr_tab
#' 
#' # summary correlate class 
#' mat <- summary(corr_tab)
#' mat
#' 
#' # Correlation type is "group" ===============================
#' ##-----------------------------------------------------------
#' # If the target variable is a categorical variable
#' # Using dplyr
#' corr_tab <- heartfailure %>%
#'   group_by(smoking, death_event) %>% 
#'   correlate() 
#' 
#' corr_tab
#' 
#' # summary correlate class 
#' mat <- summary(corr_tab)
#' mat
#' 
#' corr_tab <- heartfailure %>%
#'   group_by(smoking, death_event) %>% 
#'   correlate(creatinine) %>% 
#'   filter(abs(coef_corr) >= 0.2)
#' 
#' corr_tab
#' 
#' # summary correlate class 
#' mat <- summary(corr_tab)
#' mat
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Correlation coefficients of all numerical variables
#' corr_tab <- con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   correlate()
#'
#' # summary correlate class 
#' mat <- summary(corr_tab)
#' mat
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
#' @method summary correlate
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @export
summary.correlate <- function(object, ...) {
  type <- attr(object, "type")
  variable <- attr(object, "variable")
  method <- attr(object, "method")
  
  cat("* correlation type   :", type, "\n")  
  cat("* variable type      :", variable, "\n")   
  cat("* correlation method :", method, "\n")     
  
  if (type == "group") {
    group_variable <- attr(object, "group_variable")
    cat("* grouped variable   :", group_variable, "\n")   
    
    groups <- object %>% 
      select(attr(object, "group_variable")) %>% 
      unique()
    
    smmry <- groups %>% 
      NROW() %>% 
      seq() %>% 
      purrr::map({
        function(x) {
          tab_subset <- groups[x,] %>% 
            inner_join(object, by = attr(object, "group_variable"))
  
          len_var1 <- length(unique(tab_subset$var1))
          len_var2 <- length(unique(tab_subset$var2))
          
          if (len_var1 == len_var2) {
            tab <- tab_subset 
          } else {
            tab <- tab_subset %>% 
              mutate(var1 = factor(var1, levels = sort(unique(.$var1)))) %>% 
              mutate(var2 = factor(var2, levels = sort(unique(.$var2)))) %>%     
              mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
          }
          
          mat_corr <- tab %>% 
            select(var1, var2, coef_corr) %>% 
            tidyr::pivot_wider(names_from = "var2", values_from = "coef_corr") %>% 
            arrange(var1) %>% 
            tibble::column_to_rownames(var = "var1") %>% 
            as.matrix()
          
          mat_corr[is.na(mat_corr)] <- 1      
          
          mat_corr
        }
      })
    
    groups <- groups %>% 
      mutate_all(as.character) 
    
    name_smmry <- groups %>% 
      NROW() %>% 
      seq() %>% 
      purrr::map_chr({
        function(x) {
          val <- groups[x, ]
          nm <- names(val)
          
          paste(nm, val , sep = "==", collapse = ", ")
        }  
      })  

    names(smmry) <- name_smmry
    
    cat("\n* Matrix of Correlation\n")    
    print(smmry)
    
    invisible(smmry)
  } else if (type == "generic") {
    len_var1 <- length(unique(object$var1))
    len_var2 <- length(unique(object$var2))
    
    if (len_var1 == len_var2) {
      tab <- object 
    } else {
      tab <- object %>% 
        mutate(var1 = factor(var1, levels = sort(unique(.$var1)))) %>% 
        mutate(var2 = factor(var2, levels = sort(unique(.$var2)))) %>%     
        mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
    }
    
    smmry <- tab %>% 
      select(var1, var2, coef_corr) %>% 
      tidyr::pivot_wider(names_from = "var2", values_from = "coef_corr") %>% 
      arrange(var1) %>% 
      tibble::column_to_rownames(var = "var1") %>% 
      as.matrix()
    
    smmry[is.na(smmry)] <- 1
    
    cat("\n* Matrix of Correlation\n")
    print(smmry)    
  }  
  
  invisible(smmry)
}




#' Visualize correlation plot of numerical data
#'
#' @description 
#' The plot_correlate() visualize correlation plot
#' for find relationship between two numerical variables.
#'
#' @details The scope of the visualization is the provide a correlation information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#' 
#' The direction of the diagonal is top-left to bottom-right. and color of the 
#' cells is 'red' to -1, 'blue' to 1.
#'
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param method a character string indicating which correlation coefficient (or covariance) is 
#' to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
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
#' @method plot_correlate data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
#' @keywords internal
plot_correlate.data.frame <- function(.data, ..., 
                                      method = c("pearson", "kendall", "spearman"),
                                      typographic = TRUE, base_family = NULL) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  plot_correlate_impl(.data, vars, method, typographic, base_family)
}


#' @import ggplot2
#' 
plot_correlate_impl <- function(df, vars, method, typographic, base_family) {
  if (length(vars) == 0) vars <- names(df)

  dat <- df %>% 
    correlate(method = method) %>% 
    filter(var2 %in% vars)
    
  len_var1 <- length(unique(dat$var1))
  len_var2 <- length(unique(dat$var2))
  
  if (len_var1 == len_var2) {
    dat <- dat %>% 
      mutate(var1 = factor(var1, levels = unique(.$var2))) %>% 
      mutate(var2 = factor(var2, levels = unique(.$var2))) %>%     
      mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
  } else {
    dat <- dat %>% 
      mutate(var1 = factor(var1, levels = sort(unique(.$var1)))) %>% 
      mutate(var2 = factor(var2, levels = sort(unique(.$var2)))) %>%     
      mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
  }
    
  p <- dat %>%   
    ggplot(aes(var1, var2, fill = coef_corr, label = round(coef_corr, 2))) +
    geom_tile(col = "black") + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                         limits = c(-1, 1)) +
    geom_text() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "Correlation\nCoefficient") + 
    coord_equal() +
    theme_grey(base_family = base_family) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1),
          panel.grid.major = element_blank())
  
  if (typographic) {
    p <- p + theme_typographic(base_family) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 40, hjust = 1),
            panel.grid.major = element_blank())
  } 

  p
}


#' @method plot_correlate grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos warn
#' @export
plot_correlate.grouped_df <- function(.data, ..., method = c("pearson", "kendall", 
                                                             "spearman"),
                                      typographic = TRUE, base_family = NULL) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  method <- match.arg(method)
  
  plot_correlate_group_impl(.data, vars, method, typographic, base_family)
}


#' @import ggplot2
#' @import dplyr
#' @importFrom utils packageVersion
plot_correlate_group_impl <- function(df, vars, method, typographic, base_family) {
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
        len_var1 <- length(unique(df_corr$var1))
        len_var2 <- length(unique(df_corr$var2))
        
        if (len_var1 == len_var2) {
          df_corr <- df_corr %>% 
            mutate(var1 = factor(var1, levels = unique(.$var2))) %>% 
            mutate(var2 = factor(var2, levels = unique(.$var2))) %>%     
            mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
        } else {
          df_corr <- df_corr %>% 
            mutate(var1 = factor(var1, levels = sort(unique(.$var1)))) %>% 
            mutate(var2 = factor(var2, levels = sort(unique(.$var2)))) %>%     
            mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
        }
        
        p <- df_corr %>%   
          ggplot(aes(var1, var2, fill = coef_corr, label = round(coef_corr, 2))) +
          geom_tile(col = "black") + 
          scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                               limits = c(-1, 1)) +
          geom_text() +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_discrete(expand = c(0, 0)) +
          labs(title = label, 
               fill = "Correlation\nCoefficient") + 
          coord_equal() +
          theme_grey(base_family = base_family) +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(angle = 40, hjust = 1),
                panel.grid.major = element_blank())
        
        if (typographic) {
          p <- p + 
            theme_typographic(base_family) +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 40, hjust = 1),
                  panel.grid.major = element_blank())
        } 
        
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


#' Visualize Information for an "correlate" Object
#'
#' @description
#' Visualize by attribute of `correlate` class.
#' The plot of correlation matrix is a tile plot.
#' 
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "correlate", usually, a result of a call to correlate().
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' @seealso \code{\link{correlate}}, \code{\link{summary.correlate}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # correlate type is generic ==================================
#' tab_corr <- correlate(iris)
#' tab_corr
#' 
#' # visualize correlate class 
#' plot(tab_corr)
#' 
#' tab_corr <- iris %>% 
#'   correlate(Sepal.Length, Petal.Length)
#' tab_corr
#' 
#' # visualize correlate class 
#' plot(tab_corr)
#' 
#' # correlate type is group ====================================
#' tab_corr <- iris %>% 
#'   group_by(Species) %>% 
#'   correlate()
#' 
#' # plot correlate class
#' plot(tab_corr)
#' 
#' # S3 method for correlate class by 'tbl_dbi' ================
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy iris to the DBMS with a table named TB_IRIS
#' copy_to(con_sqlite, iris, name = "TB_IRIS", overwrite = TRUE)
#' 
#' # correlation coefficients of all numerical variables
#' tab_corr <- con_sqlite %>% 
#'   tbl("TB_IRIS") %>% 
#'   correlate()
#'   
#' plot(tab_corr)   
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
#' @method plot correlate
#' @import ggplot2
#' @import dplyr
#' @importFrom purrr walk
#' @export
plot.correlate <- function(x, typographic = TRUE, base_family = NULL, ...) {
  type <- attr(x, "type")
  variable <- attr(x, "variable")
  
  if (variable %in% "numeric") {
    limits <- c(-1, 1)
    low <- "red"
    high <- "blue"
  } else {
    limits <- c(0, 1)
    low <- "white"
    high <- "steelblue"
  }
    
  if (type == "group") {
    groups <- x %>% 
      select(attr(x, "group_variable")) %>% 
      unique()
    
    groups %>% 
      NROW() %>% 
      seq() %>% 
      purrr::walk({
        function(idx) {
          val <- groups[idx, ]
          nm <- names(val)
          
          condition <- paste(nm, pull(val), sep = "==", collapse = ", ")
          
          tab_subset <- groups[idx, ] %>% 
            inner_join(x, by = attr(x, "group_variable"))
          
          len_var1 <- length(unique(tab_subset$var1))
          len_var2 <- length(unique(tab_subset$var2))
          
          if (len_var1 == len_var2) {
            tab_subset <- tab_subset %>% 
              mutate(var1 = factor(var1, levels = unique(.$var2))) %>% 
              mutate(var2 = factor(var2, levels = unique(.$var2))) %>%     
              mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
          } else {
            tab_subset <- tab_subset %>% 
              mutate(var1 = factor(var1, levels = sort(unique(.$var1)))) %>% 
              mutate(var2 = factor(var2, levels = sort(unique(.$var2)))) %>%     
              mutate(var1 = factor(var1, levels = rev(levels(.$var1)))) %>% 
              select(var2, var1, coef_corr) 
            
            names(tab_subset) <- c("var1", "var2", "coef_corr")
          }
          
          p_corr <- tab_subset %>% 
            select(var1, var2, coef_corr) %>% 
            ggplot(aes(x = var1, y = var2, fill = coef_corr)) +
            geom_tile(col = "black") + 
            scale_fill_gradient2(low = low, high = high, limits = limits) +
            geom_text(aes(label = round(coef_corr, 2))) +
            scale_x_discrete(expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(title = paste0("Correlation Matrix (", attr(x, "method"), ") - ", condition), 
                 fill = "Correlation\nCoieficent",
                 x = "variable 2", y = "variable 2") + 
            coord_fixed() +
            theme_grey(base_family = base_family) +
            theme(axis.text.x = element_text(angle = 40, hjust = 1),
                  panel.grid.major = element_blank())
          
          if (typographic) {
            p_corr <- p_corr +
              theme_typographic(base_family) +
              theme(axis.title.x = element_text(size = 13),
                    axis.title.y = element_text(size = 13))
          }
          
          print(p_corr)
        }
      })
  } else if (type == "generic") {
    len_var1 <- length(unique(x$var1))
    len_var2 <- length(unique(x$var2))
    
    if (len_var1 == len_var2) {
      x <- x %>% 
        mutate(var1 = factor(var1, levels = unique(.$var2))) %>% 
        mutate(var2 = factor(var2, levels = unique(.$var2))) %>%     
        mutate(var2 = factor(var2, levels = rev(levels(.$var2)))) 
    } else {
      x <- x %>% 
        mutate(var1 = factor(var1, levels = sort(unique(.$var1)))) %>% 
        mutate(var2 = factor(var2, levels = sort(unique(.$var2)))) %>%     
        mutate(var1 = factor(var1, levels = rev(levels(.$var1)))) %>% 
        select(var2, var1, coef_corr) 
      
      names(x) <- c("var1", "var2", "coef_corr")
    }
    
    p_corr <- x %>% 
      ggplot(aes(x = var1, y = var2, fill = coef_corr)) +
      geom_tile(col = "black") + 
      scale_fill_gradient2(low = low, high = high, limits = limits) +
      geom_text(aes(label = round(coef_corr, 2))) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(title = paste0("Correlation Matrix (", attr(x, "method"), ")"), 
           fill = "Correlation\nCoieficent",
           x = "variable 2", y = "variable 2") + 
      coord_fixed() +
      theme_grey(base_family = base_family) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
            panel.grid.major = element_blank())
    
    if (typographic) {
      p_corr <- p_corr + theme_typographic(base_family) +
        theme(axis.text.x = element_text(angle = 40, hjust = 1),
              panel.grid.major = element_blank())
    }
    
    print(p_corr)
  } 
}


