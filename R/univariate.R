#' @rdname univar_category.data.frame
#' @export
univar_category <- function(.data, ...) {
  UseMethod("univar_category", .data)
}


#' @rdname univar_numeric.data.frame
#' @export
univar_numeric <- function(.data, ...) {
  UseMethod("univar_numeric", .data)
}


#'  Statistic of univariate categorical variables
#'
#' @description The univar_category() calculates statistic of categorical variables that is frequency table
#'
#' @details 
#' univar_category() calculates the frequency table of categorical variables. 
#' If a specific variable name is not specified, frequency tables for all categorical variables included in the data are calculated.
#' The univar_category class returned by univar_category() is useful because it can draw chisqure tests and bar plots as well as frequency tables of individual variables.
#' and return univar_category class that based list object.
#'
#' @return An object of the class as individual variables based list.
#' The information to examine the relationship between categorical variables is as follows each components.
#'
#' \itemize{
#' \item variable : factor. The level of the variable. 'variable' is the name of the variable.
#' \item n : integer. frequency by variable.
#' \item rate : double. relative frequency.
#' }
#'
#' @section Attributes of return object:
#' Attributes of compare_category class is as follows.
#' \itemize{
#' \item variables : character. List of variables selected for calculate frequency.
#' }
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' @seealso \code{\link{summary.univar_category}}, \code{\link{print.univar_category}}, \code{\link{plot.univar_category}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' library(stringr)
#' 
#' # Calculates the all categorical variables
#' all_var <- univar_category(carseats)
#' 
#' # Print univar_category class object
#' all_var
#' 
#' # Calculates the only Urban variable
#' all_var %>% 
#'   "["(str_detect(names(all_var), "Urban"))
#'   
#' urban <- univar_category(carseats, Urban)
#'   
#' # Print univar_category class object
#' urban
#' 
#' # Filtering the case of Urban included NA 
#' urban %>%
#'   "[["(1) %>% 
#'   filter(!is.na(Urban))
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#'
#' # plot all variables
#' # plot(all_var)
#' 
#' # plot urban
#' # plot(urban)
#' 
#' # plot all variables by prompt
#' # plot(all_var, prompt = TRUE)
#' 
#' @method univar_category data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
univar_category.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  univar_category_impl(.data, vars)
}


#' @import tibble
#' @importFrom purrr map
univar_category_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)
  
  df <- df %>% 
    select(vars)
  
  idx_category <- find_class(df, type = "categorical")
  
  if (length(idx_category) < 1) {
    stop("The categorical variables not selected.")
  }
  
  vars <- names(df)[idx_category]
  
  get_frequency <- function(x) {
    suppressWarnings(agg_tab <- df %>% 
      select(variable = x) %>% 
      count(variable) %>% 
      mutate(rate = n / sum(n)))
    
    names(agg_tab)[1] <- x
    agg_tab
  }
  
  result <- purrr::map(vars, get_frequency)
  
  attr(result, "variables") <- vars
  
  names(result) <- vars
  
  class(result) <- append("univar_category", class(result))
  result
}


#'  Statistic of univariate numerical variables
#'
#' @description The univar_numeric() calculates statistic of numerical variables that is frequency table
#'
#' @details 
#' univar_numeric() calculates the popular statistics of  numerical variables. 
#' If a specific variable name is not specified, statistics for all categorical numerical included in the data are calculated.
#' The statistics obtained by univar_numeric() are part of those obtained by describe(). 
#' Therefore, it is recommended to use describe() to simply calculate statistics. 
#' However, if you want to visualize the distribution of individual variables, you should use univar_numeric().
#'
#' @return An object of the class as individual variables based list.
#' A component named "statistics" is a tibble object with the following statistics.:
#' \itemize{
#' \item variable : factor. The level of the variable. 'variable' is the name of the variable.
#' \item n : number of observations excluding missing values
#' \item na : number of missing values
#' \item mean : arithmetic average
#' \item sd : standard deviation
#' \item se_mean : standrd error mean. sd/sqrt(n)
#' \item IQR : interquartile range (Q3-Q1)
#' \item skewness : skewness
#' \item kurtosis : kurtosis
#' \item median : median. 50\% percentile
#' }
#'
#' @section Attributes of return object:
#' Attributes of compare_category class is as follows.
#' \itemize{
#' \item raw : a data.frame or a \code{\link{tbl_df}}. Data containing variables to be compared. Save it for visualization with plot.univar_numeric().
#' \item variables : character. List of variables selected for calculate statistics.
#' }
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' @seealso \code{\link{summary.univar_numeric}}, \code{\link{print.univar_numeric}}, \code{\link{plot.univar_numeric}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' 
#' # Calculates the all categorical variables
#' all_var <- univar_numeric(carseats)
#' 
#' # Print univar_numeric class object
#' all_var
#' 
#' # Calculates the Price, CompPrice variable
#' univar_numeric(carseats, Price, CompPrice)
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#'
#' # Statistics of numerical variables normalized by Min-Max method
#' summary(all_var, stand = "minmax")
#' 
#' # Statistics of numerical variables standardized by Z-score method
#' summary(all_var, stand = "zscore")
#' 
#' # one plot with all variables
#' # plot(all_var)
#' 
#' # one plot with all normalized variables by Min-Max method
#' # plot(all_var, stand = "minmax")
#' 
#' # one plot with all variables
#' # plot(all_var, stand = "none")
#' 
#' # one plot with all robust standardized variables 
#' # plot(all_var, viz = "boxplot")
#' 
#' # one plot with all standardized variables by Z-score method 
#' # plot(all_var, viz = "boxplot", stand = "zscore")
#' 
#' # individual boxplot by variables
#' # plot(all_var, indiv = TRUE, "boxplot")
#' 
#' # individual histogram by variables
#' # plot(all_var, indiv = TRUE, "hist")
#' 
#' # individual histogram by robust standardized variable 
#' # plot(all_var, indiv = TRUE, "hist", stand = "robust")
#' 
#' # plot all variables by prompt
#' # plot(all_var, indiv = TRUE, "hist", prompt = TRUE)
#' 
#' @method univar_numeric data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
univar_numeric.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  univar_numeric_impl(.data, vars)
}


#' @import tibble
univar_numeric_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)
  
  df <- df %>% 
    select(vars)
  
  idx_numeric <- find_class(df, type = "numerical")
  
  if (length(idx_numeric) < 1) {
    stop("The numerical variables not selected.")
  }
  
  df <- df %>% 
    select(idx_numeric)
  
  vars <- names(df)
  
  result <- list(statistics = describe(df, vars) %>% select(1:9, median = "p50"))
  
  attr(result, "variables") <- vars
  attr(result, "raw") <- df
  
  class(result) <- append("univar_numeric", class(result))
  result
}


#' Summarizing univar_category information
#'
#' @description print and summary method for "univar_category" class.
#' @param object an object of class "univar_category", usually, a result of a call to univar_category().
#' @param na.rm logical. Specifies whether to include NA when performing a chi-square test. 
#' The default is TRUE, where NA is removed and aggregated.
#' @param ... further arguments passed to or from other methods.
#' @details
#' print.univar_category() displays only the information of variables included in univar_category. 
#' The "variables" attribute is not displayed.
#'
#' @return An object of the class as individual variables based list.
#' The information to examine the relationship between categorical variables is as follows each components.
#'
#' \itemize{
#' \item variable : factor. The level of the variable. 'variable' is the name of the variable.
#' \item statistic : numeric. the value the chi-squared test statistic.
#' \item p.value : numeric. the p-value for the test.
#' \item df : integer. the degrees of freedom of the chi-squared test.
#' }
#' 
#' @seealso \code{\link{plot.univar_category}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' library(stringr)
#' 
#' # Calculates the all categorical variavels
#' all_var <- univar_category(carseats)
#' 
#' # Print univar_category class object
#' all_var
#' 
#' # Calculates the only Urban variable
#' all_var %>% 
#'   "["(str_detect(names(all_var), "Urban"))
#'   
#' urban <- univar_category(carseats, Urban)
#'   
#' # Print univar_category class object
#' urban
#' 
#' # Filtering the case of Urban included NA 
#' urban %>%
#'   "[["(1) %>% 
#'   filter(!is.na(Urban))
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#'   
#' # Summary chi-squared test by returned object exclude NA
#' summary(urban)
#' 
#' # Summary chi-squared test by returned object include NA
#' summary(urban, na.rm = FALSE)
#' 
#' @importFrom stats chisq.test
#' @importFrom purrr map_dfr
#' @method summary univar_category
#' @export
summary.univar_category <- function(object, na.rm = TRUE, ...) {
  variables <- attr(object, "variables")
  
  get_chisq <- function(x) {
    if (na.rm) {
      tabs <- object %>% 
        "[["(x) %>% 
        select(variable = 1, 2) %>% 
        filter(!is.na(variable)) %>% 
        select(2)
    } else {
      tabs <- object %>% 
        "[["(x) %>% 
        select(2)
    }
    
    suppressWarnings(tabs%>% 
                       stats::chisq.test() %>% 
                       get_tab_chisq() %>% 
                       select(-method))
  }
  
  chisq <- purrr::map_dfr(variables, get_chisq)
  chisq <- cbind(data.frame(variables = variables), chisq)
  names(chisq)[4] <- "df"
  
 chisq
}


#' Summarizing univar_numeric information
#'
#' @description print and summary method for "univar_numeric" class.
#' @param object an object of class "univar_numeric", usually, a result of a call to univar_numeric().
#' @param stand character Describe how to standardize the original data. 
#' "robust" normalizes the raw data through transformation calculated by IQR and median.
#' "minmax" normalizes the original data using minmax transformation.
#' "zscore" standardizes the original data using z-Score transformation.
#' The default is "robust".
#' @param ... further arguments passed to or from other methods.
#' @details
#' print.univar_numeric() displays only the information of variables included in univar_numeric 
#' The "variables" attribute is not displayed.
#'
#' @return An object of the class as indivisual variabes based list.
#' The statistics returned by summary.univar_numeric() are different from the statistics returned by univar_numeric().
#' univar_numeric() is the statistics for the original data, but summary. univar_numeric() is the statistics for the standardized data.
#' A component named "statistics" is a tibble object with the following statistics.:
#' \itemize{
#' \item variable : factor. The level of the variable. 'variable' is the name of the variable.
#' \item n : number of observations excluding missing values
#' \item na : number of missing values
#' \item mean : arithmetic average
#' \item sd : standard deviation
#' \item se_mean : standard error mean. sd/sqrt(n)
#' \item IQR : interquartile range (Q3-Q1)
#' \item skewness : skewness
#' \item kurtosis : kurtosis
#' \item median : median. 50\% percentile
#' }
#' 
#' @seealso \code{\link{plot.univar_numeric}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' 
#' # Calculates the all categorical variavels
#' all_var <- univar_numeric(carseats)
#' 
#' # Print univar_numeric class object
#' all_var
#' 
#' # Calculates the Price, CompPrice variable
#' univar_numeric(carseats, Price, CompPrice)
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#'
#' # Statistics of numerical variables normalized by Min-Max method
#' summary(all_var, stand = "minmax")
#' 
#' # Statistics of numerical variables standardized by Z-score method
#' summary(all_var, stand = "zscore")
#' 
#' @importFrom purrr map_dfc
#' @method summary univar_numeric
#' @export
summary.univar_numeric <- function(object, stand = c("robust", "minmax", "zscore"), ...) {
  stand <- match.arg(stand)
  variables <- attr(object, "variables")
  
  df <- attr(object, "raw")
  
    if (stand == "minmax") {
      minmax <- function(x) {
        (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) 
      }
      
      df <- df %>% 
        apply(2, minmax) %>% 
        as.data.frame() 
    } else if (stand == "zscore") {
      df <- df %>% 
        scale() %>% 
        as.data.frame() 
    } else if (stand == "robust") {
      iqrs <- object$statistics$IQR
      medians <- object$statistics$median
      
      n_var <- ncol(df)
      
      get_robust <- function(x) {
        robust <- function(x, p50, IQR) {
          (x - p50) / IQR
        }
        
        robust(df[, x],  medians[x], iqrs[x])
      }
      
      suppressMessages(
        df <- purrr::map_dfc(seq(n_var), get_robust)
      )  
      names(df) <- variables
    }
  
  describe(df) %>% 
    select(1, 4:9, median = 18)
}


#' @param x an object of class "univar_category", usually, a result of a call to univar_category().
#' @param ... further arguments passed to or from other methods.
#' @rdname summary.univar_category
#' @method print univar_category
#' @export
print.univar_category <- function(x, ...) {
  vnames <- names(x)
  attributes(x) <- NULL
  names(x) <- vnames
  
  print(x, ...)
}


#' @param x an object of class "univar_numeric", usually, a result of a call to univar_numeric().
#' @param ... further arguments passed to or from other methods.
#' @rdname summary.univar_numeric
#' @method print univar_numeric
#' @export
print.univar_numeric <- function(x, ...) {
  vnames <- names(x)
  attributes(x) <- NULL
  names(x) <- vnames
  
  print(x, ...)
}



#' Visualize Information for an "univar_category" Object
#'
#' @description
#' Visualize mosaics plot by attribute of univar_category class.
#'
#' @param x an object of class "univar_category", usually, a result of a call to univar_category().
#' @param prompt logical. The default value is FALSE. If there are multiple visualizations to be output, 
#' if this argument value is TRUE, a prompt is output each time. 
#' @param na.rm logical. Specifies whether to include NA when plotting bar plot. 
#' The default is FALSE, so plot NA.  
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' However, it does not support all parameters.
#' @seealso \code{\link{univar_category}}, \code{\link{print.univar_category}}, \code{\link{summary.univar_category}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' 
#' # Calculates the all categorical variables
#' all_var <- univar_category(carseats)
#' 
#' # Print univar_category class object
#' all_var
#' 
#' # Calculates the only Urban variable
#' urban <- univar_category(carseats, Urban)
#'   
#' # Print univar_category class object
#' urban
#'
#' # plot all variables
#' plot(all_var)
#' 
#' # plot urban
#' plot(urban)
#'
#' # plot all variables by na.rm = FALSE
#' plot(all_var, na.rm = FALSE)
#'  
#' # plot all variables by prompt
#' # plot(all_var, prompt = TRUE)
#' 
#' # not allow the typographic elements
#' plot(all_var, typographic = FALSE)
#' 
#' @method plot univar_category
#' @import ggplot2
#' @export
plot.univar_category <- function(x, na.rm = TRUE, prompt = FALSE, typographic = TRUE, ...) {
  variables <- attr(x, "variables")
  
  n <- length(variables)
  
  for (i in seq(n)) {

    if (prompt & n > 1) {
      invisible(readline(prompt="Hit <Return> to see next plot:"))
    }
    
    df <- x %>% 
      "[["(variables[i]) %>% 
      select(variable = 1, 2:3) 
    
    if (na.rm) {
      df <- df %>%
        filter(!is.na(variable))
    }
      
    obj <- df %>% 
      ggplot(aes(variable, n, fill = variable)) +
      geom_bar(stat = "identity") + 
      ylab("Frequency") +
      ggtitle(sprintf("Bar plot of %s", variables[i])) +
      xlab(variables[i]) + 
      theme_bw() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(hjust = 0.5))
    
    if (typographic) {
      obj <- obj +
        theme_typographic() +
        scale_fill_ipsum() +
        theme(legend.position = "None",
              axis.title.x = element_text(size = 13),
              axis.title.y = element_text(size = 13)
        )  
    }
    
    suppressWarnings(print(obj))
  } 
}


#' Visualize Information for an "univar_numeric" Object
#'
#' @description
#' Visualize boxplots and histogram by attribute of univar_numeric class.
#'
#' @param x an object of class "univar_numeric", usually, a result of a call to univar_numeric().
#' @param indiv logical. Select whether to display information of all variables in one plot when there are multiple selected numeric variables. 
#' In case of FALSE, all variable information is displayed in one plot. 
#' If TRUE, the information of the individual variables is output to the individual plots. 
#' The default is FALSE. If only one variable is selected, TRUE is applied.
#' @param viz character. Describe what to plot visualization. "hist" draws a histogram and "boxplot" draws a boxplot. The default is "hist".
#' @param stand character. Describe how to standardize the original data. 
#' "robust" normalizes the raw data through transformation calculated by IQR and median.
#' "minmax" normalizes the original data using minmax transformation.
#' "zscore" standardizes the original data using z-Score transformation.
#' "none" does not perform data transformation.
#' he default is "none" if indiv is TRUE, and "robust" if FALSE.
#' @param prompt logical. The default value is FALSE. If there are multiple visualizations to be output, 
#' if this argument value is TRUE, a prompt is output each time. 
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package. 
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' However, it does not support.
#' @seealso \code{\link{univar_numeric}}, \code{\link{print.univar_numeric}}, \code{\link{summary.univar_numeric}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' 
#' # Calculates the all categorical variables
#' all_var <- univar_numeric(carseats)
#' 
#' # Print univar_numeric class object
#' all_var
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#' 
#' # one plot with all variables
#' plot(all_var)
#' 
#' # one plot with all normalized variables by Min-Max method
#' # plot(all_var, stand = "minmax")
#' 
#' # one plot with all variables
#' # plot(all_var, stand = "none")
#' 
#' # one plot with all robust standardized variables 
#' plot(all_var, viz = "boxplot")
#' 
#' # not allow the typographic elements
#' # plot(all_var, typographic = FALSE)
#' 
#' # one plot with all standardized variables by Z-score method 
#' # plot(all_var, viz = "boxplot", stand = "zscore")
#' 
#' # individual boxplot by variables
#' # plot(all_var, indiv = TRUE, "boxplot")
#' 
#' # individual histogram by variables
#' plot(all_var, indiv = TRUE, "hist")
#' 
#' # individual histogram by robust standardized variable 
#' # plot(all_var, indiv = TRUE, "hist", stand = "robust")
#' 
#' # plot all variables by prompt
#' # plot(all_var, indiv = TRUE, "hist", prompt = TRUE)
#' 
#' @importFrom tidyr gather
#' @import ggplot2
#' @method plot univar_numeric
#' @export
plot.univar_numeric <- function(x, indiv = FALSE, viz = c("hist", "boxplot"), 
                                    stand = ifelse(rep(indiv, 4), c("none", "robust", "minmax", "zscore"),
                                    c("robust", "minmax", "zscore", "none")), 
                                    prompt = FALSE, typographic = TRUE, ...) {
  stand <- match.arg(stand)
  viz <- match.arg(viz)
  
  variables <- attr(x, "variables")
  df <- attr(x, "raw")
  
  minmax <- function(x) {
    (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) 
  }
  
  robust <- function(x, p50, IQR) {
    (x - p50) / IQR
  }
  
  n <- length(variables)
  
  if (n == 1 )  indiv <-TRUE
    
  if (!indiv) {
    if (stand == "minmax") {
      df <- df %>% 
        apply(2, minmax) %>% 
        as.data.frame() 
      
      value <- "Normalization(MinMax)"
    } else if (stand == "zscore") {
      df <- df %>% 
        scale() %>% 
        as.data.frame() 
      
      value <- "Standardization(Z-Score)"
    } else if (stand == "robust") {
      iqrs <- x$statistics$IQR
      medians <- x$statistics$median
      
      get_robust <- function(x) {
        robust(df[, x],  medians[x], iqrs[x])
      }
      
      suppressMessages(
        df <- purrr::map_dfc(seq(medians), get_robust)
      )  
      names(df) <- variables
      
      value <- "Robust Normalization"
    } else {
      value <- "Original Data"
    } 

    if (viz == "boxplot") {
      p <- df %>% 
        tidyr::gather(variable, obs) %>% 
        ggplot(aes(variable, obs, fill = variable)) +
        geom_boxplot(alpha = 0.8) +
        ylab(value) +
        xlab("Variables") + 
        ggtitle(sprintf("Boxplots of %s", value)) +
        theme_bw() +
        theme(legend.position = "None") +
        theme(plot.title = element_text(hjust = 0.5))
      
      if (typographic) {
        p <- p +
          theme_typographic() +
          scale_fill_ipsum() +
          theme(legend.position = "None",
                axis.title.x = element_text(size = 13),
                axis.title.y = element_text(size = 13)
          )  
      }
      
      suppressMessages(
        suppressWarnings(print(p)))
    } else if (viz == "hist") {
      p <- df %>% 
        tidyr::gather(variable, obs) %>% 
        ggplot(aes(obs)) +
        facet_wrap(~variable) + 
        geom_histogram(color = "gray", fill = "steelblue") +
        ylab("Frequency") +
        xlab(value) + 
        ggtitle(sprintf("Histogram of %s", value)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      if (typographic) {
        p <- p +
          theme_typographic() +
          scale_fill_ipsum() +
          theme(legend.position = "None",
                axis.title.x = element_text(size = 13),
                axis.title.y = element_text(size = 13)
          )  
      }
      
      suppressMessages(
        suppressWarnings(print(p)))
    }
  } else {
    for (i in seq(n)) {
      
      if (prompt & n > 1) {
        invisible(readline(prompt="Hit <Return> to see next plot:"))
      }
      
      vec <- df %>% 
        "[["(variables[i]) 
      
      if (stand == "minmax") {
        vec <- minmax(vec) 
        
        value <- "with Normalization(MinMax)"
      } else if (stand == "zscore") {
        vec <- vec %>% 
          scale() 
        
        value <- "with Standardization(Z-Score)"
      } else if (stand == "robust") {
        iqrs <- x$statistics$IQR[i]
        medians <- x$statistics$median[i]
        
        vec <-  robust(vec,  medians, iqrs)
        
        value <- "with Robust Normalization"
      } else {
        value <- ""
      } 
      
      if (viz == "boxplot") {
        p <- data.frame(vec = vec) %>% 
          ggplot(aes(y = vec)) +
          geom_boxplot(alpha = 0.8, fill = "steelblue") +
          xlim(-0.7, 0.7) +
          labs(title = sprintf("Boxplot of %s", variables[i]), x = "", y = "") +
          theme_bw() +
          theme(legend.position = "None") +
          theme(axis.text.y = element_blank())
        
        if (typographic) {
          p <- p +
            theme_typographic() +
            theme(legend.position = "None",
                  axis.text.x = element_blank()
            )  
        }
      } else if (viz == "hist") {
        p <- data.frame(vec = vec) %>% 
          ggplot(aes(vec)) +
          geom_histogram(color = "gray", fill = "steelblue") +
          ylab("Frequency") +
          xlab(value) + 
          ggtitle(sprintf("Histogram of %s", variables[i])) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
        
        if (typographic) {
          p <- p +
            theme_typographic() +
            scale_fill_ipsum() +
            theme(legend.position = "None",
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)
            )  
        }
      }  
      
      suppressMessages(
        suppressWarnings(print(p)))
    } 
  }
}

