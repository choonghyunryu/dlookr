#' @rdname diagnose.data.frame
#' @export
diagnose <- function(.data, ...) {
  UseMethod("diagnose", .data)
}


#' Diagnose data quality of variables
#'
#' @description The diagnose() produces information for diagnosing
#' the quality of the variables of data.frame or tbl_df.
#'
#' @details The scope of data quality diagnosis is information on missing values
#' and unique value information. Data quality diagnosis can determine variables
#' that require missing value processing. Also, the unique value information can
#' determine the variable to be removed from the data analysis.
#'
#' @section Diagnostic information:
#' The information derived from the data diagnosis is as follows.:
#'
#' \itemize{
#' \item variables : variable names
#' \item types : data type of the variable
#' or to select a variable to be corrected or removed through data diagnosis.
#'   \itemize{
#'     \item integer, numeric, factor, ordered, character, etc.
#'   }
#' \item missing_count : number of missing values
#' \item missing_percent : percentage of missing values
#' \item unique_count : number of unique values
#' \item unique_rate : ratio of unique values. unique_count / number of observation
#' }
#'
#' See vignette("diagonosis") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' @return An object of tbl_df.
#' @seealso \code{\link{diagnose.tbl_dbi}}, \code{\link{diagnose_category.data.frame}}, \code{\link{diagnose_numeric.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Diagnosis of all variables
#' diagnose(jobchange)
#' 
#' # Select the variable to diagnose
#' diagnose(jobchange, gender, experience, training_hours)
#' diagnose(jobchange, -gender, -experience, -training_hours)
#' diagnose(jobchange, "gender", "experience", "training_hours")
#' diagnose(jobchange, 4, 9, 13)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#' 
#' # Diagnosis of all variables
#' jobchange %>%
#'   diagnose()
#' # Positive values select variables
#' jobchange %>%
#'   diagnose(gender, experience, training_hours)
#' # Negative values to drop variables
#' jobchange %>%
#'   diagnose(-gender, -experience, -training_hours)
#' # Positions values select variables
#' jobchange %>%
#'   diagnose(4, 9, 13)
#' # Negative values to drop variables
#' jobchange %>%
#'   diagnose(-8, -9, -10)
#'   
#' # Using pipes & dplyr -------------------------
#' # Diagnosis of missing variables
#' jobchange %>%
#'   diagnose() %>%
#'   filter(missing_count > 0)
#' }
#'    
#' @method diagnose data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
diagnose.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagn_std_impl(.data, vars)
}

#' @import tibble
#' @importFrom methods is
#' @importFrom stats complete.cases
diagn_std_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)

  variable_type <- sapply(vars,
    function(x) is(df[, x][[1]])[1])
  missing_count <- sapply(vars,
    function(x) sum(!complete.cases(df[, x])))
  unique_count <- sapply(vars,
    function(x) n_distinct(df[, x]))
  data_count <- nrow(df)

  tibble(variables = vars, types = variable_type,
    missing_count = missing_count,
    missing_percent = missing_count / data_count * 100,
    unique_count = unique_count,
    unique_rate = unique_count / data_count)
}


#' @rdname diagnose_category.data.frame
#' @export
diagnose_category <- function(.data, ...) {
  UseMethod("diagnose_category", .data)
}


#' Diagnose data quality of categorical variables
#'
#' @description The diagnose_category() produces information for
#' diagnosing the quality of the variables of data.frame or tbl_df.
#'
#' @details The scope of the diagnosis is the occupancy status of the levels
#' in categorical data. If a certain level of occupancy is close to 100%,
#' then the removal of this variable in the forecast model will have to be
#' considered. Also, if the occupancy of all levels is close to 0%, this
#' variable is likely to be an identifier.
#'
#' @section Categorical diagnostic information:
#' The information derived from the categorical data diagnosis is as follows.
#'
#' \itemize{
#' \item variables : variable names
#' \item levels: level names
#' \item N : number of observation
#' \item freq : number of observation at the levels
#' \item ratio : percentage of observation at the levels
#' \item rank : rank of occupancy ratio of levels
#' }
#'
#' See vignette("diagonosis") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_category() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#'
#' @param top an integer. Specifies the upper top rows or rank to extract.
#' Default is 10.
#' @param type a character string specifying how result are extracted.
#' "rank" that extract top n ranks by decreasing frequency. 
#' In this case, if there are ties in rank, more rows than the number specified 
#' by the top argument are returned.
#' Default is "n" extract only top n rows by decreasing frequency. 
#' If there are too many rows to be returned because there are too many ties, 
#' you can adjust the returned rows appropriately by using "n".
#' @param add_character logical. Decide whether to include text variables in the
#' diagnosis of categorical data. The default value is TRUE, which also includes character variables.
#' @param add_date ogical. Decide whether to include Date and POSIXct variables in the
#' diagnosis of categorical data. The default value is TRUE, which also includes character variables.
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose.data.frame}}, \code{\link{diagnose_numeric.data.frame}}, \code{\link{diagnose_outlier.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Diagnosis of categorical variables
#' diagnose_category(jobchange)
#' 
#' # Select the variable to diagnose
#' # diagnose_category(jobchange, education_level, company_type)
#' # diagnose_category(jobchange, -education_level, -company_type)
#' # diagnose_category(jobchange, "education_level", "company_type")
#' # diagnose_category(jobchange, 7)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#' 
#' # Diagnosis of all categorical variables
#' jobchange %>%
#'   diagnose_category()
#'
#' # Positive values select variables
#' jobchange %>%
#'  diagnose_category(company_type, job_chnge)
#'  
#' # Negative values to drop variables
#' jobchange %>%
#'   diagnose_category(-company_type, -job_chnge)
#'   
#' # Positions values select variables
#' jobchange %>%
#'   diagnose_category(7)
#'   
#' # Negative values to drop variables
#' jobchange %>%
#'   diagnose_category(-7)
#'   
#' # Top rank levels with top argument
#' jobchange %>%
#'   diagnose_category(top = 2)
#'   
#' # Using pipes & dplyr -------------------------
#' # Extraction of level that is more than 60% of categorical data
#' jobchange %>%
#'   diagnose_category()  %>%
#'   filter(ratio >= 60)
#'
#' # All observations of enrollee_id have a rank of 1. 
#' # Because it is a unique identifier. Therefore, if you select up to the top rank 3, 
#' # all records are displayed. It will probably fill your screen.
#' 
#' # extract rows that less than equal rank 3
#' # default of type argument is "n"
#' jobchange %>% 
#'   diagnose_category(enrollee_id, top = 3)
#'
#' # extract rows that less than equal rank 3
#' jobchange %>% 
#'   diagnose_category(enrollee_id, top = 3, type = "rank")
#'  
#' # extract only 3 rows
#' jobchange %>% 
#'   diagnose_category(enrollee_id, top = 3, type = "n")
#' }
#'   
#' @method diagnose_category data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
diagnose_category.data.frame <- function(.data, ..., top = 10, type = c("rank", "n")[2], 
                                         add_character = TRUE, add_date = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagn_category_impl(.data, vars, top, type, add_character, add_date)
}

diagn_category_impl <- function(df, vars, top, type, add_character, add_date) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  if (add_date & add_character)
    idx_factor <- find_class(df[, vars], type = "date_categorical2")  
  else if (add_character & !add_date)
    idx_factor <- find_class(df[, vars], type = "categorical2")
  else if (!add_character & add_date)
    idx_factor <- find_class(df[, vars], type = "date_categorical")
  else
    idx_factor <- find_class(df[, vars], type = "categorical")
  
  if (length(type) != 1 | !type %in% c("rank", "n")) {
    message("The type argument must be one of \"rank\" or \"n\".\n")
    return(NULL)    
  }
  
  if (length(idx_factor) == 0) {
    message("There is no categorical variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_topn <- function(df, var, top, type) {
    tab <- df %>%
      select(variable = var) %>%
      mutate(variable = as.character(variable)) %>% 
      count(variable, sort = TRUE) %>%
      transmute(variables = var, levels = variable, N = sum(n), freq = n,
                ratio = n / sum(n) * 100, 
                rank = rank(max(freq) - freq, ties.method = "min"))
    
    if (type == "n") {
      tab %>% 
        slice_head(n = top)
    } else if (type == "rank") {
      tab %>% 
        top_n(n = top, freq)      
    }
  }

  result <- lapply(vars[idx_factor],
                   function(x) get_topn(df, x, top, type))
  suppressWarnings(
    do.call("rbind", result) %>% 
      tibble::as_tibble()
  )
}


#' @rdname diagnose_numeric.data.frame
#' @export
diagnose_numeric <- function(.data, ...) {
  UseMethod("diagnose_numeric")
}


#' Diagnose data quality of numerical variables
#'
#' @description The diagnose_numeric() produces information
#' for diagnosing the quality of the numerical data.
#'
#' @details The scope of the diagnosis is the calculate a statistic that can be
#' used to understand the distribution of numerical data.
#' min, Q1, mean, median, Q3, max can be used to estimate the distribution
#' of data. If the number of zero or minus is large, it is necessary to suspect
#' the error of the data. If the number of outliers is large, a strategy of
#' eliminating or replacing outliers is needed.
#'
#' @section Numerical diagnostic information:
#' The information derived from the numerical data diagnosis is as follows.
#'
#' \itemize{
#' \item variables : variable names
#' \item min : minimum
#' \item Q1 : 25 percentile
#' \item mean : arithmetic average
#' \item median : median. 50 percentile
#' \item Q3 : 75 percentile
#' \item max : maximum
#' \item zero : count of zero values
#' \item minus : count of minus values
#' \item outlier : count of outliers
#' }
#'
#' See vignette("diagonosis") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_numeric() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_numeric.tbl_dbi}}, \code{\link{diagnose.data.frame}}, \code{\link{diagnose_category.data.frame}}, \code{\link{diagnose_outlier.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Diagnosis of numerical variables
#' diagnose_numeric(heartfailure)
#' 
#' # Select the variable to diagnose
#' diagnose_numeric(heartfailure, cpk_enzyme, sodium)
#' diagnose_numeric(heartfailure, -cpk_enzyme, -sodium)
#' diagnose_numeric(heartfailure, "cpk_enzyme", "sodium")
#' diagnose_numeric(heartfailure, 5)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#' 
#' # Diagnosis of all numerical variables
#' heartfailure %>%
#'   diagnose_numeric()
#' # Positive values select variables
#' heartfailure %>%
#'   diagnose_numeric(cpk_enzyme, sodium)
#' # Negative values to drop variables
#' heartfailure %>%
#'   diagnose_numeric(-cpk_enzyme, -sodium)
#' # Positions values select variables
#' heartfailure %>%
#'   diagnose_numeric(5)
#' # Negative values to drop variables
#' heartfailure %>%
#'   diagnose_numeric(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # List of variables containing outliers
#' heartfailure %>%
#'   diagnose_numeric()  %>%
#'   filter(outlier > 0)
#' }
#' 
#' @method diagnose_numeric data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
diagnose_numeric.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagn_numeric_impl(.data, vars)
}

#' @importFrom stats median quantile
diagn_numeric_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  if (length(idx_numeric) == 0) {
    message("There is no numeric variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_descr <- function(df, var) {
    df %>%
      select(variable = var) %>%
      summarise(min = min(variable, na.rm = TRUE),
        Q1 = quantile(variable, 0.25, na.rm = TRUE),
        mean = mean(variable, na.rm = TRUE),
        median = median(variable, na.rm = TRUE),
        Q3 = quantile(variable, 0.75, na.rm = TRUE),
        max = max(variable, na.rm = TRUE),
        zero = sum(variable == 0, na.rm = TRUE),
        minus = sum(variable < 0, na.rm = TRUE),
        outlier = length(boxplot.stats(variable)$out)) %>%
      transmute(variables = var, min, Q1, mean, median, Q3, max,
        zero, minus, outlier)
  }

  result <- lapply(vars[idx_numeric],
    function(x) get_descr(df, x))
  
  suppressWarnings(
    as.tbl(do.call("rbind", result))
  )  
}


#' @rdname diagnose_outlier.data.frame
#' @export
diagnose_outlier <- function(.data, ...) {
  UseMethod("diagnose_outlier", .data)
}


#' Diagnose outlier of numerical variables
#'
#' @description The diagnose_outlier() produces outlier information
#' for diagnosing the quality of the numerical data.
#'
#' @details The scope of the diagnosis is the provide a outlier information.
#' If the number of outliers is small and the difference between the averages
#' including outliers and the averages not including them is large,
#' it is necessary to eliminate or replace the outliers.
#'
#' @section Outlier Diagnostic information:
#' The information derived from the numerical data diagnosis is as follows.
#'
#' \itemize{
#' \item variables : variable names
#' \item outliers_cnt : number of outliers
#' \item outliers_ratio : percent of outliers
#' \item outliers_mean : arithmetic average of outliers
#' \item with_mean : arithmetic average of with outliers
#' \item without_mean : arithmetic average of without outliers
#' }
#'
#' See vignette("diagonosis") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_outlier() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context
#' where column names represent column positions.
#' They support unquoting and splicing.
#'
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_outlier.tbl_dbi}}, \code{\link{diagnose.data.frame}}, \code{\link{diagnose_category.data.frame}}, \code{\link{diagnose_numeric.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Diagnosis of numerical variables
#' diagnose_outlier(heartfailure)
#' 
#' # Select the variable to diagnose
#' diagnose_outlier(heartfailure, cpk_enzyme, sodium)
#' diagnose_outlier(heartfailure, -cpk_enzyme, -sodium)
#' diagnose_outlier(heartfailure, "cpk_enzyme", "sodium")
#' diagnose_outlier(heartfailure, 5)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#' 
#' # Diagnosis of all numerical variables
#' heartfailure %>%
#'   diagnose_outlier()
#' # Positive values select variables
#' heartfailure %>%
#'   diagnose_outlier(cpk_enzyme, sodium)
#' # Negative values to drop variables
#' heartfailure %>%
#'   diagnose_outlier(-cpk_enzyme, -sodium)
#' # Positions values select variables
#' heartfailure %>%
#'   diagnose_outlier(5)
#' # Negative values to drop variables
#' heartfailure %>%
#'   diagnose_outlier(-1, -5)
#' 
#' # Using pipes & dplyr -------------------------
#' # outlier_ratio is more than 1%
#' heartfailure %>%
#'   diagnose_outlier()  %>%
#'   filter(outliers_ratio > 1)
#' }
#' 
#' @method diagnose_outlier data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
diagnose_outlier.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagnose_outlier_impl(.data, vars)
}

#' @import dplyr
diagnose_outlier_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  if (length(idx_numeric) == 0) {
    message("There is no numeric variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_outlier <- function(df, var) {
    df %>%
      select(variable = var) %>%
      summarise(outliers_cnt = length(boxplot.stats(variable)$out),
        outliers_ratio = length(boxplot.stats(variable)$out) / n(),
        outliers_mean = mean(ifelse(variable %in% boxplot.stats(variable)$out,
          variable, NA), na.rm = TRUE),
        with_mean = mean(variable, na.rm = TRUE),
        without_mean = mean(ifelse(variable %in% boxplot.stats(variable)$out,
          NA, variable), na.rm = TRUE)) %>%
      transmute(variables = var, outliers_cnt,
        outliers_ratio = outliers_ratio * 100,
        outliers_mean, with_mean, without_mean)
  }

  result <- lapply(vars[idx_numeric],
    function(x) get_outlier(df, x))
  
  suppressWarnings(
    do.call("rbind", result)
  )  
}


#' @rdname plot_outlier.data.frame
#' @export
plot_outlier <- function(.data, ...) {
  UseMethod("plot_outlier", .data)
}


#' Plot outlier information of numerical data diagnosis
#'
#' @description The plot_outlier() visualize outlier information
#' for diagnosing the quality of the numerical data.
#'
#' @details The scope of the diagnosis is the provide a outlier information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#'
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @section Outlier diagnostic information:
#' The plot derived from the numerical data diagnosis is as follows.
#'
#' \itemize{
#' \item With outliers box plot
#' \item Without outliers box plot
#' \item With outliers histogram
#' \item Without outliers histogram
#' }
#'
#' See vignette("diagonosis") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_outlier() will automatically start
#' with all variables.
#' These arguments are automatically quoted and evaluated in a context
#' where column names represent column positions.
#' They support unquoting and splicing.
#' @param col a color to be used to fill the bars. The default is "steelblue".
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @seealso \code{\link{plot_outlier.tbl_dbi}}, \code{\link{diagnose_outlier.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Visualization of all numerical variables
#' plot_outlier(heartfailure)
#' 
#' # Select the variable to diagnose
#' plot_outlier(heartfailure, cpk_enzyme, sodium)
#' plot_outlier(heartfailure, -cpk_enzyme, -sodium)
#' plot_outlier(heartfailure, "cpk_enzyme", "sodium")
#' plot_outlier(heartfailure, 7)
#' 
#' # Using the col argument
#' plot_outlier(heartfailure, cpk_enzyme, col = "gray")
#' 
#' # Not allow typographic argument
#' plot_outlier(heartfailure, cpk_enzyme, typographic = FALSE)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#' 
#' # Visualization of all numerical variables
#' heartfailure %>%
#'   plot_outlier()
#' 
#' # Positive values select variables
#' heartfailure %>%
#'   plot_outlier(cpk_enzyme, sodium)
#'   
#' # Negative values to drop variables
#' heartfailure %>%
#'   plot_outlier(-cpk_enzyme, -sodium)
#' 
#' # Positions values select variables
#' heartfailure %>%
#'   plot_outlier(7)
#' 
#' # Negative values to drop variables
#' heartfailure %>%
#'   plot_outlier(-1, -5)
#' 
#' # Using pipes & dplyr -------------------------
#' # Visualization of numerical variables with a ratio of
#' # outliers greater than 5%
#' heartfailure %>%
#'   plot_outlier(heartfailure %>%
#'     diagnose_outlier() %>%
#'     filter(outliers_ratio > 5) %>%
#'     select(variables) %>%
#'     pull())
#' }
#' 
#' @method plot_outlier data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
plot_outlier.data.frame <- function(.data, ..., col = "steelblue", 
                                    typographic = TRUE, base_family = NULL) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  plot_outlier_impl(.data, vars, col, typographic, base_family)
}

#' @importFrom graphics boxplot hist title par
plot_outlier_impl <- function(df, vars, col = "steelblue", typographic = TRUE,
                              base_family = NULL) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) 
    df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  plot_outliers <- function(df, var, col, typographic, base_family) {
    x <- dplyr::pull(df, var)
    main <- sprintf("Outlier Diagnosis Plot (%s)", var)
    
    plot_outlier_raw(x, main, col, typographic, base_family)
  }

  if (length(idx_numeric) == 0) {
    message("There is no numeric variable in the data or variable list.\n")
    invisible(NULL)
  } else if (length(idx_numeric) == 1 & all(is.na(df[, vars]))) {
    message("All observed values for numeric variables are NA.\n")
    invisible(NULL)
  } else {
    idx_na <- sapply(vars[idx_numeric],
                     function(x) all(is.na(df[, x])))
    if (sum(idx_na) > 0) {
      name_null <- paste(vars[idx_numeric][idx_na], collapse = ",")
      message(sprintf("All observations for the numerical variable %s are NA.", name_null))
    }
    
    tmp <- lapply(vars[idx_numeric][!idx_na],
                  function(x) plot_outliers(df, x, col, typographic, base_family))
  }
}

#' @import ggplot2
#' @import hrbrthemes
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
plot_outlier_raw <- function(x, main = NULL, col = "steelblue", 
                             typographic = TRUE, base_family = NULL) {
  main <- ifelse(is.null(main), "Outlier Diagnose Plot", main)
  
  df_all <- data.frame(x = x) %>% 
    filter(!is.na(x))
  
  df_out <- data.frame(x = x) %>% 
    filter(!is.na(x)) %>% 
    filter(!x  %in% boxplot.stats(x)$out)
  
  # calculate number of bins using Sturges' formula
  n_bins_all <- round(log2(nrow(df_all)) + 1)
  n_bins_out <- round(log2(nrow(df_out)) + 1)
  
  top_left <- df_all %>% 
    ggplot(aes(y = x)) +
    geom_boxplot(fill = col, color = "black", alpha = 0.8) +
    xlim(-0.7, 0.7) + 
    labs(title = "With outliers", x = "", y = "") +
    theme_grey(base_family = base_family) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  bottom_left <- df_out %>% 
    ggplot(aes(y = x)) +
    geom_boxplot(fill = col, color = "black", alpha = 0.8) +
    xlim(-0.7, 0.7) + 
    labs(title = "Without outliers", x = "", y = "") +
    theme_grey(base_family = base_family) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  top_right <- df_all %>% 
    ggplot(aes(x)) +
    geom_histogram(fill = col, color = "black", alpha = 0.8, bins = n_bins_all) +
    labs(title = "With outliers", x = "", y = "") +
    theme_grey(base_family = base_family)
  
  bottom_right <- df_out %>% 
    ggplot(aes(x)) +
    geom_histogram(fill = col, color = "black", alpha = 0.8, bins = n_bins_out) +
    labs(title = "Without outliers", x = "", y = "") +
    theme_grey(base_family = base_family)
  
  if (typographic) {
    top_left <- top_left +
      theme_typographic(base_family) +
      theme(plot.title = element_text(size = 15, face = "plain"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = margin(10, 30, 10, 30))
    
    top_right <- top_right +
      theme_typographic(base_family) +
      theme(plot.title = element_text(size = 15, face = "plain"),
            plot.margin = margin(10, 30, 10, 30))
    
    bottom_left <- bottom_left +
      theme_typographic(base_family) +
      theme(plot.title = element_text(size = 15, face = "plain"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = margin(10, 30, 10, 30))
    
    bottom_right <- bottom_right +
      theme_typographic(base_family) +
      theme(plot.title = element_text(size = 15, face = "plain"),
            plot.margin = margin(10, 30, 10, 30))    
  } 
  
  if (is.null(base_family)) {
    base_family <- "Roboto Condensed"     
  }
  
  top <- grid::textGrob(main, gp = grid::gpar(fontfamily = base_family, 
                                              fontsize = 18, font = 2),
                        x = unit(0.075, "npc"), just = "left")    
  
  suppressWarnings(gridExtra::grid.arrange(top_left, top_right, bottom_left, bottom_right, 
                                           ncol = 2, nrow = 2, widths = c(2, 3), top = top))
}


#' Plot outlier information of target_df 
#'
#' @description The plot_outlier() visualize outlier information
#' for diagnosing the quality of the numerical data with target_df class.
#'
#' @details The scope of the diagnosis is the provide a outlier information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#'
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @section Outlier diagnostic information:
#' The plot derived from the numerical data diagnosis is as follows.
#'
#' \itemize{
#' \item With outliers box plot by target variable
#' \item Without outliers box plot by target variable
#' \item With outliers density plot by target variable
#' \item Without outliers density plot by target variable
#' }
#'
#' @param .data a target_df. reference \code{\link{target_by}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_outlier() will automatically start
#' with all variables.
#' These arguments are automatically quoted and evaluated in a context
#' where column names represent column positions.
#' They support unquoting and splicing.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @seealso \code{\link{plot_outlier.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#' 
#' plot_outlier(categ, sodium)
#' # plot_outlier(categ, sodium, typographic = FALSE)
#' 
#' # death_eventing dplyr
#' library(dplyr)
#' heartfailure %>% 
#'   target_by(death_event) %>% 
#'   plot_outlier(sodium, cpk_enzyme)
#' 
#' # death_eventing DBMS tables ----------------------------------
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#' 
#' # If the target variable is a categorical variable
#' categ <- target_by(con_sqlite %>% tbl("TB_HEARTFAILURE") , death_event)
#' 
#' plot_outlier(categ, sodium)
#' }
#' 
#' @method plot_outlier target_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
plot_outlier.target_df <- function(.data, ..., typographic = TRUE, base_family = NULL) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  plot_outlier_target_impl(.data, vars, typographic, base_family)
}

#' @import dplyr
#' @import ggplot2
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
plot_outlier_target_impl <- function(df, vars, typographic = TRUE, base_family = NULL) {
  if (utils::packageVersion("dplyr") >= "0.8.0") {
    target <- setdiff(attr(df, "groups") %>% names(), ".rows")
  } else {
    target <- attr(df, "vars")
  }
  
  if (length(target) > 1) {
    message(sprintf("plot_outlier() only supports one group variable. \
                   However, the call now has %d group variables.", length(target)))
    invisible(NULL)
  }
  
  if (utils::packageVersion("dplyr") >= "0.8.0") {
    type_target <- df[, target] %>% pull %>% is %>% "["(1)
  } else {
    type_target <- is(df[, target][[1]])[1]
  } 
  
  if (!type_target %in% c("ordered", "factor", "character")) {
    message("target variabe is not in  (\"ordered\", \"factor\", \"character\")")
    invisible(NULL)
  }
  
  if (length(vars) == 0) {
    vars <- names(df)
  }
  
  vars <- setdiff(vars, target)
  
  if (length(vars) == 0) {
    message("There is no variable in variable list or target and variable are the same.\n")
    invisible(NULL)
  }
  
  plot_outliers <- function(df, target, predictor, typographic = TRUE, base_family = NULL) {
    data_with <- df %>% 
      ungroup() %>% 
      select(target, predictor) %>% 
      filter(!is.na(target))
    
    box_with <- ggplot(data_with, aes(x = !!sym(target), y = !!sym(predictor), fill = !!sym(target))) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "boxplot with outliers") +
      theme_grey(base_family = base_family) +
      theme(legend.position = "none")
    
    density_with <- ggplot(data_with, aes(x = !!sym(predictor), colour = !!sym(target))) +
      geom_density() +
      labs(title = "density with outliers") +
      theme_grey(base_family = base_family)
    
    flag <- !data_with[, predictor] %>% pull %in% boxplot.stats(data_with[, predictor] %>% pull)$out
    data_without <- data_with[flag, ]
    
    box_without <- ggplot(data_without, aes(x = !!sym(target), y = !!sym(predictor), fill = !!sym(target))) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "boxplot without outliers") +
      theme_grey(base_family = base_family) +
      theme(legend.position = "none")
    
    density_without <- ggplot(data_without, aes(x = !!sym(predictor), colour = !!sym(target))) +
      geom_density() +
      labs(title = "density with outliers") +
      theme_grey(base_family = base_family)
    
    if (typographic) {
      box_with <- box_with +
        theme_typographic(base_family) +
        scale_fill_ipsum() + 
        theme(legend.position = "none",
              plot.title = element_text(size = 15),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),              
              plot.margin = margin(10, 30, 10, 10))
      
      density_with <- density_with +
        theme_typographic(base_family) +
        scale_color_ipsum() +
        theme(plot.title = element_text(size = 15),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),              
              plot.margin = margin(10, 30, 10, 10))
      
      box_without <- box_without +
        theme_typographic(base_family) +
        scale_fill_ipsum() + 
        theme(legend.position = "none",
              plot.title = element_text(size = 15),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),              
              plot.margin = margin(10, 30, 10, 10))
      
      density_without <- density_without +
        theme_typographic(base_family) +
        scale_color_ipsum() +
        theme(plot.title = element_text(size = 15),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),              
              plot.margin = margin(10, 30, 10, 10))    
    }
    
    suppressWarnings(gridExtra::grid.arrange(box_with, density_with, box_without, density_without, 
                                             nrow = 2, ncol = 2))
  }
  
  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  if (length(idx_numeric) == 0) {
    message("There is no numeric variable in the data or variable list.\n")
    invisible(NULL)
  } else if (length(idx_numeric) == 1 & all(is.na(df[, vars]))) {
    message("All observed values for numeric variables are NA.\n")
    invisible(NULL)
  } else {
    idx_na <- sapply(vars[idx_numeric],
                     function(x) all(is.na(df[, x])))
    if (sum(idx_na) > 0) {
      name_null <- paste(vars[idx_numeric][idx_na], collapse = ",")
      message(sprintf("All observations for the numerical variable %s are NA.", name_null))
    }
    
    tmp <- lapply(vars[idx_numeric][!idx_na],
                  function(x) plot_outliers(df, target, x, typographic, base_family))
  }
}


#' @rdname diagnose_report.data.frame
#' @export
diagnose_report <- function(.data, output_format, output_file, output_dir, ...) {
  .Deprecated("diagnose_web_report", msg = "'diagnose_report' is deprecated. \nUse 'diagnose_web_report' and 'diagnose_paged_report' instead.\nSee help(\"Deprecated\")")
  UseMethod("diagnose_report", .data)
}


#' Reporting the information of data diagnosis
#'
#' @description The diagnose_report() report the information for diagnosing
#' the quality of the data.
#'
#' @details Generate generalized data diagnostic reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for diagnosing a data frame with a large number of variables
#' than data with a small number of variables.
#' For pdf output, Korean Gothic font must be installed in Korean operating system.
#'
#' @section Reported information:
#' Reported from the data diagnosis is as follows.
#'
#' \itemize{
#'   \item Diagnose Data
#'   \itemize{
#'     \item Overview of Diagnosis
#'     \itemize{
#'       \item List of all variables quality
#'       \item Diagnosis of missing data
#'       \item Diagnosis of unique data(Text and Category)
#'       \item Diagnosis of unique data(Numerical)
#'     }
#'     \item Detailed data diagnosis
#'     \itemize{
#'       \item Diagnosis of categorical variables
#'       \item Diagnosis of numerical variables
#'       \item List of numerical diagnosis (zero)
#'       \item List of numerical diagnosis (minus)
#'     }
#'   }
#'   \item Diagnose Outliers
#'   \itemize{
#'     \item Overview of Diagnosis
#'     \itemize{
#'       \item Diagnosis of numerical variable outliers
#'       \item Detailed outliers diagnosis
#'     }
#'   }
#' }
#'
#' See vignette("diagonosis") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param font_family character. font family name for figure in pdf.
#' @param browse logical. choose whether to output the report results to the browser.
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \donttest{
#' if (FALSE) {
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is DataDiagnosis_Report.pdf
#' diagnose_report(heartfailure)
#' 
#' # create pdf file. file name is Diagn.pdf
#' diagnose_report(heartfailure, output_file = "Diagn.pdf")
#' 
#' # create pdf file. file name is ./Diagn.pdf and not browse
#' diagnose_report(heartfailure, output_dir = ".", output_file = "Diagn.pdf", 
#'   browse = FALSE)
#' 
#' # create html file. file name is Diagnosis_Report.html
#' diagnose_report(heartfailure, output_format = "html")
#' 
#' # create html file. file name is Diagn.html
#' diagnose_report(heartfailure, output_format = "html", output_file = "Diagn.html")
#' }
#' }
#' 
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom kableExtra kable_styling
#' @importFrom utils browseURL
#' @method diagnose_report data.frame
#' @export
diagnose_report.data.frame <- function(.data, output_format = c("pdf", "html"),
  output_file = NULL, output_dir = tempdir(), font_family = NULL, browse = TRUE, ...) {
  output_format <- match.arg(output_format)
  
  assign("edaData", as.data.frame(.data), .dlookrEnv)
  
  path <- output_dir
  if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
    latex_main <- "DataDiagnosis_Report_KR.Rnw"
    latex_sub <- "01_Diagnose_KR.Rnw"
  } else {
    latex_main <- "DataDiagnosis_Report.Rnw"
    latex_sub <- "01_Diagnose.Rnw"
  }
  
  if (!is.null(font_family)) {
    ggplot2::theme_set(ggplot2::theme_gray(base_family = font_family))
    par(family = font_family)
  }
  
  if (output_format == "pdf") {
    installed <- file.exists(Sys.which("pdflatex"))
    
    if (!installed) {
      stop("No TeX installation detected. Please install TeX before running.\nor Use output_format = \"html\"")
    }
    
    if (is.null(output_file))
      output_file <- "DataDiagnosis_Report.pdf"
    
    Rnw_file <- file.path(system.file(package = "dlookr"),
      "report", latex_main)
    file.copy(from = Rnw_file, to = path)
    
    Rnw_file <- file.path(system.file(package = "dlookr"),
      "report", latex_sub)
    file.copy(from = Rnw_file, to = path)
    
    Img_file <- file.path(system.file(package = "dlookr"), "img")
    file.copy(from = Img_file, to = path, recursive = TRUE)
    
    dir.create(paste(path, "figure", sep = "/"))
    
    # you needs tinytex package for compiler = "pdflatex"
    knitr::knit2pdf(paste(path, latex_main, sep = "/"),
      compiler = "pdflatex",
      output = sub("pdf$", "tex", paste(path, output_file, sep = "/")))
      
    file.remove(paste(path, latex_sub, sep = "/"))
    file.remove(paste(path, latex_main, sep = "/"))
      
    fnames <- sub("pdf$", "", output_file)
    fnames <- grep(fnames, list.files(path), value = TRUE)
    fnames <- grep("\\.pdf$", fnames, invert = TRUE, value = TRUE)
      
    file.remove(paste(path, fnames, sep = "/"))
      
    unlink(paste(path, "figure", sep = "/"), recursive = TRUE)
    unlink(paste(path, "img", sep = "/"), recursive = TRUE)
  } else if (output_format == "html") {
    if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
      rmd <- "Diagnosis_Report_KR.Rmd"
    } else {
      rmd <- "Diagnosis_Report.Rmd"
    }
    
    if (is.null(output_file))
      output_file <- "Diagnosis_Report.html"
    
    Rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
    file.copy(from = Rmd_file, to = path, recursive = TRUE)
    
    if (!requireNamespace("forecast", quietly = TRUE)) {
      stop("Package \"forecast\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    rmarkdown::render(paste(path, rmd, sep = "/"),
      output_format = prettydoc::html_pretty(toc = TRUE, number_sections = TRUE),
      output_file = paste(path, output_file, sep = "/"))
    
    file.remove(paste(path, rmd, sep = "/"))
  }
  
  if (browse & file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}