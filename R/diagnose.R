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
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Diagnosis of all variables
#' diagnose(carseats)
#'
#' # Select the variable to diagnose
#' diagnose(carseats, Sales, Income, Age)
#' diagnose(carseats, -Sales, -Income, -Age)
#' diagnose(carseats, "Sales", "Income", "Age")
#' diagnose(carseats, 1, 3, 8)
#'
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Diagnosis of all variables
#' carseats %>%
#'   diagnose()
#' # Positive values select variables
#' carseats %>%
#'   diagnose(Sales, Income, Age)
#' # Negative values to drop variables
#' carseats %>%
#'   diagnose(-Sales, -Income, -Age)
#' # Positions values select variables
#' carseats %>%
#'   diagnose(1, 3, 8)
#' # Positions values select variables
#' carseats %>%
#'   diagnose(-8, -9, -10)
#'
#' # Using pipes & dplyr -------------------------
#' # Diagnosis of missing variables
#' carseats %>%
#'   diagnose() %>%
#'   filter(missing_count > 0)
#' @method diagnose data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom methods is
#' @export
diagnose.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagn_std_impl(.data, vars)
}

#' @import tibble
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
#' @param top an integer. Specifies the upper top rank to extract.
#' Default is 10.
#' @param add_character logical. Decide whether to include text variables in the
#' diagnosis of categorical data. The default value is TRUE, which also includes character variables.
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose.data.frame}}, \code{\link{diagnose_numeric.data.frame}}, \code{\link{diagnose_outlier.data.frame}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Diagnosis of categorical variables
#' diagnose_category(carseats)
#'
#' # Select the variable to diagnose
#' diagnose_category(carseats, ShelveLoc, Urban)
#' diagnose_category(carseats, -ShelveLoc, -Urban)
#' diagnose_category(carseats, "ShelveLoc", "Urban")
#' diagnose_category(carseats, 7)
#'
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Diagnosis of all categorical variables
#' carseats %>%
#'   diagnose_category()
#' # Positive values select variables
#' carseats %>%
#'   diagnose_category(Urban, US)
#' # Negative values to drop variables
#' carseats %>%
#'   diagnose_category(-Urban, -US)
#' # Positions values select variables
#' carseats %>%
#'   diagnose_category(7)
#' # Positions values select variables
#' carseats %>%
#'   diagnose_category(-7)
#' # Top rank levels with top argument
#' carseats %>%
#'   diagnose_category(top = 2)
#'
#' # Using pipes & dplyr -------------------------
#' # Extraction of level that is more than 60% of categorical data
#' carseats %>%
#'   diagnose_category()  %>%
#'   filter(ratio >= 60)
#' @method diagnose_category data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
diagnose_category.data.frame <- function(.data, ..., top = 10, add_character = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagn_category_impl(.data, vars, top, add_character)
}

diagn_category_impl <- function(df, vars, top, add_character) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  if (add_character)
    idx_factor <- find_class(df[, vars], type = "categorical2")
  else
    idx_factor <- find_class(df[, vars], type = "categorical")

  if (length(idx_factor) == 0) {
    message("There is no categorical variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_topn <- function(df, var, top) {
    df %>%
      select(variable = var) %>%
      count(variable, sort = TRUE) %>%
      transmute(variables = var, levels = variable, N = sum(n), freq = n,
                ratio = n / sum(n) * 100, rank = row_number()) %>%
      top_n(freq, n = top)
  }

  result <- lapply(vars[idx_factor],
                   function(x) get_topn(df, x, top))
  suppressWarnings(
    do.call("rbind", result)
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
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Diagnosis of numerical variables
#' diagnose_numeric(carseats)
#'
#' # Select the variable to diagnose
#' diagnose_numeric(carseats, Sales, Income)
#' diagnose_numeric(carseats, -Sales, -Income)
#' diagnose_numeric(carseats, "Sales", "Income")
#' diagnose_numeric(carseats, 5)
#'
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Diagnosis of all numerical variables
#' carseats %>%
#'   diagnose_numeric()
#' # Positive values select variables
#' carseats %>%
#'   diagnose_numeric(Sales, Income)
#' # Negative values to drop variables
#' carseats %>%
#'   diagnose_numeric(-Sales, -Income)
#' # Positions values select variables
#' carseats %>%
#'   diagnose_numeric(5)
#' # Positions values select variables
#' carseats %>%
#'   diagnose_numeric(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # Information records of zero variable more than 0
#' carseats %>%
#'   diagnose_numeric()  %>%
#'   filter(zero > 0)
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
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Diagnosis of numerical variables
#' diagnose_outlier(carseats)
#'
#' # Select the variable to diagnose
#' diagnose_outlier(carseats, Sales, Income)
#' diagnose_outlier(carseats, -Sales, -Income)
#' diagnose_outlier(carseats, "Sales", "Income")
#' diagnose_outlier(carseats, 5)
#'
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Diagnosis of all numerical variables
#' carseats %>%
#'   diagnose_outlier()
#' # Positive values select variables
#' carseats %>%
#'   diagnose_outlier(Sales, Income)
#' # Negative values to drop variables
#' carseats %>%
#'   diagnose_outlier(-Sales, -Income)
#' # Positions values select variables
#' carseats %>%
#'   diagnose_outlier(5)
#' # Positions values select variables
#' carseats %>%
#'   diagnose_outlier(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # outlier_ratio is more than 1%
#' carseats %>%
#'   diagnose_outlier()  %>%
#'   filter(outliers_ratio > 1)
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
#' @param col a color to be used to fill the bars. The default is "lightblue".
#'
#' @seealso \code{\link{plot_outlier.tbl_dbi}}, \code{\link{diagnose_outlier.data.frame}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Visualization of all numerical variables
#' plot_outlier(carseats)
#'
#' # Select the variable to diagnose
#' plot_outlier(carseats, Sales, Price)
#' plot_outlier(carseats, -Sales, -Price)
#' plot_outlier(carseats, "Sales", "Price")
#' plot_outlier(carseats, 6)
#'
#' # Using the col argument
#' plot_outlier(carseats, Sales, col = "gray")
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Visualization of all numerical variables
#' carseats %>%
#'   plot_outlier()
#' # Positive values select variables
#' carseats %>%
#'   plot_outlier(Sales, Price)
#' # Negative values to drop variables
#' carseats %>%
#'   plot_outlier(-Sales, -Price)
#' # Positions values select variables
#' carseats %>%
#'   plot_outlier(6)
#' # Positions values select variables
#' carseats %>%
#'   plot_outlier(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # Visualization of numerical variables with a ratio of
#' # outliers greater than 1%
#' carseats %>%
#'   plot_outlier(carseats %>%
#'       diagnose_outlier() %>%
#'       filter(outliers_ratio > 1) %>%
#'       select(variables) %>%
#'       pull())
#'   
#' @method plot_outlier data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
plot_outlier.data.frame <- function(.data, ..., col = "lightblue") {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  plot_outlier_impl(.data, vars, col)
}

#' @importFrom graphics boxplot hist title par
plot_outlier_impl <- function(df, vars, col = "lightblue") {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) 
    df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  plot_outliers <- function(df, var, col) {
    x <- dplyr::pull(df, var)
    
    op <- par(no.readonly = TRUE)
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(2, 4, 2, 2))
    on.exit(par(op))

    boxplot(x, main = "With outliers", col = col)
    hist(x, main = "With outliers", xlab = NA, ylab = NA, col = col)

    outlier <- boxplot.stats(x)$out
    x <- ifelse(x %in% outlier, NA, x)
    boxplot(x, main = "Without outliers", col = col)
    hist(x, main = "Without outliers", xlab = NA, ylab = NA, col = col)

    title(sprintf("Outlier Diagnosis Plot (%s)", var), outer = TRUE)
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
                  function(x) plot_outliers(df, x, col))
  }
}


#' @rdname diagnose_report.data.frame
#' @export
diagnose_report <- function(.data, output_format, output_file, output_dir, ...) {
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
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is DataDiagnosis_Report.pdf
#' diagnose_report(carseats)
#' # create pdf file. file name is Diagn.pdf
#' diagnose_report(carseats, output_file = "Diagn.pdf")
#' # create pdf file. file name is ./Diagn.pdf and not browse
#' # diagnose_report(carseats, output_dir = ".", output_file = "Diagn.pdf", 
#' #   browse = FALSE)
#' # create html file. file name is Diagnosis_Report.html
#' diagnose_report(carseats, output_format = "html")
#' # create html file. file name is Diagn.html
#' diagnose_report(carseats, output_format = "html", output_file = "Diagn.html")
#' }
#'
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom prettydoc html_pretty
#' @importFrom kableExtra kable_styling
#' @importFrom tinytex latexmk
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
    
    rmarkdown::render(paste(path, rmd, sep = "/"),
      output_format = prettydoc::html_pretty(toc = TRUE, number_sections = TRUE),
      output_file = paste(path, output_file, sep = "/"))
    
    file.remove(paste(path, rmd, sep = "/"))
  }
  
  if (browse & file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}
