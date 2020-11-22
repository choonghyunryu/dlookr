#' Describe column of table in the DBMS
#'
#' @description The get_column_info() retrieves the column information of the DBMS table through the tbl_bdi object of dplyr.
#'
#' @section Column information of the DBMS table:
#'
#' \itemize{
#'   \item SQLite DBMS connected RSQLite::SQLite(): 
#'   \itemize{
#'     \item name: column name
#'     \item type: data type in R
#'   }
#'   \item MySQL/MariaDB DBMS connected RMySQL::MySQL(): 
#'   \itemize{
#'     \item name: column name
#'     \item Sclass: data type in R
#'     \item type: data type of column in the DBMS
#'     \item length: data length in the DBMS   
#'   }
#'   \item Oracle DBMS connected ROracle::dbConnect(): 
#'   \itemize{
#'     \item name: column name
#'     \item Sclass: column type in R
#'     \item type: data type of column in the DBMS
#'     \item len: length of column(CHAR/VARCHAR/VARCHAR2 data type) in the DBMS
#'     \item precision: precision of column(NUMBER data type) in the DBMS
#'     \item scale: decimal places of column(NUMBER data type) in the DBMS
#'     \item nullOK: nullability
#'   }
#' }
#'
#' @param df a tbl_dbi.
#'
#' @return An object of data.frame.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#' 
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   get_column_info

get_column_info <- function(df) {
  res <- DBI::dbSendQuery(df$src$con, 
                          sprintf("select * from %s", df$ops$x))
  
  column_info <- DBI::dbColumnInfo(res)
  DBI::dbClearResult(res)
  
  column_info
}


#' Diagnose data quality of variables in the DBMS 
#'
#' @description The diagnose() produces information for diagnosing the quality of 
#' the column of the DBMS table through tbl_dbi.
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
#' \item variables : column names
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param in_database a logical. Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#'
#' @return An object of tbl_df.
#' @seealso \code{\link{diagnose.data.frame}}, \code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose_numeric.tbl_dbi}}.
#' @export
#' @import dplyr
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#' 
#' # Using pipes ---------------------------------
#' # Diagnosis of all columns
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose()
#'   
#' # Positive values select columns
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose(Sales, Income, Age)
#'   
#' # Negative values to drop columns
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose(-Sales, -Income, -Age)
#'   
#' # Positions values select columns, and In-memory mode
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose(1, 3, 8, in_database = FALSE)
#'   
#' # Positions values select columns, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose(-8, -9, -10, in_database = FALSE, collect_size = 200)
#'
#' # Using pipes & dplyr -------------------------
#' # Diagnosis of missing variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose() %>%
#'   filter(missing_count > 0)
#'   
diagnose.tbl_dbi <- function(.data, ..., in_database = TRUE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    diagn_std_impl_dbi(.data, vars)
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>% 
      diagn_std_impl(vars)
  }
}

diagn_std_impl_dbi <- function(df, vars) {
  if (length(vars) == 0) vars <- colnames(df)
  
  get_na_cnt <- function(df, x) {
    suppressWarnings(df %>%
                       select(variable = !!x) %>%
                       summarise(missing_count = sum(ifelse(is.na(variable), 1, 0))) %>%
                       pull())
  }
  
  get_unique_cnt <- function(df, x) {
    df %>%
      select(variable = !!x) %>%
      distinct(variable) %>%
      summarise(unique_count = n()) %>%
      pull()
  }
  
  col_info <- df %>%
    get_column_info %>%
    filter(.[, 1] %in% vars) %>% 
    select(variables = 1, types = 2)
  
  missing_count <- sapply(vars,
                          function(x) get_na_cnt(df, x))
  unique_count <- sapply(vars,
                         function(x) get_unique_cnt(df, x))
  data_count <- tally(df) %>%
    pull
  
  as_tibble(cbind(col_info,
                  tibble(missing_count = missing_count,
                         missing_percent = missing_count / data_count * 100,
                         unique_count = unique_count,
                         unique_rate = unique_count / data_count))) 
}


#' Diagnose data quality of categorical variables in the DBMS 
#'
#' @description The diagnose_category() produces information for 
#' diagnosing the quality of the character(CHAR, VARCHAR, VARCHAR2, etc.) 
#' column of the DBMS table through tbl_dbi.
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_category() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#' @param top an integer. Specifies the upper top rank to extract.
#' Default is 10.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' 
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_category.data.frame}}, \code{\link{diagnose.tbl_dbi}}, \code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose_numeric.tbl_dbi}}, \code{\link{diagnose_outlier.tbl_dbi}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#' 
#' # Using pipes ---------------------------------
#' # Diagnosis of all categorical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category()
#'   
#' # Positive values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category(Urban, US)
#'   
#' # Negative values to drop variables, and In-memory mode
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category(-Urban, -US, in_database = FALSE)
#'   
#' # Positions values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category(7, in_database = FALSE, collect_size = 200) 
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category(-7)
#'   
#' # Top rank levels with top argument
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category(top = 2)
#'
#' # Using pipes & dplyr -------------------------
#' # Extraction of level that is more than 60% of categorical data
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_category()  %>%
#'   filter(ratio >= 60)
#'   
diagnose_category.tbl_dbi <- function(.data, ..., top = 10, in_database = TRUE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    diagn_category_impl_dbi(.data, vars, top)
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>% 
      diagn_category_impl(vars, top, add_character = TRUE)
  }
}

diagn_category_impl_dbi <- function(df, vars, top) {
  if (length(vars) == 0) vars <- colnames(df)
  
  col_type <- df %>%
    get_column_info %>%
    filter(.[, 1] %in% vars) %>% 
    select(2) %>% 
    pull()
  
  idx_factor <- which(col_type == "character")
  
  N <- tally(df) %>% 
    pull
  
  get_topn <- function(df, var, top) {
    suppressMessages(
      df %>%
        select(levels = var) %>%
        group_by(levels) %>% 
        tally %>% 
        arrange(desc(n)) %>% 
        as_tibble %>% 
        cbind(var, .) %>% 
        transmute(variables = var, levels, N, freq = n,
                  ratio = n / N * 100, rank = row_number()) %>%
        top_n(top)
    )
  }
  
  result <- lapply(vars[idx_factor],
                   function(x) get_topn(df, x, top))
  as_tibble(do.call("rbind", result))
}


#' Diagnose data quality of numerical variables in the DBMS 
#'
#' @description The diagnose_numeric() produces information
#' for diagnosing the quality of the numerical(INTEGER, NUMBER, etc.) column 
#' of the DBMS table through tbl_dbi.
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_numeric() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_numeric.data.frame}}, \code{\link{diagnose.tbl_dbi}}, \code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose_outlier.tbl_dbi}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Diagnosis of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_numeric()
#'   
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_numeric(Sales, Income, collect_size = 200)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_numeric(-Sales, -Income)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_numeric(5)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_numeric(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # Information records of zero variable more than 0
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_numeric()  %>%
#'   filter(zero > 0)
#'   
diagnose_numeric.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>% 
      diagn_numeric_impl(vars)
  }
}


#' Diagnose outlier of numerical variables in the DBMS
#'
#' @description The diagnose_outlier() produces outlier information
#' for diagnosing the quality of the numerical(INTEGER, NUMBER, etc.) column 
#' of the DBMS table through tbl_dbi.
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_outlier() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context
#' where column names represent column positions.
#' They support unquoting and splicing.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' 
#' @return an object of tbl_df.
#' @seealso \code{\link{diagnose_outlier.data.frame}}, \code{\link{diagnose.tbl_dbi}}, \code{\link{diagnose_category.tbl_dbi}}, \code{\link{diagnose_numeric.tbl_dbi}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Diagnosis of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_outlier()
#'   
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_outlier(Sales, Income, collect_size = 200)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_outlier(-Sales, -Income)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_outlier(5)
#' # Positions values select variables
#' 
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_outlier(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # outlier_ratio is more than 1%
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_outlier()  %>%
#'   filter(outliers_ratio > 1)
#'   
diagnose_outlier.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use dbi = FALSE.")
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>%
      diagnose_outlier_impl(vars)
  }
}


#' Plot outlier information of numerical data diagnosis in the DBMS
#'
#' @description The plot_outlier() visualize outlier information
#' for diagnosing the quality of the numerical(INTEGER, NUMBER, etc.) column 
#' of the DBMS table through tbl_dbi.
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_outlier() will automatically start
#' with all variables.
#' These arguments are automatically quoted and evaluated in a context
#' where column names represent column positions.
#' They support unquoting and splicing.
#' @param col a color to be used to fill the bars. The default is "lightblue".
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' 
#' @seealso \code{\link{plot_outlier.data.frame}}, \code{\link{diagnose_outlier.tbl_dbi}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#' 
#' # Using pipes ---------------------------------
#' # Visualization of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_outlier()
#'   
#' # Positive values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_outlier(Sales, Price)
#'   
#' # Negative values to drop variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_outlier(-Sales, -Price, collect_size = 200)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_outlier(6)
#'   
#' # Positions values select variables
#' carseats %>%
#'   plot_outlier(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # Visualization of numerical variables with a ratio of
#' # outliers greater than 1%
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_outlier(con_sqlite %>% 
#'                  tbl("TB_CARSEATS") %>% 
#'                  diagnose_outlier() %>%
#'                  filter(outliers_ratio > 1) %>%
#'                  select(variables) %>%
#'                  pull())
#'       
plot_outlier.tbl_dbi <- function(.data, ..., col = "lightblue", 
  in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use dbi = FALSE.")
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>%
      plot_outlier_impl(vars, col)
  }
}


#' Performs the Shapiro-Wilk test of normality
#'
#' @description The normality() performs Shapiro-Wilk test of normality 
#' of numerical(INTEGER, NUMBER, etc.) column of the DBMS table through tbl_dbi.
#'
#' @details This function is useful when used with the \code{\link{group_by}}
#' function of the dplyr package. If you want to test by level of the categorical
#' data you are interested in, rather than the whole observation,
#' you can use group_tf as the group_by function.
#' This function is computed \code{\link{shapiro.test}} function.
#'
#' @section Normality test information:
#' The information derived from the numerical data test is as follows.
#'
#' \itemize{
#' \item statistic : the value of the Shapiro-Wilk statistic.
#' \item p_value : an approximate p-value for the test. This is said in
#' Roystion(1995) to be adequate for p_value < 0.1.
#' \item sample : the numer of samples to perform the test.
#' The number of observations supported by the stats::shapiro.test function is 3 to 5000.
#' }
#'
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, normality() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param sample the number of samples to perform the test.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' 
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @return An object of the same class as .data.
#' @seealso \code{\link{normality.data.frame}}, \code{\link{diagnose_numeric.tbl_dbi}}, \code{\link{describe.tbl_dbi}}, \code{\link{plot_normality.tbl_dbi}}.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Normality test of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   normality()
#'
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   normality(Sales, Price, collect_size  = 200)
#'
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   normality(1)
#'
#' # Using pipes & dplyr -------------------------
#' # Test all numerical variables by 'ShelveLoc' and 'US',
#' # and extract only those with 'ShelveLoc' variable level is "Good".
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   group_by(ShelveLoc, US) %>%
#'   normality() %>%
#'   filter(ShelveLoc == "Good")
#'
#' # extract only those with 'Urban' variable level is "Yes",
#' # and test 'Sales' by 'ShelveLoc' and 'US'
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   filter(Urban == "Yes") %>%
#'   group_by(ShelveLoc, US) %>%
#'   normality(Sales)
#'
#' # Test log(Income) variables by 'ShelveLoc' and 'US',
#' # and extract only p.value greater than 0.01.
#' 
#' # SQLite extension functions for log
#' RSQLite::initExtension(con_sqlite)
#' 
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   mutate(log_income = log(Income)) %>%
#'   group_by(ShelveLoc, US) %>%
#'   normality(log_income) %>%
#'   filter(p_value > 0.01)
#' }
#' 
normality.tbl_dbi <- function(.data, ..., sample = 5000, 
  in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (class(.data$ops)[1] != "op_group_by") {
      sample <- min(5000, tally(.data) %>% pull, sample, collect_size)
      
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        sample_n(size = sample) %>% 
        normality_impl(vars, sample)
    } else {
      sample <- min(5000, sum(ungroup(.data) %>% tally() %>% pull), 
        sample, collect_size)
      
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        normality_group_impl(vars, sample)
    }
  }
}


#' Plot distribution information of numerical data
#'
#' @description The plot_normality() visualize distribution information
#' for normality test of the numerical(INTEGER, NUMBER, etc.) column of 
#' the DBMS table through tbl_dbi.
#'
#' @details The scope of the visualization is the provide a distribution information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#'
#' The argument values that left and right can have are as follows.:
#' 
#' \itemize{
#'   \item "log" : log transformation. log(x)
#'   \item "log+1" : log transformation. log(x + 1). Used for values that contain 0.
#'   \item "sqrt" : square root transformation.
#'   \item "1/x" : 1 / x transformation
#'   \item "x^2" : x square transformation
#'   \item "x^3" : x^3 square transformation
#'   \item "Box-Cox" : Box-Box transformation
#'   \item "Yeo-Johnson" : Yeo-Johnson transformation
#' }
#'
#' @section Distribution information:
#' The plot derived from the numerical data visualization is as follows.
#'
#' \itemize{
#' \item histogram by original data
#' \item q-q plot by original data
#' \item histogram by log transfer data
#' \item histogram by square root transfer data
#' }
#'
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_normality() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' 
#' See vignette("EDA") for an introduction to these concepts.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param left character. Specifies the data transformation method to draw the histogram in the 
#' lower left corner. The default is "log".
#' @param right character. Specifies the data transformation method to draw the histogram in the 
#' lower right corner. The default is "sqrt".
#'
#' @seealso \code{\link{plot_normality.data.frame}}, \code{\link{plot_outlier.tbl_dbi}}.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Visualization of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_normality()
#'
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_normality(Income, Price, collect_size = 200)
#'
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_normality(1)
#'
#' # Using pipes & dplyr -------------------------
#' # Plot 'Sales' variable by 'ShelveLoc' and 'US'
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   group_by(ShelveLoc, US) %>%
#'   plot_normality(Sales)
#'
#' # Plot using left and right arguments
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   group_by(ShelveLoc, US) %>%
#'   plot_normality(Sales, left = "Box-Cox", right = "log")
#'
#' # extract only those with 'ShelveLoc' variable level is "Good",
#' # and plot 'Income' by 'US'
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   filter(ShelveLoc == "Good") %>%
#'   group_by(US) %>%
#'   plot_normality(Income)
#' }
#' 
plot_normality.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf, 
                                   left = c("log", "sqrt", "log+1", "1/x", "x^2", 
                                            "x^3", "Box-Cox", "Yeo-Johnson"),
                                   right = c("sqrt", "log", "log+1", "1/x", "x^2", 
                                             "x^3", "Box-Cox", "Yeo-Johnson")) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  left <- match.arg(left)
  right <- match.arg(right)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (class(.data$ops)[1] != "op_group_by") {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        plot_normality_impl(vars, left, right)
    } else {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        plot_normality_group_impl(vars, left, right)
    }
  }
}


#' Compute the correlation coefficient between two numerical data
#'
#' @description The correlate() compute Pearson's the correlation
#' coefficient of the numerical(INTEGER, NUMBER, etc.) column of 
#' the DBMS table through tbl_dbi.
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, correlate() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param method a character string indicating which correlation coefficient (or covariance) is 
#' to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' 
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @seealso \code{\link{correlate.data.frame}}, \code{\link{cor}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Correlation coefficients of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate()
#'  
#' # Positive values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate(Sales, Price)
#'  
#' # Negative values to drop variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate(-Sales, -Price, collect_size = 200)
#'  
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate(1)
#'  
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate(-1, -2, -3, -5, -6)
#'  
#' # ---------------------------------------------
#' # Correlation coefficient
#' # that eliminates redundant combination of variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate() %>%
#'   filter(as.integer(var1) > as.integer(var2))
#'
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   correlate(Sales, Price) %>%
#'   filter(as.integer(var1) > as.integer(var2))
#'
#' # Using pipes & dplyr -------------------------
#' # Compute the correlation coefficient of Sales variable by 'ShelveLoc'
#' # and 'US' variables. And extract only those with absolute
#' # value of correlation coefficient is greater than 0.5
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   group_by(ShelveLoc, US) %>%
#'   correlate(Sales) %>%
#'   filter(abs(coef_corr) >= 0.5)
#'
#' # extract only those with 'ShelveLoc' variable level is "Good",
#' # and compute the correlation coefficient of 'Sales' variable
#' # by 'Urban' and 'US' variables.
#' # And the correlation coefficient is negative and smaller than -0.5
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   filter(ShelveLoc == "Good") %>%
#'   group_by(Urban, US) %>%
#'   correlate(Sales) %>%
#'   filter(coef_corr < 0) %>%
#'   filter(abs(coef_corr) > 0.5)
#'  
correlate.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf,
                              method = c("pearson", "kendall", "spearman")) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  method <- match.arg(method)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (class(.data$ops)[1] != "op_group_by") {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        correlate_impl(vars, method)
    } else {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        correlate_group_impl(vars, method)
    }
  }
}


#' Visualize correlation plot of numerical data
#'
#' @description The plot_correlate() visualize correlation plot
#' for find relationship between two numerical(INTEGER, NUMBER, etc.) column of 
#' the DBMS table through tbl_dbi.
#'
#' @details The scope of the visualization is the provide a correlation information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#'
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_correlate() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param method a character string indicating which correlation coefficient (or covariance) is 
#' to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' 
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @seealso \code{\link{plot_correlate.data.frame}}, \code{\link{plot_outlier.tbl_dbi}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Visualize correlation plot of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_correlate()
#'   
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_correlate(Sales, Price, collect_size = 200)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_correlate(-Sales, -Price)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_correlate(1)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   plot_correlate(-1, -2, -3, -5, -6)
#'
#' # Using pipes & dplyr -------------------------
#' # Visualize correlation plot of 'Sales' variable by 'ShelveLoc'
#' # and 'US' variables.
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   group_by(ShelveLoc, US) %>%
#'   plot_correlate(Sales)
#'
#' # Extract only those with 'ShelveLoc' variable level is "Good",
#' # and visualize correlation plot of 'Sales' variable by 'Urban'
#' # and 'US' variables.
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   filter(ShelveLoc == "Good") %>%
#'   group_by(Urban, US) %>%
#'   plot_correlate(Sales)
#'  
plot_correlate.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf,
                                   method = c("pearson", "kendall", "spearman")) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  method <- match.arg(method)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (class(.data$ops)[1] != "op_group_by") {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        plot_correlate_impl(vars, method)
    } else {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        plot_correlate_group_impl(vars, method)
    }
  }  
}


#' Compute descriptive statistic
#'
#' @description The describe() compute descriptive statistic of numerical(INTEGER, NUMBER, etc.) 
#' column of the DBMS table through tbl_dbi for exploratory data analysis.
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
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, describe() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' 
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @return An object of the same class as .data.
#' @seealso \code{\link{describe.data.frame}}, \code{\link{diagnose_numeric.tbl_dbi}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Positive values select variables
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   describe(Sales, CompPrice, Income)
#'
#' # Negative values to drop variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   describe(-Sales, -CompPrice, -Income, collect_size = 200)
#'
#' # Using pipes & dplyr -------------------------
#' # Find the statistic of all numerical variables by 'ShelveLoc' and 'US',
#' # and extract only those with 'ShelveLoc' variable level is "Good".
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   group_by(ShelveLoc, US) %>%
#'   describe() %>%
#'   filter(ShelveLoc == "Good")
#'
#' # extract only those with 'Urban' variable level is "Yes",
#' # and find 'Sales' statistics by 'ShelveLoc' and 'US'
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   filter(Urban == "Yes") %>%
#'   group_by(ShelveLoc, US) %>%
#'   describe(Sales)
#'  
describe.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (class(.data$ops)[1] != "op_group_by") {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        describe_impl(vars)
    } else {
      group <- .data$ops$dots[[1]]
      
      .data %>% 
        group_by(group) %>% 
        dplyr::collect(n = collect_size) %>%
        describe(vars)
    }
  }
}


#' Target by one column in the DBMS
#'
#' @description
#' In the data analysis, a target_df class is created to identify the
#' relationship between the target column and the other column of the DBMS table through tbl_dbi
#'
#' @details
#' Data analysis proceeds with the purpose of predicting target variables that
#'  correspond to the facts of interest, or examining associations and
#'  relationships with other variables of interest.
#'  Therefore, it is a major challenge for EDA to examine the relationship
#'  between the target variable and its corresponding variable.
#'  Based on the derived relationships, analysts create scenarios for data analysis.
#'
#'  target_by() inherits the \code{\link{grouped_df}} class and returns a target_df
#'  class containing information about the target variable and the variable.
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @param .data a tbl_dbi.
#' @param target target variable.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R.
#' Applies only if in_database = FALSE.
#' @param ... arguments to be passed to methods.
#' @return an object of target_df class.
#' Attributes of target_df class is as follows.
#' \itemize{
#' \item type_y : the data type of target variable.
#' }
#' @seealso \code{\link{target_by.data.frame}}, \code{\link{relate}}.
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # If the target variable is a categorical variable
#' categ <- target_by(con_sqlite %>% tbl("TB_CARSEATS") , US)
#'
#' # If the variable of interest is a numarical variable
#' cat_num <- relate(categ, Sales)
#' cat_num
#' summary(cat_num)
#' plot(cat_num)
#'
#' # If the variable of interest is a categorical column
#' cat_cat <- relate(categ, ShelveLoc)
#' cat_cat
#' summary(cat_cat)
#' plot(cat_cat)
#'
#' ##---------------------------------------------------
#' # If the target variable is a categorical column, 
#' # and In-memory mode and collect size is 350
#' num <- target_by(con_sqlite %>% tbl("TB_CARSEATS"), Sales, collect_size = 350)
#'
#' # If the variable of interest is a numarical column
#' num_num <- relate(num, Price)
#' num_num
#' summary(num_num)
#' plot(num_num)
#' plot(num_num, hex_thres = 400)
#'
#' # If the variable of interest is a categorical column
#' num_cat <- relate(num, ShelveLoc)
#' num_cat
#' summary(num_cat)
#' plot(num_cat)
#' @method target_by tbl_dbi
#' @export
#'
target_by.tbl_dbi <- function(.data, target, in_database = FALSE, collect_size = Inf, ...) {
  tryCatch(vars <- tidyselect::vars_select(colnames(.data), !! rlang::enquo(target)),
           error = function(e) {
             pram <- as.character(substitute(target))
             stop(sprintf("Column %s is unknown", pram))
           }, finally = NULL)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>%
      mutate_if(is.character, as.factor) %>% 
      target_by_impl(vars)
  }  
}


#' Reporting the information of data diagnosis for table of the DBMS
#'
#' @description The diagnose_report() report the information for diagnosing
#' the quality of the DBMS table through tbl_dbi
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
#' @param .data a tbl_dbi.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param font_family character. font family name for figure in pdf.
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{diagnose_report.data.frame}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is DataDiagnosis_Report.pdf
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_report()
#'   
#' # create pdf file. file name is Diagn.pdf, and collect size is 350
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_report(collect_size = 350, output_file = "Diagn.pdf")
#' 
#' # create html file. file name is Diagnosis_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_report(output_format = "html")
#' 
#' # create html file. file name is Diagn.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   diagnose_report(output_format = "html", output_file = "Diagn.html")
#' }
#' 
#' @method diagnose_report tbl_dbi
#' @export
#' 
diagnose_report.tbl_dbi <- function(.data, output_format = c("pdf", "html"), 
  output_file = NULL, output_dir = tempdir(), font_family = NULL, 
  in_database = FALSE, collect_size = Inf, ...) {
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    diagnose_report(collect(.data, n = collect_size), output_format, 
      output_file, output_dir, font_family)
  }  
}


#' Reporting the information of EDA for table of the DBMS
#'
#' @description The eda_report() report the information of Exploratory
#' data analysis for object inheriting from the DBMS table through tbl_dbi
#'
#' @details Generate generalized data EDA reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for EDA a data frame with a large number of variables
#' than data with a small number of variables.
#' For pdf output, Korean Gothic font must be installed in Korean operating system.
#' 
#' @section Reported information:
#' The EDA process will report the following information:
#'
#' \itemize{
#'   \item Introduction
#'   \itemize{
#'     \item Information of Dataset
#'     \item Information of Variables
#'     \item About EDA Report
#'   }
#'   \item Univariate Analysis
#'   \itemize{
#'     \item Descriptive Statistics
#'     \item Normality Test of Numerical Variables
#'     \itemize{
#'       \item Statistics and Visualization of (Sample) Data
#'     }
#'   }
#'   \item Relationship Between Variables
#'   \itemize{
#'     \item Correlation Coefficient
#'     \itemize{
#'       \item Correlation Coefficient by Variable Combination
#'       \item Correlation Plot of Numerical Variables
#'     }
#'   }
#'   \item Target based Analysis
#'   \itemize{
#'     \item Grouped Descriptive Statistics
#'     \itemize{
#'       \item Grouped Numerical Variables
#'       \item Grouped Categorical Variables
#'     }
#'     \item Grouped Relationship Between Variables
#'     \itemize{
#'       \item Grouped Correlation Coefficient
#'       \item Grouped Correlation Plot of Numerical Variables
#'     }
#'   }
#' }
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @param .data a tbl_dbi.
#' @param target target variable.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param font_family character. font family name for figure in pdf.
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{eda_report.data.frame}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#'
#' ## target variable is categorical variable
#' # reporting the EDA information
#' # create pdf file. file name is EDA_Report.pdf
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(US)
#' 
#' # create pdf file. file name is EDA_TB_CARSEATS.pdf
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report("US", output_file = "EDA_TB_CARSEATS.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report("US", output_format = "html")
#' 
#' # create html file. file name is EDA_TB_CARSEATS.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(US, output_format = "html", output_file = "EDA_TB_CARSEATS.html")
#'
#' ## target variable is numerical variable
#' # reporting the EDA information, and collect size is 350
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(Sales, collect_size = 350)
#' 
#' # create pdf file. file name is EDA2.pdf
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report("Sales", output_file = "EDA2.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report("Sales", output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(Sales, output_format = "html", output_file = "EDA2.html")
#'
#' ## target variable is null
#' # reporting the EDA information
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report()
#' 
#' # create pdf file. file name is EDA2.pdf
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(output_file = "EDA2.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   eda_report(output_format = "html", output_file = "EDA2.html")
#' }
#'
#' @export
#' 
eda_report.tbl_dbi <- function(.data, target = NULL,  output_format = c("pdf", "html"), 
  output_file = NULL, font_family = NULL, output_dir = tempdir(), in_database = FALSE, 
  collect_size = Inf, ...) {
  tryCatch(vars <- tidyselect::vars_select(colnames(.data), !! rlang::enquo(target)),
    error = function(e) {
      pram <- as.character(substitute(target))
      stop(sprintf("Column %s is unknown", pram))
    }, finally = NULL)
  
  output_format <- match.arg(output_format)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    tab <- .data %>% 
      dplyr::collect(n = collect_size) %>%
      mutate_if(is.character, as.factor) 
    
    eda_report(tab, target = vars, output_format, 
      output_file, output_dir, font_family)    
  }
}




