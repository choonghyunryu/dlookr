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
#' \donttest{
#' library(dplyr)
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#' 
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   get_column_info
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
get_column_info <- function(df) {
  if (requireNamespace("DBI", quietly = TRUE)) {
    res <- DBI::dbSendQuery(df$src$con,
                            dbplyr::remote_query(df))
  } else {
    stop("Package 'DBI' needed for this function to work. Please install it.", 
         call. = FALSE)
  }

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
#' \donttest{
#' library(dplyr)
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy jobchange to the DBMS with a table named TB_JOBCHANGE
#' copy_to(con_sqlite, jobchange, name = "TB_JOBCHANGE", overwrite = TRUE)
#' 
#' # Using pipes ---------------------------------
#' # Diagnosis of all columns
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose()
#'   
#' # Positive values select columns
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose(gender, education_level, company_size)
#'   
#' # Negative values to drop columns
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose(-gender, -education_level, -company_size)
#'   
#' # Positions values select columns, and In-memory mode
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose(1, 3, 8, in_database = FALSE)
#'   
#' # Positions values select columns, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose(-8, -9, -10, in_database = FALSE, collect_size = 200)
#'
#' # Using pipes & dplyr -------------------------
#' # Diagnosis of missing variables
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose() %>%
#'   filter(missing_count > 0)
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
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
#' @param type a character string specifying how result are extracted.
#' Default is "rank" that extract top n ranks by decreasing frequency. 
#' In this case, if there are ties in rank, more rows than the number specified 
#' by the top argument are returned.
#' "n" extract top n rows by decreasing frequency. 
#' If there are too many rows to be returned because there are too many ties, 
#' you can adjust the returned rows appropriately by using "n".
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
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy jobchange to the DBMS with a table named TB_JOBCHANGE
#' copy_to(con_sqlite, jobchange, name = "TB_JOBCHANGE", overwrite = TRUE)
#' 
#' # Using pipes ---------------------------------
#' # Diagnosis of all categorical variables
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category()
#'   
#' # Positive values select variables
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category(company_type, job_chnge)
#'   
#' # Negative values to drop variables, and In-memory mode
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category(-company_type, -job_chnge, in_database = FALSE)
#'   
#' # Positions values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category(7, in_database = FALSE, collect_size = 200) 
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category(-7)
#'   
#' # Top rank levels with top argument
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category(top = 2)
#'
#' # Using pipes & dplyr -------------------------
#' # Extraction of level that is more than 60% of categorical data
#' con_sqlite %>% 
#'   tbl("TB_JOBCHANGE") %>% 
#'   diagnose_category()  %>%
#'   filter(ratio >= 60)
#'   
#' # Using type argument -------------------------
#'  dfm <- data.frame(alpabet = c(rep(letters[1:5], times = 5), "c")) 
#'  
#' # copy dfm to the DBMS with a table named TB_EXAMPLE
#' copy_to(con_sqlite, dfm, name = "TB_EXAMPLE", overwrite = TRUE)  
#'  
#' # extract rows that less than equal rank 10
#' # default of top argument is 10
#' con_sqlite %>% 
#'   tbl("TB_EXAMPLE") %>% 
#'   diagnose_category()
#'    
#' # extract rows that less than equal rank 2
#' con_sqlite %>% 
#'   tbl("TB_EXAMPLE") %>% 
#'   diagnose_category(top = 2, type = "rank")
#'    
#' # extract rows that less than equal rank 2
#' # default of type argument is "rank"
#' con_sqlite %>% 
#'   tbl("TB_EXAMPLE") %>% 
#'   diagnose_category(top = 2)
#'  
#' # extract only 2 rows
#' con_sqlite %>% 
#'   tbl("TB_EXAMPLE") %>% 
#'   diagnose_category(top = 2, type = "n")
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
diagnose_category.tbl_dbi <- function(.data, ..., top = 10, type = c("rank", "n")[1],
                                      in_database = TRUE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    diagn_category_impl_dbi(.data, vars, top, type)
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>% 
      diagn_category_impl(vars, top, type, add_character = TRUE, add_date = TRUE)
  }
}

diagn_category_impl_dbi <- function(df, vars, top, type) {
  if (length(vars) == 0) vars <- colnames(df)
  
  if (length(type) != 1 | !type %in% c("rank", "n")) {
    message("The type argument must be one of \"rank\" or \"n\".\n")
    return(NULL)    
  }
  
  col_type <- df %>%
    get_column_info %>%
    filter(.[, 1] %in% vars) %>% 
    select(2) %>% 
    pull()
  
  idx_factor <- which(col_type == "character")
  
  N <- tally(df) %>% 
    pull
  
  get_topn <- function(df, var, top, type) {
    suppressMessages({
      tab <- df %>%
        select(levels = var) %>%
        group_by(levels) %>% 
        tally %>% 
        arrange(desc(n)) %>% 
        as_tibble %>% 
        cbind(var, .) %>% 
        transmute(variables = var, levels, N, freq = n,
                  ratio = n / N * 100, 
                  rank = rank(max(freq) - freq, ties.method = "min"))
      
      if (type == "n") {
        tab %>% 
          slice_head(n = top)
      } else if (type == "rank") {
        tab %>% 
          top_n(n = top, freq)      
      }
    })
  }
  
  result <- lapply(vars[idx_factor],
                   function(x) get_topn(df, x, top, type))
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
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Diagnosis of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_numeric()
#'   
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_numeric(age, sodium, collect_size = 200)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_numeric(-age, -sodium)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_numeric(5)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_numeric(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # List of variables containing outliers
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_numeric()  %>%
#'   filter(outlier > 0)
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
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
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Diagnosis of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_outlier()
#'   
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_outlier(platelets, sodium, collect_size = 200)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_outlier(-platelets, -sodium)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_outlier(5)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_outlier(-1, -5)
#'
#' # Using pipes & dplyr -------------------------
#' # outlier_ratio is more than 1%
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_outlier()  %>%
#'   filter(outliers_ratio > 1)
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
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
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font(). 
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
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' 
#' @seealso \code{\link{plot_outlier.data.frame}}, \code{\link{diagnose_outlier.tbl_dbi}}.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#' 
#' # Using pipes ---------------------------------
#' # Visualization of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_outlier()
#'   
#' # Positive values select variables
#'  con_sqlite %>% 
#'    tbl("TB_HEARTFAILURE") %>% 
#'    plot_outlier(platelets, sodium)
#'   
#' # Negative values to drop variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_outlier(-platelets, -sodium, collect_size = 200)
#'   
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_outlier(6)
#'   
#' # Negative values to drop variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_outlier(-1, -5)
#'   
#' # Not allow the typographic elements
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_outlier(-1, -5, typographic = FALSE)
#'
#' # Using pipes & dplyr -------------------------
#' # Visualization of numerical variables with a ratio of
#' # outliers greater than 1%
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_outlier(con_sqlite %>% 
#'                  tbl("TB_HEARTFAILURE") %>% 
#'                  diagnose_outlier() %>%
#'                  filter(outliers_ratio > 1) %>%
#'                  select(variables) %>%
#'                  pull())
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#'       
plot_outlier.tbl_dbi <- function(.data, ..., col = "steelblue", 
  in_database = FALSE, collect_size = Inf, typographic = TRUE, base_family = NULL) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use dbi = FALSE.")
  } else {
    .data %>% 
      dplyr::collect(n = collect_size) %>%
      plot_outlier_impl(vars, col, typographic, base_family)
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
#' @seealso \code{\link{normality.data.frame}}, \code{\link{diagnose_numeric.tbl_dbi}}, \code{\link{describe.tbl_dbi}}.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Normality test of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   normality()
#'
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   normality(platelets, sodium, collect_size  = 200)
#'
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   normality(1)
#'
#' # Using pipes & dplyr -------------------------
#' # Test all numerical variables by 'smoking' and 'death_event',
#' # and extract only those with 'smoking' variable level is "Yes".
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   group_by(smoking, death_event) %>%
#'   normality() %>%
#'   filter(smoking == "Yes")
#'
#' # extract only those with 'sex' variable level is "Male",
#' # and test 'sodium' by 'smoking' and 'death_event'
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   filter(sex == "Male") %>%
#'   group_by(smoking, death_event) %>%
#'   normality(sodium)
#'
#' # Test log(sodium) variables by 'smoking' and 'death_event',
#' # and extract only p.value greater than 0.01.
#' 
#' # SQLite extension functions for log
#' RSQLite::initExtension(con_sqlite)
#' 
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   mutate(log_sodium = log(sodium)) %>%
#'   group_by(smoking, death_event) %>%
#'   normality(log_sodium) %>%
#'   filter(p_value > 0.01)
#'  
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
normality.tbl_dbi <- function(.data, ..., sample = 5000, 
  in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (!is_grouped(.data)) {
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
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font(). 
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
#' @param col a color to be used to fill the bars. The default is "steelblue".
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' 
#' @seealso \code{\link{plot_normality.data.frame}}, \code{\link{plot_outlier.tbl_dbi}}.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Visualization of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_normality()
#'
#' # Positive values select variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_normality(platelets, sodium, collect_size = 200)
#'
#' # Positions values select variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_normality(1)
#'
#' # Not allow the typographic elements
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   plot_normality(1, typographic = FALSE)
#'   
#' # Using pipes & dplyr -------------------------
#' # Plot 'sodium' variable by 'smoking' and 'death_event'
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   group_by(smoking, death_event) %>%
#'   plot_normality(sodium)
#'
#' # Plot using left and right arguments
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   group_by(smoking, death_event) %>%
#'   plot_normality(sodium, left = "Box-Cox", right = "log")
#'
#' # extract only those with 'smoking' variable level is "Yes",
#' # and plot 'sodium' by 'death_event'
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   filter(smoking == "Yes") %>%
#'   group_by(death_event) %>%
#'   plot_normality(sodium)
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
plot_normality.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf, 
                                   left = c("log", "sqrt", "log+1", "1/x", "x^2", 
                                            "x^3", "Box-Cox", "Yeo-Johnson"),
                                   right = c("sqrt", "log", "log+1", "1/x", "x^2", 
                                             "x^3", "Box-Cox", "Yeo-Johnson"),
                                   col = "steelblue", typographic = TRUE, 
                                   base_family = NULL) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  left <- match.arg(left)
  right <- match.arg(right)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (!is_grouped(.data)) {
      .data %>%
        dplyr::collect(n = collect_size) %>%
        plot_normality_impl(vars, left, right, col, typographic, base_family)
    } else {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        plot_normality_group_impl(vars, left, right, col, typographic, base_family)
    }
  }
}


#' @rdname correlate.data.frame
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#'
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Correlation coefficients of all numerical variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   correlate()
#'
#' # Using pipes & dplyr -------------------------
#' # Compute the correlation coefficient of creatinine variable by 'hblood_pressure'
#' # and 'death_event' variables.
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   group_by(hblood_pressure, death_event) %>%
#'   correlate(creatinine) 
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#'   
correlate.tbl_dbi <- function(.data, ..., 
                              method = c("pearson", "kendall", "spearman", 
                                         "cramer", "theil"),
                              in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  method <- match.arg(method)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (!is_grouped(.data)) {
      if (method %in% c("pearson", "kendall", "spearman")) {
        result <- .data %>% 
          dplyr::collect(n = collect_size) %>%
          correlate_impl_num(vars, method)
      } else if (method %in% c("cramer", "theil")) {
        result <- .data %>% 
          dplyr::collect(n = collect_size) %>%
          correlate_impl_cat(vars, method)
      }  
      

    } else {
      if (method %in% c("pearson", "kendall", "spearman")) {
        result <- .data %>% 
          dplyr::collect(n = collect_size) %>%
          correlate_group_impl_num(vars, method)
      } else if (method %in% c("cramer", "theil")) {
        result <- .data %>% 
          dplyr::collect(n = collect_size) %>%
          correlate_group_impl_cat(vars, method)
      }
    }
    
    return(result)
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
#' The direction of the diagonal is top-left to bottom-right. and color of the 
#' cells is 'red' to -1, 'blue' to 1.
#' 
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font(). 
#' 
#' @param .data a tbl_dbi.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_correlate() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
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
#' @seealso \code{\link{plot.correlate}}, \code{\link{plot_outlier.tbl_dbi}}.
#' @export
#' @keywords internal
plot_correlate.tbl_dbi <- function(.data, ..., in_database = FALSE, collect_size = Inf,
                                   method = c("pearson", "kendall", "spearman"),
                                   typographic = TRUE, base_family = NULL) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  method <- match.arg(method)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (!is_grouped(.data)) {
      .data %>%
        dplyr::collect(n = collect_size) %>%
        plot_correlate_impl(vars, method, typographic, base_family)
    } else {
      .data %>% 
        dplyr::collect(n = collect_size) %>%
        plot_correlate_group_impl(vars, method, typographic, base_family)
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
#' From version 0.5.5, the 'variable' column in the "descriptive statistic 
#' information" tibble object has been changed to 'described_variables'. 
#' This is because there are cases where 'variable' is included in the variable 
#' name of the data. There is probably no case where 'described_variables' is 
#' included in the variable name of the data.
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
#' @param statistics character. the name of the descriptive statistic to calculate. The defaults is c("mean", "sd", "se_mean", "IQR", "skewness", "kurtosis", "quantiles")
#' @param quantiles numeric. list of quantiles to calculate. The values of elements must be between 0 and 1. and to calculate quantiles, you must include "quantiles" in the statistics argument value. The default is c(0, .01, .05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99, 1).
#' @param all.combinations logical. When used with group_by(), 
#' this argument expresses all combinations of  group combinations. 
#' If the argument value is TRUE, cases that do not exist as actual 
#' data are also included in the output.
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
#' \donttest{
#' library(dplyr)
#' 
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # Using pipes ---------------------------------
#' # Positive values select variables
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   describe(platelets, creatinine, sodium)
#'   
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   describe(platelets, creatinine, sodium, 
#'     statistics = c("mean", "sd", "quantiles"), quantiles = 0.1)
#'
#' # Negative values to drop variables, and In-memory mode and collect size is 200
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   describe(-platelets, -creatinine, -sodium, collect_size = 200)
#'
#' # Using pipes & dplyr -------------------------
#' # Find the statistic of all numerical variables by 'smoking' and 'death_event',
#' # and extract only those with 'smoking' variable level is "Yes".
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   group_by(smoking, death_event) %>%
#'   describe() %>%
#'   filter(smoking == "Yes")
#'
#' # Using all.combinations = TRUE
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   filter(!smoking %in% "Yes" | !death_event %in% "Yes") %>% 
#'   group_by(smoking, death_event) %>%
#'   describe(all.combinations = TRUE) %>%
#'   filter(smoking == "Yes")
#'   
#' # extract only those with 'sex' variable level is "Male",
#' # and find 'sodium' statistics by 'smoking' and 'death_event'
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   filter(sex == "Male") %>%
#'   group_by(smoking, death_event) %>%
#'   describe(sodium)
#'
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
describe.tbl_dbi <- function(.data, ..., statistics = NULL, quantiles = NULL,
                             all.combinations = FALSE,
                             in_database = FALSE, collect_size = Inf) {
  vars <- tidyselect::vars_select(colnames(.data), !!! rlang::quos(...))
  
  if (is.null(statistics))
    statistics <- c("mean", "sd", "se_mean", "IQR", "skewness",
                    "kurtosis", "quantiles")
  
  if (is.null(quantiles))
    quantiles <- c(0, .01, .05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
                   0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
  quantiles <- unique(quantiles)
  
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    if (!is_grouped(.data)) {
      .data %>%
        dplyr::collect(n = collect_size) %>%
        describe_impl(vars, statistics, quantiles)
    } else {
      .data %>%
        dplyr::collect(n = collect_size) %>%
        describe(vars, statistics = statistics, quantiles = quantiles,
                 all.combinations = all.combinations)
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
#' \donttest{
#' library(dplyr)
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # If the target variable is a categorical variable
#' categ <- target_by(con_sqlite %>% tbl("TB_HEARTFAILURE") , death_event)
#'
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, sodium)
#' cat_num
#' summary(cat_num)
#' plot(cat_num)
#'
#' # If the variable of interest is a categorical column
#' cat_cat <- relate(categ, hblood_pressure)
#' cat_cat
#' summary(cat_cat)
#' plot(cat_cat)
#'
#' ##---------------------------------------------------
#' # If the target variable is a categorical column, 
#' # and In-memory mode and collect size is 200
#' num <- target_by(con_sqlite %>% tbl("TB_HEARTFAILURE"), death_event, collect_size = 250)
#'
#' # If the variable of interest is a numerical column
#' num_num <- relate(num, creatinine)
#' num_num
#' summary(num_num)
#' plot(num_num)
#' plot(num_num, hex_thres = 200)
#'
#' # If the variable of interest is a categorical column
#' num_cat <- relate(num, smoking)
#' num_cat
#' summary(num_cat)
#' plot(num_cat)
#' 
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' 
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
#' if (FALSE) {
#' library(dplyr)
#' 
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure2 to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure2, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is DataDiagnosis_Report.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_report()
#'   
#' # create pdf file. file name is Diagn.pdf, and collect size is 350
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_report(collect_size = 350, output_file = "Diagn.pdf")
#' 
#' # create html file. file name is Diagnosis_Report.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_report(output_format = "html")
#' 
#' # create html file. file name is Diagn.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_report(output_format = "html", output_file = "Diagn.html")
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
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
#' if (FALSE) {
#' library(dplyr)
#' 
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure2 to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure2, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' ## target variable is categorical variable
#' # reporting the EDA information
#' # create pdf file. file name is EDA_Report.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(death_event)
#' 
#' # create pdf file. file name is EDA_TB_HEARTFAILURE.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report("death_event", output_file = "EDA_TB_HEARTFAILURE.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report("death_event", output_format = "html")
#' 
#' # create html file. file name is EDA_TB_HEARTFAILURE.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(death_event, output_format = "html", output_file = "EDA_TB_HEARTFAILURE.html")
#'
#' ## target variable is numerical variable
#' # reporting the EDA information, and collect size is 250
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(sodium, collect_size = 250)
#' 
#' # create pdf file. file name is EDA2.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report("sodium", output_file = "EDA2.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report("sodium", output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(sodium, output_format = "html", output_file = "EDA2.html")
#'
#' ## target variable is null
#' # reporting the EDA information
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report()
#' 
#' # create pdf file. file name is EDA2.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(output_file = "EDA2.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_report(output_format = "html", output_file = "EDA2.html")
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
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

is_grouped <- function(x) {
  length(group_vars(x)) > 0
}
