#' Reporting the information of data diagnosis for table of the DBMS with html
#'
#' @description The diagnose_web_report() report the information for diagnosing
#' the quality of the DBMS table through tbl_dbi
#'
#' @details Generate generalized data diagnostic reports automatically.
#' This is useful for diagnosing a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' @section Reported information:
#' Reported from the data diagnosis is as follows.
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \itemize{
#'       \item Data Structures
#'       \item Data Types
#'       \item Job Informations
#'     }
#'     \item Warnings
#'     \item Variables
#'   }
#'   \item Missing Values
#'   \itemize{
#'     \item Top Ranks
#'   }   
#'   \item Numerical Variable Diagnosis
#'   \itemize{
#'     \item List of Missing Values
#'     \item Visualization
#'   }
#'   \item Unique Values
#'   \itemize{
#'     \item Categorical Variables
#'     \item Numerical Variables
#'   }
#'   \item Outliers
#'   \item Samples
#'   \itemize{
#'     \item Duplicated
#'     \item Heads
#'     \item Tails
#'   }
#' }
#'
#' @param .data a tbl_dbi.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param title_color character. color of title. default is "gray".
#' @param thres_uniq_cat numeric. threshold to use for "Unique Values - 
#' Categorical Variables". default is 0.5.
#' @param thres_uniq_num numeric. threshold to use for "Unique Values - 
#' Numerical Variables". default is 5.
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param as_factor logical. whether to convert to factor when importing a character type variable from DBMS table into R.
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{diagnose_web_report.data.frame}}.
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
#' if (!require(DBI)) install.packages('DBI', repos = "http://cran.us.r-project.org")
#' if (!require(RSQLite)) install.packages('RSQLite', repos = "http://cran.us.r-project.org")
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure2 to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure2, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is Diagnosis_Report.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_web_report()
#'   
#' # create pdf file. file name is Diagn.html, and collect size is 250
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_web_report(collect_size = 250, output_file = "Diagn.html")
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' }
#' 
#' @method diagnose_web_report tbl_dbi
#' @export
#' 
diagnose_web_report.tbl_dbi <- function(.data, output_file = NULL, output_dir = tempdir(),   
                                        browse = TRUE, title = "Data Diagnosis",
                                        subtitle = deparse(substitute(.data)), author = "dlookr",
                                        title_color = "gray", thres_uniq_cat = 0.5, 
                                        thres_uniq_num = 5, logo_img = NULL, 
                                        create_date = Sys.time(),
                                        theme = c("orange", "blue")[1], 
                                        sample_percent = 100, in_database = FALSE, 
                                        collect_size = Inf, as_factor = TRUE, ...) {
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    raw_data <- collect(.data, n = collect_size)
    
    if (as_factor) {
      raw_data <- raw_data %>% 
        mutate_if(is.character, as.factor)
    }
    
    diagnose_web_report(raw_data, output_file = output_file, 
                        output_dir = output_dir, browse = browse, title = title,
                        title_color = title_color, thres_uniq_cat = thres_uniq_cat, 
                        thres_uniq_num = thres_uniq_num, logo_img = logo_img, 
                        create_date = create_date, theme = theme, 
                        subtitle = dbplyr::remote_name(.data) %>% as.character(), author = author,
                        sample_percent = sample_percent, is_tbl_dbi = TRUE)
  }  
}


#' Reporting the information of data diagnosis for table of the DBMS
#'
#' @description The diagnose_paged_report() paged report the information 
#' for diagnosing the quality of the DBMS table through tbl_dbi.
#'
#' @details Generate generalized data diagnostic reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for diagnosing a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' Create an  PDF through the Chrome DevTools Protocol. If you want to create PDF, 
#' Google Chrome or Microsoft Edge (or Chromium on Linux) must be installed prior to using this function.
#' If not installed, you must use output_format = "html".
#' 
#' @section Reported information:
#' Reported from the data diagnosis is as follows.
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \item Job Informations
#'     \item Warnings
#'     \item Variables
#'   } 
#'   \item Missing Values
#'   \itemize{
#'     \item List of Missing Values
#'     \item Visualization
#'   } 
#'   \item Unique Values
#'   \itemize{
#'     \item Categorical Variables
#'     \item Numerical Variables
#'   } 
#'   \item Categorical Variable Diagnosis
#'   \itemize{
#'      \item Top Ranks
#'   }
#'   \item Numerical Variable Diagnosis
#'   \itemize{
#'     \item Distribution
#'     \itemize{
#'       \item Zero Values
#'       \item Minus Values
#'     }
#'     \item Outliers
#'     \itemize{
#'       \item List of Outliers
#'       \item Individual Outliers
#'     }
#'   }
#' }
#'
#' @param .data a tbl_dbi.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by rmarkdown::render() and pagedown::chrome_print(). so, 
#' you needed Chrome web browser on computer.  
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param abstract_title character. abstract title of report. default is 
#' "Report Overview".
#' @param abstract character. abstract of report. 
#' @param title_color character. color of title. default is "white".
#' @param subtitle_color character. color of title. default is "gold".
#' @param thres_uniq_cat numeric. threshold to use for "Unique Values - 
#' Categorical Variables". default is 0.5.
#' @param thres_uniq_num numeric. threshold to use for "Unique Values - 
#' Numerical Variables". default is 5.
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param flag_content_missing logical. whether to output "Missing Value" information. 
#' the default value is TRUE, and the information is displayed.
#' @param flag_content_zero logical. whether to output "Zero Values" information. 
#' the default value is TRUE, and the information is displayed.
#' @param flag_content_minus logical. whether to output "Minus Values" information. 
#' the default value is TRUE, and the information is displayed.
#' @param cover_img character. name of cover image. 
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param as_factor logical. whether to convert to factor when importing a character type variable from DBMS table into R.
#' @param ... arguments to be passed to pagedown::chrome_print().
#' 
#' @seealso \code{\link{diagnose_paged_report.data.frame}}.
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
#' if (!require(DBI)) install.packages('DBI', repos = "http://cran.us.r-project.org")
#' if (!require(RSQLite)) install.packages('RSQLite', repos = "http://cran.us.r-project.org")
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure2 to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure2, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is Diagnosis_Paged_Report.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_paged_report()
#'   
#' # create pdf file. file name is Diagn.pdf, and collect size is 250
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   diagnose_paged_report(collect_size = 250, output_file = "Diagn.pdf")
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' }
#' 
#' @method diagnose_paged_report tbl_dbi
#' @export
diagnose_paged_report.tbl_dbi <- function(.data, output_format = c("pdf", "html")[1],
                                  output_file = NULL, output_dir = tempdir(),   
                                  browse = TRUE, title = "Data Diagnosis Report",
                                  subtitle = deparse(substitute(.data)), author = "dlookr",
                                  abstract_title = "Report Overview", abstract = NULL,
                                  title_color = "white", subtitle_color = "gold",
                                  thres_uniq_cat = 0.5, thres_uniq_num = 5,
                                  flag_content_zero = TRUE, flag_content_minus = TRUE,
                                  flag_content_missing = TRUE, cover_img = NULL, 
                                  create_date = Sys.time(),
                                  logo_img = NULL, theme = c("orange", "blue")[1],
                                  sample_percent = 100, in_database = FALSE, 
                                  collect_size = Inf, as_factor = TRUE, ...) {
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    raw_data <- collect(.data, n = collect_size)
    
    if (as_factor) {
      raw_data <- raw_data %>% 
        mutate_if(is.character, as.factor)
    }
    
    if (output_format == "pdf") {
      args <- list(...)

      if ("extra_args" %in% names(args)) {
        extra_args <- args$extra_args
      } else {
        extra_args <- c("--disable-gpu")
      }       
    }
    
    diagnose_paged_report(raw_data, 
                          output_format = output_format, output_file = output_file, 
                          output_dir = output_dir, browse = browse, title = title,
                          title_color = title_color, thres_uniq_cat = thres_uniq_cat, 
                          thres_uniq_num = thres_uniq_num, 
                          subtitle = dbplyr::remote_name(.data) %>% as.character(), 
                          author = author,
                          flag_content_zero = flag_content_zero,
                          flag_content_minus = flag_content_minus, 
                          flag_content_missing = flag_content_missing,
                          logo_img = logo_img, create_date = create_date, 
                          theme = theme, sample_percent = sample_percent, 
                          is_tbl_dbi = TRUE,
                          extra_args = extra_args)
  }  
}  
  

#' Reporting the information of EDA for table of the DBMS with html
#'
#' @description The eda_web_report() report the information of exploratory 
#' data analysis for the DBMS table through tbl_dbi
#'
#' @details Generate generalized EDA report automatically.
#' This feature is useful for EDA of data with many variables, rather than data with fewer variables.
#' 
#' @section Reported information:
#' Reported from the EDA is as follows.
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \item Data Types
#'     \item Job Informations
#'   }
#'   \item Univariate Analysis
#'   \itemize{
#'     \item Descriptive Statistics
#'     \item Normality Test
#'   }   
#'   \item Bivariate Analysis
#'   \itemize{
#'     \item Compare Numerical Variables
#'     \item Compare Categorical Variables
#'   }
#'   \item Multivariate Analysis
#'   \itemize{
#'     \item Correlation Analysis
#'     \itemize{
#'       \item Correlation Matrix
#'       \item Correlation Plot
#'     }
#'   }
#'   \item Target based Analysis
#'   \itemize{
#'     \item Grouped Numerical Variables
#'     \item Grouped Categorical Variables
#'     \item Grouped Correlation
#'   }
#' }
#'
#' @param .data a tbl_dbi.
#' @param target character. target variable.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "EDA Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param title_color character. color of title. default is "gray".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing EDA. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param as_factor logical. whether to convert to factor when importing a character type variable from DBMS table into R.
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{eda_web_report.data.frame}}.
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
#' if (!require(DBI)) install.packages('DBI', repos = "http://cran.us.r-project.org")
#' if (!require(RSQLite)) install.packages('RSQLite', repos = "http://cran.us.r-project.org")
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure2 to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure2, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_web_report(target = "death_event")
#'   
#' # create pdf file. file name is EDA.html, and collect size is 250
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_web_report(collect_size = 250, output_file = "EDA.html")
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' }
#' 
#' @method eda_web_report tbl_dbi
#' @export
#' 
eda_web_report.tbl_dbi <- function(.data, target = NULL, output_file = NULL, 
                                   output_dir = tempdir(), browse = TRUE, 
                                   title = "EDA", subtitle = deparse(substitute(.data)), 
                                   author = "dlookr", title_color = "gray", logo_img = NULL, 
                                   create_date = Sys.time(), theme = c("orange", "blue")[1], 
                                   sample_percent = 100, in_database = FALSE, 
                                   collect_size = Inf, as_factor = TRUE, ...) {
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    raw_data <- collect(.data, n = collect_size)
    
    if (as_factor) {
      raw_data <- raw_data %>% 
        mutate_if(is.character, as.factor)
    }
    
    eda_web_report(raw_data, target = target, output_file = output_file, 
                   output_dir = output_dir, browse = browse, title = title,
                   title_color = title_color, logo_img = logo_img, 
                   create_date = create_date, theme = theme, 
                   subtitle = dbplyr::remote_name(.data) %>% as.character(), author = author,
                   sample_percent = sample_percent, is_tbl_dbi = TRUE)
  }  
}


#' Reporting the information of EDA for table of the DBMS
#'
#' @description The eda_paged_report() paged report the information for EDA of the DBMS table through tbl_dbi
#'
#' @details Generate generalized EDA report automatically. 
#' You can choose to output to pdf and html files.
#' This feature is useful for EDA of data with many variables, 
#' rather than data with fewer variables.
#' 
#' Create an  PDF through the Chrome DevTools Protocol. If you want to create PDF, 
#' Google Chrome or Microsoft Edge (or Chromium on Linux) must be installed prior to using this function.
#' If not installed, you must use output_format = "html".
#' 
#' @section Reported information:
#' The EDA process will report the following information:
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \item Job Informations
#'   } 
#'   \item Univariate Analysis
#'   \itemize{
#'     \item Descriptive Statistics
#'     \itemize{
#'       \item Numerical Variables
#'       \item Categorical Variables
#'     }
#'     \item Normality Test
#'   } 
#'   \item Bivariate Analysis
#'   \itemize{
#'     \item Compare Numerical Variables
#'     \item Compare Categorical Variables
#'   } 
#'   \item Multivariate Analysis
#'   \itemize{
#'     \item Correlation Analysis
#'     \itemize{
#'       \item Correlation Coefficient Matrix
#'       \item Correlation Plot
#'     }
#'   }
#'   \item Target based Analysis
#'   \itemize{
#'     \item Grouped Numerical Variables
#'     \item Grouped Categorical Variables
#'     \item Grouped Correlation
#'   }
#' }
#'
#' @param .data a tbl_dbi.
#' @param target character. target variable.
#' @param in_database Specifies whether to perform in-database operations. 
#' If TRUE, most operations are performed in the DBMS. if FALSE, 
#' table data is taken in R and operated in-memory. Not yet supported in_database = TRUE.
#' @param collect_size a integer. The number of data samples from the DBMS to R. 
#' Applies only if in_database = FALSE.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by rmarkdown::render() and pagedown::chrome_print(). so, 
#' you needed Chrome web browser on computer.  
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param abstract_title character. abstract title of report. default is 
#' "Report Overview".
#' @param abstract character. abstract of report. 
#' @param title_color character. color of title. default is "black".
#' @param subtitle_color character. color of title. default is "blue".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param cover_img character. name of cover image. 
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing EDA. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param as_factor logical. whether to convert to factor when importing a character type variable from DBMS table into R.
#' @param ... arguments to be passed to pagedown::chrome_print().
#' 
#' @seealso \code{\link{eda_paged_report.data.frame}}.
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
#' if (!require(DBI)) install.packages('DBI', repos = "http://cran.us.r-project.org")
#' if (!require(RSQLite)) install.packages('RSQLite', repos = "http://cran.us.r-project.org")
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy heartfailure2 to the DBMS with a table named TB_HEARTFAILURE
#' copy_to(con_sqlite, heartfailure2, name = "TB_HEARTFAILURE", overwrite = TRUE)
#'
#' # reporting the diagnosis information -------------------------
#' # create pdf file. file name is EDA_Paged_Report.pdf
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_paged_report(target = "death_event")
#'   
#' # create pdf file. file name is EDA.pdf, and collect size is 250
#' con_sqlite %>% 
#'   tbl("TB_HEARTFAILURE") %>% 
#'   eda_paged_report(collect_size = 250, output_file = "EDA.pdf")
#'   
#' # Disconnect DBMS   
#' DBI::dbDisconnect(con_sqlite)
#' }
#' }
#' 
#' @method eda_paged_report tbl_dbi
#' @export
#' 
eda_paged_report.tbl_dbi <- function(.data, target = NULL, output_format = c("pdf", "html")[1],
                                     output_file = NULL, output_dir = tempdir(),   
                                     browse = TRUE, title = "EDA Report",
                                     subtitle = deparse(substitute(.data)), author = "dlookr",
                                     abstract_title = "Report Overview", abstract = NULL,
                                     title_color = "black", subtitle_color = "blue",
                                     cover_img = NULL, create_date = Sys.time(),
                                     logo_img = NULL, theme = c("orange", "blue")[1],
                                     sample_percent = 100, in_database = FALSE, 
                                     collect_size = Inf, as_factor = TRUE, ...) {
  if (in_database) {
    stop("It does not yet support in-database mode. Use in_database = FALSE.")
  } else {
    raw_data <- collect(.data, n = collect_size)
    
    if (as_factor) {
      raw_data <- raw_data %>% 
        mutate_if(is.character, as.factor)
    }
  
    if (output_format == "pdf") {
      args <- list(...)
      
      if ("extra_args" %in% names(args)) {
        extra_args <- args$extra_args
      } else {
        extra_args <- c("--disable-gpu")
      }       
    }
    
    eda_paged_report(raw_data, target = target, 
                     output_format = output_format, output_file = output_file, 
                     output_dir = output_dir, browse = browse, title = title,
                     subtitle = dbplyr::remote_name(.data) %>% as.character(), 
                     author = author, abstract_title = abstract_title, 
                     abstract = abstract,
                     title_color = title_color, subtitle_color = subtitle_color,
                     cover_img = cover_img, create_date = create_date, 
                     logo_img = logo_img, theme = theme, 
                     sample_percent = sample_percent, is_tbl_dbi = TRUE,
                     extra_args = extra_args)
  }  
}
