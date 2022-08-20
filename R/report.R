#' @rdname diagnose_web_report.data.frame
#' @export
diagnose_web_report <- function(.data, ...) {
  UseMethod("diagnose_web_report")
}


#' @rdname diagnose_paged_report.data.frame
#' @export
diagnose_paged_report <- function(.data, ...) {
  UseMethod("diagnose_paged_report")
}


#' @rdname eda_web_report.data.frame
#' @export
eda_web_report <- function(.data, ...) {
  UseMethod("eda_web_report")
}


#' @rdname eda_paged_report.data.frame
#' @export
eda_paged_report <- function(.data, ...) {
  UseMethod("eda_paged_report")
}


#' Reporting the information of data diagnosis with html
#'
#' @description The diagnose_web_report() report the information for diagnosing
#' the quality of the data.
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
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font(). 
#' 
#' @seealso \code{\link{diagnose_web_report.tbl_dbi}}.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
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
#' @param logo_img character. name of logo image file on top left.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param is_tbl_dbi logical. whether .data is a tbl_dbi object.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \donttest{
#' if (FALSE) {
#' # create dataset
#' heartfailure2 <- dlookr::heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 2), "time"] <- 0
#' heartfailure2[sample(seq(NROW(heartfailure2)), 1), "creatinine"] <- -0.3
#'  
#' # create pdf file. file name is Diagnosis_Report.html
#' diagnose_web_report(heartfailure2)
#' 
#' # file name is Diagn.html. and change logo image
#' logo <- file.path(system.file(package = "dlookr"), "report", "R_logo_html.svg")
#' diagnose_web_report(heartfailure2, logo_img = logo, title_color = "black",
#'   output_file = "Diagn.html")
#'
#' # file name is ./Diagn_heartfailure.html, "blue" theme and not browse
#' diagnose_web_report(heartfailure2, output_dir = ".", author = "Choonghyun Ryu",
#'   output_file = "Diagn_heartfailure.html", theme = "blue", browse = FALSE)
#' }
#' }
#' @importFrom rmarkdown render
#' @importFrom knitr image_uri
#' @method diagnose_web_report data.frame
#' @export
diagnose_web_report.data.frame <- function(.data, output_file = NULL, output_dir = tempdir(),   
                            browse = TRUE, title = "Data Diagnosis",
                            subtitle = deparse(substitute(.data)), author = "dlookr",
                            title_color = "gray", thres_uniq_cat = 0.5, 
                            thres_uniq_num = 5, logo_img = NULL, 
                            create_date = Sys.time(),
                            theme = c("orange", "blue"), 
                            sample_percent = 100, is_tbl_dbi = FALSE, 
                            base_family = NULL, ...) {
  theme <- match.arg(theme)
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  assign("reportData", as.data.frame(.data), .dlookrEnv)
  assign("thres_uniq_cat", thres_uniq_cat, .dlookrEnv)  
  assign("thres_uniq_num", thres_uniq_num, .dlookrEnv) 
  assign("sample_percent", sample_percent, .dlookrEnv)  
  assign("author", author, .dlookrEnv)  
  assign("base_family", base_family, .dlookrEnv)  
  
  path <- output_dir
  
  rmd   <- "diagnosis_temp.Rmd"
  header <- "header_temp.html"
  logo  <- "dlookr_html.svg"  
  
  if (is.null(output_file))
    output_file <- "Diagnosis_Report.html"
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy header  
  header_file <- file.path(system.file(package = "dlookr"), "report", header)
  flag <- file.copy(from = header_file, to = path, recursive = TRUE)  
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- gsub("\\$theme\\$", theme, 
                      readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  if (theme == "orange") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightorange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-orange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  } else if (theme == "blue") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightblue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-blue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")        
  }   

  # store title
  header_content <- sub("\\$title\\$", title, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store the menus
  menus <- "<li><a href=\"#ID-h1-overview\">Overview</a></li>
            <li><a href=\"#ID-h1-missing\">Missing Values</a></li>	
            <li><a href=\"#ID-h1-uniq-value\">Unique Values</a></li>	
            <li><a href=\"#ID-h1-outlier\">Outliers</a></li>
            <li><a href=\"#ID-h1-sample\">Samples</a></li>"
  header_content <- sub("\\$menu\\$", menus, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", ifelse(is_tbl_dbi, subtitle, deparse(substitute(.data))), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store dataset type
  rmd_content <- sub("\\$datatype\\$", ifelse(is_tbl_dbi, "tbl_dbi", class(.data)[1]), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookr"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- sub("\\$logo\\$", base64_logo,
                     readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  #--Rendering------------------------------------------------------------------    
  grDevices::graphics.off()
  rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)

  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, header, sep = "/"))  
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}


#' Reporting the information of data diagnosis
#'
#' @description The diagnose_paged_report() paged report the information 
#' for diagnosing the quality of the data.
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
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font(). 
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
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
#' @param subtitle_color character. color of subtitle. default is "gold".
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
#' @param logo_img character. name of logo image file on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param is_tbl_dbi logical. whether .data is a tbl_dbi object.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to pagedown::chrome_print().
#' 
#' @seealso \code{\link{diagnose_paged_report.tbl_dbi}}.
#' @examples
#' \donttest{
#' if (FALSE) {
#' # create dataset
#' heartfailure2 <- dlookr::heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 2), "time"] <- 0
#' heartfailure2[sample(seq(NROW(heartfailure2)), 1), "creatinine"] <- -0.3
#'  
#' # create pdf file. file name is Diagnosis_Paged_Report.pdf
#' diagnose_paged_report(heartfailure2)
#' 
#' # create pdf file. file name is Diagn.pdf. and change cover image
#' cover <- file.path(system.file(package = "dlookr"), "report", "cover2.jpg")
#' diagnose_paged_report(heartfailure2, cover_img = cover, title_color = "gray",
#'   output_file = "Diagn.pdf")
#'
#' # create pdf file. file name is ./Diagn.pdf and not browse
#' cover <- file.path(system.file(package = "dlookr"), "report", "cover3.jpg")
#' diagnose_paged_report(heartfailure2, output_dir = ".", cover_img = cover, 
#'   flag_content_missing = FALSE, output_file = "Diagn.pdf", browse = FALSE)
#' 
#' # create pdf file. file name is Diagnosis_Paged_Report.html
#' diagnose_paged_report(heartfailure2, output_format = "html")
#' }
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print find_chrome
#' @importFrom knitr image_uri
#' @method diagnose_paged_report data.frame
#' @export
diagnose_paged_report.data.frame <- function(.data, output_format = c("pdf", "html"),
                           output_file = NULL, output_dir = tempdir(),   
                           browse = TRUE, title = "Data Diagnosis Report",
                           subtitle = deparse(substitute(.data)), author = "dlookr",
                           abstract_title = "Report Overview", abstract = NULL,
                           title_color = "white", subtitle_color = "gold",
                           thres_uniq_cat = 0.5, thres_uniq_num = 5,
                           flag_content_zero = TRUE, flag_content_minus = TRUE,
                           flag_content_missing = TRUE, cover_img = NULL, 
                           create_date = Sys.time(),
                           logo_img = NULL, theme = c("orange", "blue"),
                           sample_percent = 100, is_tbl_dbi = FALSE, 
                           base_family = NULL, ...) {
  output_format <- match.arg(output_format)
  theme <- match.arg(theme)
  
  if (output_format == "pdf") {
    args <- list(...)
    
    browser <- pagedown::find_chrome()    
  }
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  assign("reportData", as.data.frame(.data), .dlookrEnv)
  assign("thres_uniq_cat", thres_uniq_cat, .dlookrEnv)  
  assign("thres_uniq_num", thres_uniq_num, .dlookrEnv) 
  assign("sample_percent", sample_percent, .dlookrEnv)  
  assign("author", author, .dlookrEnv)  
  assign("base_family", base_family, .dlookrEnv)  
  
  path <- output_dir
  
  rmd   <- "diagnosis_paged_temp.Rmd"
  html  <- "diagnosis_paged_temp.html"
  cover <- "cover1.jpg"
  logo  <- "dlookr.svg"  
  
  if (is.null(output_file))
    output_file <- paste("Diagnosis_Paged_Report", output_format, sep = ".")
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- sub("\\$theme\\$", theme, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store title
  rmd_content <- sub("\\$title\\$", title, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle
  rmd_content <- sub("\\$subtitle\\$", subtitle, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store abstract-title
  rmd_content <- sub("\\$abstract_title\\$", abstract_title, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store abstract
  if (is.null(abstract)) {
    abstract <- sprintf("This report was created for an overview quality 
                        diagnosis of ***%s*** data. It was created for **the 
                        purpose of judging the validity of variables** before 
                        conducting EDA.", deparse(substitute(.data)))
  }
  rmd_content <- sub("\\$abstract\\$", abstract, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle color
  subtitle_color <- col2hex(subtitle_color)
  rmd_content <- sub("\\$subtitle_color\\$", subtitle_color, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", ifelse(is_tbl_dbi, subtitle, deparse(substitute(.data))), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store dataset type
  rmd_content <- sub("\\$datatype\\$", ifelse(is_tbl_dbi, "tbl_dbi", class(.data)[1]), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookr"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  rmd_content <- sub("\\$logo\\$", base64_logo, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store cover image
  if (is.null(cover_img)) {
    cover_file <- file.path(system.file(package = "dlookr"), "report", cover)
    base64_cover <- knitr::image_uri(cover_file)
  } else {
    base64_cover <- knitr::image_uri(cover_img)
  }
  rmd_content <- sub("\\$cover\\$", base64_cover, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  
  #--Set contents  -------------------------------------------------------------  
  # missing value contents
  if (flag_content_missing) {
    rmd_content <- gsub("\\$content_missing\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$content_missing\\$[[:print:][:space:]]+\\$content_missing\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  # zero contents
  if (flag_content_zero) {
    rmd_content <- gsub("\\$content_zero\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$content_zero\\$[[:print:][:space:]]+\\$content_zero\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  # minus contents
  if (flag_content_minus) {
    rmd_content <- gsub("\\$content_minus\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$content_minus\\$[[:print:][:space:]]+\\$content_minus\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  if (output_format == "pdf") {
    # for non-ASCII in MS-Windows 
    if (grepl("^mingw", R.version$os)) { 
      writeLines(iconv(readLines(paste(path, rmd, sep = "/")), from = "CP949", to = "UTF8"), 
                 file(paste(path, rmd, sep = "/"), encoding="UTF-8"))
    }
    
    html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
    
    if ("extra_args" %in% names(args)) {
      extra_args <- args$extra_args
    } else {
      extra_args <- c("--disable-gpu")
    }
    pagedown::chrome_print(html_out, output = output_file, extra_args = extra_args)
    
    file.remove(paste(path, html, sep = "/"))
  } else {
    rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  }
  
  file.remove(paste(path, rmd, sep = "/"))
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}



#' Reporting the information of EDA with html
#'
#' @description The eda_web_report() report the information of exploratory 
#' data analysis for object inheriting from data.frame.
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
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target character. target variable.
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "EDA".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param title_color character. color of title. default is "gray".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param logo_img character. name of logo image file on top left.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing EDA. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param is_tbl_dbi logical. whether .data is a tbl_dbi object.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods.
#'
#' @seealso \code{\link{eda_web_report.tbl_dbi}}.
#' @examples
#' \donttest{
#' if (FALSE) {
#' # create the dataset
#' heartfailure2 <- dlookr::heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' 
#' # create html file. file name is EDA_Report.html
#' eda_web_report(heartfailure2)
#' 
#' # file name is EDA.html. and change logo image
#' logo <- file.path(system.file(package = "dlookr"), "report", "R_logo_html.svg")
#' eda_web_report(heartfailure2, logo_img = logo, title_color = "black",
#'   output_file = "EDA.html")
#'
#' # file name is ./EDA_heartfailure.html, "blue" theme and not browse
#' eda_web_report(heartfailure2, target = "death_event", output_dir = ".", 
#'   author = "Choonghyun Ryu", output_file = "EDA_heartfailure.html", 
#'   theme = "blue", browse = FALSE)
#' }
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom knitr image_uri
#' @importFrom tidyselect vars_select
#' @method eda_web_report data.frame
#' @export
eda_web_report.data.frame <- function(.data, target = NULL, output_file = NULL, 
                       output_dir = tempdir(), browse = TRUE, 
                       title = "EDA", subtitle = deparse(substitute(.data)), 
                       author = "dlookr", title_color = "gray", logo_img = NULL, 
                       create_date = Sys.time(), theme = c("orange", "blue"), 
                       sample_percent = 100, is_tbl_dbi = FALSE, 
                       base_family = NULL, ...) {
  theme <- match.arg(theme)
  
  pram <- as.character(substitute(target))
  
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             # pram <- as.character(substitute(target))
             stop(sprintf("The target variable '%s' does not exist.", pram))
           },
           finally = NULL)

  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  assign("reportData", as.data.frame(.data), .dlookrEnv)
  assign("targetVariable", vars, .dlookrEnv)
  assign("sample_percent", sample_percent, .dlookrEnv)  
  assign("author", author, .dlookrEnv)  
  assign("base_family", base_family, .dlookrEnv) 
  
  path <- output_dir
  
  rmd   <- "eda_temp.Rmd"
  header <- "header_temp.html"
  logo  <- "dlookr_html.svg"  
  
  if (is.null(output_file))
    output_file <- "EDA_Report.html"
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy header  
  header_file <- file.path(system.file(package = "dlookr"), "report", header)
  flag <- file.copy(from = header_file, to = path, recursive = TRUE)  
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- gsub("\\$theme\\$", theme, 
                      readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  if (theme == "orange") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightorange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-orange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  } else if (theme == "blue") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightblue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-blue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")        
  }  
  
  # store title
  header_content <- sub("\\$title\\$", title, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store the menus
  disabled <- ifelse(is.null(target), 
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-univariate\">Univariate</a></li>	
                      <li><a href=\"#ID-h1-bivariate\">Bivariate</a></li>	
                      <li><a href=\"#ID-h1-multivariate\">Multivariate</a></li>
                      <li><a href=\"#\" class=\"disable-links\">Target based</a></li>",
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-univariate\">Univariate</a></li>	
                      <li><a href=\"#ID-h1-bivariate\">Bivariate</a></li>	
                      <li><a href=\"#ID-h1-multivariate\">Multivariate</a></li>
                      <li><a href=\"#ID-h1-target-based\">Target based</a></li>")
  header_content <- sub("\\$menu\\$", disabled, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")

  # store dataset
  rmd_content <- sub("\\$dataset\\$", ifelse(is_tbl_dbi, subtitle, deparse(substitute(.data))), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store dataset type
  rmd_content <- sub("\\$datatype\\$", ifelse(is_tbl_dbi, "tbl_dbi", class(.data)[1]), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                        readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookr"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- sub("\\$logo\\$", base64_logo,
                        readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  #--Rendering------------------------------------------------------------------    
  grDevices::graphics.off()
  rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  
  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, header, sep = "/"))  
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}



#' Reporting the information of EDA
#'
#' @description The eda_paged_report() paged report the information for EDA.
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
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target character. target variable.
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
#' @param subtitle_color character. color of subtitle. default is "blue".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param cover_img character. name of cover image. 
#' @param logo_img character. name of logo image file on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param is_tbl_dbi logical. whether .data is a tbl_dbi object.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details) 
#' @param ... arguments to be passed to pagedown::chrome_print().
#' 
#' @seealso \code{\link{eda_paged_report.tbl_dbi}}.
#' @examples
#' \donttest{
#' if (FALSE) {
#' # create the dataset
#' heartfailure2 <- dlookr::heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' 
#' # create pdf file. file name is EDA_Paged_Report.pdf
#' eda_paged_report(heartfailure2, sample_percent = 80)
#' 
#' # create pdf file. file name is EDA.pdf. and change cover image
#' cover <- file.path(system.file(package = "dlookr"), "report", "cover1.jpg")
#' eda_paged_report(heartfailure2, cover_img = cover, title_color = "gray",
#'   output_file = "EDA.pdf")
#'
#' # create pdf file. file name is ./EDA.pdf and not browse
#' cover <- file.path(system.file(package = "dlookr"), "report", "cover3.jpg")
#' eda_paged_report(heartfailure2, output_dir = ".", cover_img = cover, 
#'   flag_content_missing = FALSE, output_file = "EDA.pdf", browse = FALSE)
#' 
#' # create pdf file. file name is EDA_Paged_Report.html
#' eda_paged_report(heartfailure2, target = "death_event", output_format = "html")
#' }
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print find_chrome
#' @importFrom knitr image_uri
#' @method eda_paged_report data.frame
#' @export
eda_paged_report.data.frame <- function(.data, target = NULL, output_format = c("pdf", "html"),
                             output_file = NULL, output_dir = tempdir(),   
                             browse = TRUE, title = "EDA Report",
                             subtitle = deparse(substitute(.data)), author = "dlookr",
                             abstract_title = "Report Overview", abstract = NULL,
                             title_color = "black", subtitle_color = "blue",
                             cover_img = NULL, create_date = Sys.time(),
                             logo_img = NULL, theme = c("orange", "blue"),
                             sample_percent = 100, is_tbl_dbi = FALSE, 
                             base_family = NULL, ...) {
  output_format <- match.arg(output_format)
  theme <- match.arg(theme)
  
  if (output_format == "pdf") {
    args <- list(...)
    
    browser <- pagedown::find_chrome()    
  }
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  pram <- as.character(substitute(target))
  
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             # pram <- as.character(substitute(target))
             stop(sprintf("The target variable '%s' does not exist.", pram))
           },
           finally = NULL)
  
  assign("reportData", as.data.frame(.data), .dlookrEnv)
  assign("targetVariable", vars, .dlookrEnv)  
  assign("sample_percent", sample_percent, .dlookrEnv)  
  assign("author", author, .dlookrEnv)  
  assign("base_family", base_family, .dlookrEnv) 
  
  path <- output_dir
  
  rmd   <- "eda_paged_temp.Rmd"
  html  <- "eda_paged_temp.html"
  cover <- "cover2.jpg"
  logo  <- "dlookr.svg"  
  
  if (is.null(output_file))
    output_file <- paste("EDA_Paged_Report", output_format, sep = ".")
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- sub("\\$theme\\$", theme, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store title
  rmd_content <- sub("\\$title\\$", title, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle
  rmd_content <- sub("\\$subtitle\\$", subtitle, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store abstract-title
  rmd_content <- sub("\\$abstract_title\\$", abstract_title, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store abstract
  if (is.null(abstract)) {
    abstract <- sprintf("This report was created for the EDA of ***%s*** data. 
                        It helps explore data to **understand the data 
                        and find scenarios for performing the analysis.**", 
                        deparse(substitute(.data)))
  }
  rmd_content <- sub("\\$abstract\\$", abstract, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle color
  subtitle_color <- col2hex(subtitle_color)
  rmd_content <- sub("\\$subtitle_color\\$", subtitle_color, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", ifelse(is_tbl_dbi, subtitle, deparse(substitute(.data))), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store dataset type
  rmd_content <- sub("\\$datatype\\$", ifelse(is_tbl_dbi, "tbl_dbi", class(.data)[1]), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookr"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  rmd_content <- sub("\\$logo\\$", base64_logo, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store cover image
  if (is.null(cover_img)) {
    cover_file <- file.path(system.file(package = "dlookr"), "report", cover)
    base64_cover <- knitr::image_uri(cover_file)
  } else {
    base64_cover <- knitr::image_uri(cover_img)
  }
  rmd_content <- sub("\\$cover\\$", base64_cover, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  
  # targeted contents
  if (!is.null(target)) {
    rmd_content <- gsub("\\$targeted_eda\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$targeted_eda\\$[[:print:][:space:]]+\\$targeted_eda\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  if (output_format == "pdf") {
    # for non-ASCII in MS-Windows 
    if (grepl("^mingw", R.version$os)) { 
      writeLines(iconv(readLines(paste(path, rmd, sep = "/")), from = "CP949", to = "UTF8"), 
                 file(paste(path, rmd, sep = "/"), encoding="UTF-8"))
    }
    
    html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
    
    if ("extra_args" %in% names(args)) {
      extra_args <- args$extra_args
    } else {
      extra_args <- c("--disable-gpu")
    }
    pagedown::chrome_print(html_out, output = output_file, extra_args = extra_args)
    
    file.remove(paste(path, html, sep = "/"))
  } else {
    rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  }
  
  file.remove(paste(path, rmd, sep = "/"))
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}



#' Reporting the information of transformation with html
#'
#' @description The transformation_web_report() report the information of 
#' transform numerical variables for object inheriting from data.frame.
#'
#' @details Generate transformation reports automatically. 
#' This is useful for Binning a data frame with a large number of variables 
#' than data with a small number of variables.
#' 
#' @section Reported information:
#' The transformation process will report the following information:
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \item Data Types
#'     \item Job Informations
#'   }
#'   \item Imputation
#'   \itemize{
#'     \item Missing Values
#'     \item Outliers
#'   }   
#'   \item Resolving Skewness
#'   \item Binning
#'   \item Optimal Binning
#' }
#' 
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target character. target variable.
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "EDA Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param title_color character. color of title. default is "gray".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param logo_img character. name of logo image file on top left.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing EDA. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \donttest{
#' if (FALSE) {
#' # create html file. file name is Transformation_Report.html
#' transformation_web_report(heartfailure)
#' 
#' # file name is Transformation.html. and change logo image
#' logo <- file.path(system.file(package = "dlookr"), "report", "R_logo_html.svg")
#' transformation_web_report(heartfailure, logo_img = logo, title_color = "black",
#'   output_file = "Transformation.html")
#'
#' # file name is ./Transformation.html, "blue" theme and not browse
#' transformation_web_report(heartfailure, output_dir = ".", target = "death_event", 
#'   author = "Choonghyun Ryu", output_file = "Transformation.html", 
#'   theme = "blue", browse = FALSE)
#' }
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom knitr image_uri
#' @export
transformation_web_report <- function(.data, target = NULL, output_file = NULL, 
                       output_dir = tempdir(), browse = TRUE, 
                       title = "Transformation", subtitle = deparse(substitute(.data)), 
                       author = "dlookr", title_color = "gray", logo_img = NULL, 
                       create_date = Sys.time(), theme = c("orange", "blue"), 
                       sample_percent = 100, base_family = NULL, ...) {
  theme <- match.arg(theme)
  
  pram <- as.character(substitute(target))
  
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             # pram <- as.character(substitute(target))
             stop(sprintf("The target variable '%s' does not exist.", pram))
           },
           finally = NULL)
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  assign("reportData", as.data.frame(.data), .dlookrEnv)
  assign("targetVariable", vars, .dlookrEnv)
  assign("sample_percent", sample_percent, .dlookrEnv)  
  assign("author", author, .dlookrEnv)  
  assign("base_family", base_family, .dlookrEnv) 
  
  path <- output_dir
  
  rmd   <- "transformation_temp.Rmd"
  header <- "header_temp.html"
  logo  <- "dlookr_html.svg"  
  
  if (is.null(output_file))
    output_file <- "Transformation_Report.html"
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy header  
  header_file <- file.path(system.file(package = "dlookr"), "report", header)
  flag <- file.copy(from = header_file, to = path, recursive = TRUE)  
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- gsub("\\$theme\\$", theme, 
                      readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  if (theme == "orange") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightorange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-orange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  } else if (theme == "blue") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightblue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-blue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")        
  }  
  
  # store title
  header_content <- sub("\\$title\\$", title, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store the menus
  disabled <- ifelse(is.null(target), 
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-imputation\">Imputation</a></li>	
                      <li><a href=\"#ID-h1-skewness\">Resolving Skewness</a></li>	
                      <li><a href=\"#ID-h1-binning\">Binning</a></li>	
                      <li><a href=\"#\" class=\"disable-links\">Optimal Binning</a></li>",
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-imputation\">Imputation</a></li>	
                      <li><a href=\"#ID-h1-skewness\">Resolving Skewness</a></li>	
                      <li><a href=\"#ID-h1-binning\">Binning</a></li>	
                      <li><a href=\"#ID-h1-optimal-binning\">Optimal Binning</a></li>")
  header_content <- sub("\\$menu\\$", disabled, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookr"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- gsub("\\$logo\\$", base64_logo,
                        readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  #--Rendering------------------------------------------------------------------    
  grDevices::graphics.off()  
  rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  
  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, header, sep = "/"))  
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}


#' Reporting the information of transformation
#'
#' @description The eda_paged_report() paged report the information for data transformatiom.
#'
#' @details Generate transformation reports automatically. 
#' You can choose to output to pdf and html files.
#' This is useful for Binning a data frame with a large number of variables 
#' than data with a small number of variables.
#' 
#' Create an  PDF through the Chrome DevTools Protocol. If you want to create PDF, 
#' Google Chrome or Microsoft Edge (or Chromium on Linux) must be installed prior to using this function.
#' If not installed, you must use output_format = "html".
#' 
#' @section Reported information:
#' TThe transformation process will report the following information:
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \item Job Informations
#'   } 
#'   \item Imputation
#'   \itemize{
#'     \item Missing Values
#'     \item Outliers
#'   } 
#'   \item Resolving Skewness
#'   \item Binning
#'   \item Optimal Binning
#' }
#'
#' The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target character. target variable.
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
#' @param subtitle_color character. color of subtitle. default is "tomato1".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param cover_img character. name of cover image. 
#' @param logo_img character. name of logo image file on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5\% of sample data.
#' This is useful for data with a large number of observations.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to pagedown::chrome_print().
#' 
#' @examples
#' \donttest{
#' if (FALSE) {
#' # create pdf file. file name is Transformation_Paged_Report.pdf
#' transformation_paged_report(heartfailure, sample_percent = 80)
#' 
#' # create pdf file. file name is Transformation_heartfailure. and change cover image
#' cover <- file.path(system.file(package = "dlookr"), "report", "cover2.jpg")
#' transformation_paged_report(heartfailure, cover_img = cover, title_color = "gray",
#'   output_file = "Transformation_heartfailure")
#'
#' # create pdf file. file name is ./Transformation.pdf and not browse
#' cover <- file.path(system.file(package = "dlookr"), "report", "cover1.jpg")
#' transformation_paged_report(heartfailure, output_dir = ".", cover_img = cover, 
#'   flag_content_missing = FALSE, output_file = "Transformation.pdf", browse = FALSE)
#' 
#' # create pdf file. file name is Transformation_Paged_Report.html
#' transformation_paged_report(heartfailure, target = "death_event", output_format = "html")
#' }
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print find_chrome
#' @importFrom knitr image_uri
#' @export
transformation_paged_report <- function(.data, target = NULL, 
                             output_format = c("pdf", "html"),
                             output_file = NULL, output_dir = tempdir(),   
                             browse = TRUE, title = "Transformation Report",
                             subtitle = deparse(substitute(.data)), author = "dlookr",
                             abstract_title = "Report Overview", abstract = NULL,
                             title_color = "white", subtitle_color = "tomato1",
                             cover_img = NULL, create_date = Sys.time(),
                             logo_img = NULL, theme = c("orange", "blue"),
                             sample_percent = 100, base_family = NULL, ...) {

  output_format <- match.arg(output_format)
  theme <- match.arg(theme)
  
  if (output_format == "pdf") {
    args <- list(...)
    
    browser <- pagedown::find_chrome()    
  }
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  pram <- as.character(substitute(target))
  
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             # pram <- as.character(substitute(target))
             stop(sprintf("The target variable '%s' does not exist.", pram))
           },
           finally = NULL)
  
  assign("reportData", as.data.frame(.data), .dlookrEnv)
  assign("targetVariable", vars, .dlookrEnv)  
  assign("sample_percent", sample_percent, .dlookrEnv)  
  assign("author", author, .dlookrEnv)
  assign("base_family", base_family, .dlookrEnv) 
  
  path <- output_dir
  
  rmd   <- "transformation_paged_temp.Rmd"
  html  <- "transformation_paged_temp.html"
  cover <- "cover3.jpg"
  logo  <- "dlookr.svg"  
  
  if (is.null(output_file))
    output_file <- paste("Transformation_Paged_Report", output_format, sep = ".")
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- sub("\\$theme\\$", theme, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store title
  rmd_content <- sub("\\$title\\$", title, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle
  rmd_content <- sub("\\$subtitle\\$", subtitle, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store abstract-title
  rmd_content <- sub("\\$abstract_title\\$", abstract_title, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store abstract
  if (is.null(abstract)) {
    abstract <- sprintf("This report was created for the transformation of ***%s*** data. 
                        It helps to  impute missing values and outliers, resolve skewed data, 
                        and categorize continuous variables into categorical variables.", 
                        deparse(substitute(.data)))
  }
  rmd_content <- sub("\\$abstract\\$", abstract, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle color
  subtitle_color <- col2hex(subtitle_color)
  rmd_content <- sub("\\$subtitle_color\\$", subtitle_color, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookr"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  rmd_content <- sub("\\$logo\\$", base64_logo, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store cover image
  if (is.null(cover_img)) {
    cover_file <- file.path(system.file(package = "dlookr"), "report", cover)
    base64_cover <- knitr::image_uri(cover_file)
  } else {
    base64_cover <- knitr::image_uri(cover_img)
  }
  rmd_content <- sub("\\$cover\\$", base64_cover, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  
  # targeted contents
  if (!is.null(target)) {
    rmd_content <- gsub("\\$targeted_eda\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$targeted_eda\\$[[:print:][:space:]]+\\$targeted_eda\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }  
  
  if (output_format == "pdf") {
    # for non-ASCII in MS-Windows 
    if (grepl("^mingw", R.version$os)) { 
      writeLines(iconv(readLines(paste(path, rmd, sep = "/")), from = "CP949", to = "UTF8"), 
                 file(paste(path, rmd, sep = "/"), encoding="UTF-8"))
    }
    
    html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
    
    if ("extra_args" %in% names(args)) {
      extra_args <- args$extra_args
    } else {
      extra_args <- c("--disable-gpu")
    }
    pagedown::chrome_print(html_out, output = output_file, extra_args = extra_args)
    
    file.remove(paste(path, html, sep = "/"))
  } else {
    rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  }
  
  file.remove(paste(path, rmd, sep = "/"))
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}


