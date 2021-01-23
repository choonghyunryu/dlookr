#' @rdname eda_report.data.frame
#' @export
eda_report <- function(.data, ...) {
  UseMethod("eda_report")
}


#' Reporting the information of EDA
#'
#' @description The eda_report() report the information of exploratory
#' data analysis for object inheriting from data.frame.
#'
#' @details Generate generalized EDA report automatically.
#' You can choose to output as pdf and html files.
#' This feature is useful for EDA of data with many variables, rather than data with fewer variables.
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
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target target variable.
#' @param output_format character. report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file character. name of generated file. default is NULL.
#' @param output_dir character. name of directory to generate report file. default is tempdir().
#' @param font_family character. font family name for figure in pdf.
#' @param browse logical. choose whether to output the report results to the browser.
#' @param ... arguments to be passed to methods.
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' ## target variable is categorical variable
#' # reporting the EDA information
#' # create pdf file. file name is EDA_Report.pdf
#' eda_report(carseats, US)
#' 
#' # create pdf file. file name is EDA_carseats.pdf
#' eda_report(carseats, "US", output_file = "EDA_carseats.pdf")
#' 
#' # create pdf file. file name is EDA_carseats.pdf and not browse
#' # eda_report(carseats, "US", output_dir = ".", output_file = "EDA_carseats.pdf", browse = FALSE)
#' 
#' # create html file. file name is EDA_Report.html
#' eda_report(carseats, "US", output_format = "html")
#' 
#' # create html file. file name is EDA_carseats.html
#' eda_report(carseats, US, output_format = "html", output_file = "EDA_carseats.html")
#'
#'
#' ## target variable is numerical variable
#' # reporting the EDA information
#' eda_report(carseats, Sales)
#' 
#' # create pdf file. file name is EDA2.pdf
#' eda_report(carseats, "Sales", output_file = "EDA2.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' eda_report(carseats, "Sales", output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' eda_report(carseats, Sales, output_format = "html", output_file = "EDA2.html")
#'
#' ## target variable is null
#' # reporting the EDA information
#' eda_report(carseats)
#' 
#' 
#' # create pdf file. file name is EDA2.pdf
#' eda_report(carseats, output_file = "EDA2.pdf")
#' 
#' # create html file. file name is EDA_Report.html
#' eda_report(carseats, output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' eda_report(carseats, output_format = "html", output_file = "EDA2.html")
#' }
#'
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom grDevices cairo_pdf
#' @importFrom prettydoc html_pretty
#' @importFrom kableExtra kable_styling
#' @importFrom utils browseURL
#' @method eda_report data.frame
#' @export
eda_report.data.frame <- function(.data, target = NULL, output_format = c("pdf", "html"),
  output_file = NULL, output_dir = tempdir(), font_family = NULL, browse = TRUE, ...) {
  tryCatch(vars <- tidyselect::vars_select(names(.data),
    !! rlang::enquo(target)),
    error = function(e) {
      pram <- as.character(substitute(target))
      stop(sprintf("Column %s is unknown", pram))
    },
    finally = NULL)
  output_format <- match.arg(output_format)
  
  assign("edaData", as.data.frame(.data), .dlookrEnv)
  assign("targetVariable", vars, .dlookrEnv)
  assign("font_family", font_family, .dlookrEnv)
  
  path <- output_dir
  if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
    latex_main <- "EDA_Report_KR.Rnw"
    latex_sub <- "02_RunEDA_KR.Rnw"
  } else {
    latex_main <- "EDA_Report.Rnw"
    latex_sub <- "02_RunEDA.Rnw"
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
      output_file <- "EDA_Report.pdf"
    
    Rnw_file <- file.path(system.file(package = "dlookr"), "report", latex_main)
    file.copy(from = Rnw_file, to = path)
    
    Rnw_file <- file.path(system.file(package = "dlookr"), "report", latex_sub)
    file.copy(from = Rnw_file, to = path)
    
    Img_file <- file.path(system.file(package = "dlookr"), "img")
    file.copy(from = Img_file, to = path, recursive = TRUE)
    
    dir.create(paste(path, "figure", sep = "/"))
    
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
      rmd <- "EDA_Report_KR.Rmd"
    } else {
      rmd <- "EDA_Report.Rmd"
    }
    
    if (is.null(output_file))
      output_file <- "EDA_Report.html"
    
    if (!is.null(font_family))
      assign("font_family", font_family, envir = .dlookrEnv)
    
    Rmd_file <- file.path(system.file(package = "dlookr"), "report", rmd)
    file.copy(from = Rmd_file, to = path, recursive = TRUE)
    
    rmarkdown::render(paste(path, rmd, sep = "/"),
      prettydoc::html_pretty(toc = TRUE, number_sections = TRUE),
      output_file = paste(path, output_file, sep = "/"))
    
    file.remove(paste(path, rmd, sep = "/"))
  }
  
  if (browse & file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}
