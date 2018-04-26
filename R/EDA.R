#' Reporting the information of EDA
#'
#' @description The eda_report() report the information of Exploratory
#' data analysis for object inheriting from data.frame.
#'
#' @details Generate generalized data EDA reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for EDA a data frame with a large number of variables
#' than data with a small number of variables.
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
#'     \item Gruoped Descriptive Statistics
#'     \itemize{
#'       \item Gruoped Numerical Variables
#'       \item Gruoped Categorical Variables
#'     }
#'     \item Gruoped Relationship Between Variables
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
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#'
#' @examples
#' \donttest{
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
#' # create pdf file. file name is EDA.pdf
#' eda_report(carseats, "US", output_file = "EDA.pdf")
#' # create html file. file name is EDA_Report.html
#' eda_report(carseats, "US", output_format = "html")
#' # create html file. file name is EDA.html
#' eda_report(carseats, US, output_format = "html", output_file = "EDA.html")
#'
#' ## target variable is numerical variable
#' # reporting the EDA information
#' eda_report(carseats, Sales)
#' # create pdf file. file name is EDA2.pdf
#' eda_report(carseats, "Sales", output_file = "EDA2.pdf")
#' # create html file. file name is EDA_Report.html
#' eda_report(carseats, "Sales", output_format = "html")
#' # create html file. file name is EDA2.html
#' eda_report(carseats, Sales, output_format = "html", output_file = "EDA2.html")
#'
#' ## target variable is null
#' # reporting the EDA information
#' eda_report(carseats)
#' # create pdf file. file name is EDA2.pdf
#' eda_report(carseats, output_file = "EDA2.pdf")
#' # create html file. file name is EDA_Report.html
#' eda_report(carseats, output_format = "html")
#' # create html file. file name is EDA2.html
#' eda_report(carseats, output_format = "html", output_file = "EDA2.html")
#' }
#'
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom grDevices cairo_pdf
#' @importFrom xtable xtable
#' @importFrom moments skewness kurtosis
#' @importFrom prettydoc html_pretty
#' @importFrom kableExtra kable_styling
#' @importFrom utils browseURL
#'
#' @export
eda_report <- function(.data, target = NULL, output_format = c("pdf", "html"),
  output_file = NULL, output_dir = tempdir()) {
  tryCatch(vars <- tidyselect::vars_select(names(.data),
    !!! rlang::enquo(target)),
    error = function(e) {
      pram <- as.character(substitute(target))
      stop(sprintf("Column %s is unknown", pram))
    },
    finally = NULL)
  output_format <- match.arg(output_format)
  
  assign("edaData", as.data.frame(.data), .dlookrEnv)
  assign("targetVariable", vars, .dlookrEnv)
  
  path <- output_dir
  
  if (output_format == "pdf") {
    installed <- file.exists(Sys.which("pdflatex"))
    
    if (!installed) {
      stop("No TeX installation detected. Please install TeX before running.\nor Use output_format = \"html\"")
    }
    
    if (is.null(output_file))
      output_file <- "EDA_Report.pdf"
    
    Rnw_file <- file.path(system.file(package = "dlookr"), "report",
      "EDA_Report.Rnw")
    file.copy(from = Rnw_file, to = path)
    
    Rnw_file <- file.path(system.file(package = "dlookr"), "report",
      "02_RunEDA.Rnw")
    file.copy(from = Rnw_file, to = path)
    
    Img_file <- file.path(system.file(package = "dlookr"), "img")
    file.copy(from = Img_file, to = path, recursive = TRUE)
    
    dir.create(paste(path, "figure", sep = "/"))
    
    knitr::knit2pdf(paste(path, "EDA_Report.Rnw", sep = "/"), 
      compiler = "pdflatex",
      output = sub("pdf$", "tex", paste(path, output_file, sep = "/")))
    
    file.remove(paste(path, "02_RunEDA.Rnw", sep = "/"))
    file.remove(paste(path, "EDA_Report.Rnw", sep = "/"))
    
    fnames <- sub("pdf$", "", output_file)
    fnames <- grep(fnames, list.files(path), value = TRUE)
    fnames <- grep("\\.pdf$", fnames, invert = TRUE, value = TRUE)
    
    file.remove(paste(path, fnames, sep = "/"))
    
    unlink(paste(path, "figure", sep = "/"), recursive = TRUE)
    unlink(paste(path, "img", sep = "/"), recursive = TRUE)
  } else if (output_format == "html") {
    output_file <- "EDA_Report.html"
    
    Rmd_file <- file.path(system.file(package = "dlookr"), "report",
      "EDA_Report.Rmd")
    file.copy(from = Rmd_file, to = path, recursive = TRUE)
    
    rmarkdown::render(paste(path, "EDA_Report.Rmd", sep = "/"),
      prettydoc::html_pretty(toc = TRUE, number_sections = TRUE),
      output_file = paste(path, output_file, sep = "/"))
    
    file.remove(paste(path, "EDA_Report.Rmd", sep = "/"))
  }
  
  if (file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}
