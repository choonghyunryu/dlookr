#' dlookr: Tools for Data Diagnosis, Exploration, Transformation
#'
#' dlookr provides data diagnosis, data exploration and transformation of variables
#' during data analysis.
#'
#' It has three main goals:
#'
#' \itemize{
#' \item When data is acquired, it is possible to judge whether data is erroneous
#' or to select a variable to be corrected or removed through data diagnosis.
#' \item Understand the distribution of data in the EDA process. It can also
#' understand the relationship between target variables and predictor variables
#' for the prediction model.
#' \item Imputes missing value and outlier to standardization and resolving skewness.
#' And, To convert a continuous variable to a categorical variable, bin the continuous variables.
#' }
#'
#' To learn more about dlookr, start with the vignettes:
#' `browseVignettes(package = "dlookr")`
#'
#' @import dplyr
#' @import extrafont
#' @import ggplot2
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "key", "value", "var1", "var2", "outlier",
    "outliers_cnt", "outliers_mean", "outliers_ratio", "p_value", "variable",
    "with_mean", "without_mean", "Q1", "Q3", "minus", "freq", "zero", "obs",
    "coef_corr", "method", "r.squared", "Var1", "Var2", "pos_x", "pos_y",
    "na_pct", "n_var", "name_var", "n_obs", "ratio", "txt", "name",
    "cumulative", "y", "frequencies", "A", "B", "a", "b", "AUC", "Bin", 
    "CntCumNeg", "CntCumPos", "CntNeg", "CntPos", "CntRec", "Freq", "LnOdds",
    "Odds", "PctPos", "PctRec", "REV", "RateCumNeg", "RateCumPos", "RateNeg",
    "RatePos", "WoE", "add_pos", "cnt", "datas", "diff_prim", "diff_sec",
    "flag", "indicator", "menas", "min_prim", "min_sec", "msg", "n_missing",
    "na.exclude", "na_flag", "object.size", "reformulate", "target", "x_end",
    "x_start", "x_width", "AIC", "BIC", "deviance", "logLik", "n_case"))
}

# Adapted from hrbrthemes package's .onAttach
.onAttach <- function(libname, pkgname) {
  load_fonts <- function() {
    if (.Platform$OS.type == "windows") { 
      msg <- "Registering Windows fonts with R for Viz"
      if (interactive()) 
        packageStartupMessage(msg)
    
      windowsFonts <- grDevices::windowsFonts
      extrafont::loadfonts("win", quiet = TRUE)
    } 
    
    msg <- "Registering PDF & PostScript fonts with R for Viz"
    if (interactive()) 
      packageStartupMessage(msg)
    
    pdfFonts <- grDevices::pdfFonts
    postscriptFonts <- grDevices::postscriptFonts
    
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }
  
  import_arial_narrow <- function() {
    family <- c("Arial Narrow", "Liberation Sans Narrow")
    font_tab <- extrafont::fonttable()
    
    find_font <- function(pattern) {
      # get from extrafont:::ttf_find_default_path(), because CRN not allow ::: operator
      get_ttf_paths <- function() {
        if (grepl("^darwin", R.version$os)) {
          paths <- c("/Library/Fonts/", "/System/Library/Fonts", 
                     "~/Library/Fonts/")
          return(paths[file.exists(paths)])
        }
        else if (grepl("^linux-gnu", R.version$os)) {
          paths <- c("/usr/share/fonts/", "/usr/X11R6/lib/X11/fonts/TrueType/", 
                     "~/.fonts/")
          return(paths[file.exists(paths)])
        }
        else if (grepl("^freebsd", R.version$os)) {
          paths <- c("/usr/local/share/fonts/truetype/", "/usr/local/lib/X11/fonts/", 
                     "~/.fonts/")
          return(paths[file.exists(paths)])
        }
        else if (grepl("^mingw", R.version$os)) {
          return(paste(Sys.getenv("SystemRoot"), "\\Fonts", sep = ""))
        }
        else if (grepl("^SunOS", R.version$os)) {
          paths <- c("/usr/share/fonts/", "/usr/X11/lib/X11/fonts/TrueType/", 
                     "~/.fonts/")
        }        
        else {
          #msg <- "Don't know where to look for truetype fonts."
          #packageStartupMessage(msg)
          return(character(0))
        }
      }
      
      font_path <- get_ttf_paths()
      font_files <- list.files(font_path, pattern = "\\.ttf$", full.names = TRUE, 
                               recursive = TRUE, ignore.case = TRUE) 
      grep(pattern, font_files, value = TRUE)
    }
    
    if (nrow(font_tab) > 0) {
      if(any(family %in% font_tab$FamilyName)) {
        #msg <- "Arial Narrow or similar fonts have already been imported."
        #packageStartupMessage(msg)
        return(FALSE)
      } 
    }
    
    pattern_an <- "Arial[ ]*Narrow|ARIALN"
    pattern_ln <- "Liberation[ ]*Sans[ ]*Narrow"
    
    font_an <- find_font(pattern_an)
    font_ln <- find_font(pattern_ln)
    
    if (length(font_an) > 0) {
      suppressWarnings(suppressMessages(extrafont::font_import(
        pattern = pattern_an, prompt = FALSE)))
      packageStartupMessage("Imported Arial Narrow fonts.")
      
      return(TRUE)
    } else if (length(font_ln) > 0) {
      suppressWarnings(suppressMessages(extrafont::font_import(
        pattern = pattern_ln, prompt = FALSE)))
      packageStartupMessage("Imported Liberation Sans Narrow fonts.")
      
      return(TRUE)
    } else {
      msg <- paste0(sprintf("Either %s or %s fonts are required to Viz.\n",
                            "Arial Narrow", "Liberation Sans Narrow"),
                    sprintf("Please use %s to install Liberation Sans Narrow font.",
                            "dlookr::import_liberation()"))
      packageStartupMessage(msg)
      return(FALSE)
    }
  }
  
  sucess <- TRUE
  result <- try(load_fonts())
  
  if (class(result) == "try-error") {
    sucess <- FALSE
  }
  
  flag <- try(import_arial_narrow())
  
  if (class(flag) == "try-error") {
    sucess <- FALSE
  } else {
    if (flag) {
      try(load_fonts())
    }    
  }
}
