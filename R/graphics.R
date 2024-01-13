#' Import Google Fonts
#'
#' @description
#' Import google font to be used when drawing charts.
#' 
#' @param family character. font family name
#' 
#' @return No return value. This function just loads Google Fonts.
#'
#' @details
#' When attaching the dlookr package, use "Roboto Condensed" and 
#' "Noto Sans Korean" among Google fonts. And also loads "Liberation Sans Narrow"
#' and "NanumSquare" included in the package for offline environment.
#' 
#' If you want to use anything other than the 4 fonts that are loaded with 
#' the dlookr package, load the desired Google fonts with import_google_font().
#' 
#' dlookr recommends the following google fonts, both sans and condensed:
#' "IBM Plex Sans Condensed", "Encode Sans Condensed", "Barlow Condensed", 
#' "Saira Condensed", "Titillium Web", "Oswald", "PT Sans Narrow"
#' 
#' Korean fonts:
#' "Nanum Gothic", "Gothic A1"
#'
#' @export
#' @importFrom sysfonts font_add_google font_families
#' 
import_google_font <- function(family) {
  recommend_family <- c(
    "IBM Plex Sans Condensed", "Encode Sans Condensed", "Barlow Condensed",
    "Saira Condensed", "Titillium Web", "Oswald", "PT Sans Narrow",
    "Nanum Gothic", "Gothic A1")
  
  sysfonts::font_add_google(name = family, family = family) 
}


#' @importFrom hrbrthemes theme_ipsum_rc
theme_typographic <- function(base_family = NULL) {
  if (is.null(base_family)) {
    if (options()[["dlookr_offline"]]) {
      base_family <- "Liberation Sans Narrow"
    } else {
      base_family <- "Roboto Condensed"
    }
  }
  
  if (!base_family %in% sysfonts::font_families()) {
    if (options()[["dlookr_offline"]]) {
      base_family <- "Liberation Sans Narrow"
    } else {
      base_family <- "Roboto Condensed"
    }
  }
  
  hrbrthemes::theme_ipsum_rc(base_family = base_family)
}

#' @importFrom graphics text
plot_message <- function(msg = NULL, cex = 1) {
  if (is.null(msg)) {
    stop("msg is NULL. Please describe the msg value.")
  }
  
  plot(1:3, type = "n", axes = FALSE, xlab = "", ylab = "")  
  graphics::text(x = 2, y = 2, msg, cex = cex)
}

