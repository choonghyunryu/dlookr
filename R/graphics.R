#' Import Liberation Sans Narrow fonts
#'
#' @description
#' Import Liberation Sans Narrow font to be used when drawing charts.
#'
#' @details 
#' The Liberation(tm) Fonts is a font family which aims at metric compatibility 
#' with Arial, Times New Roman, and Courier New. It is sponsored by Red Hat.
#' Import fonts is older versions of the Liberation(tm) fonts. This is released 
#' as open source under the GNU General Public License version 2 with exceptions. 
#' https://fedoraproject.org/wiki/Licensing/LiberationFontLicense
#' 
#' @importFrom extrafont font_import
#' @export
import_liberation <- function() {
  font_path <- system.file("fonts", "LiberationSansNarrow", package = "dlookr")
  suppressWarnings(suppressMessages(extrafont::font_import(font_path, 
                                                           prompt = FALSE)))
  
  msg <- sprintf("Imported Liberation Sans Narrow fonts from %s", font_path)
  message(msg)
}

#' @importFrom hrbrthemes theme_ipsum
theme_typographic <- function () {
  base_family <- get_font_family()
  
  hrbrthemes::theme_ipsum(base_family = base_family)
}

#' @importFrom extrafont fonttable
get_font_family <- function () {
  family <- c("Arial Narrow", "Liberation Sans Narrow")
  
  font_tab <- extrafont::fonttable()
  
  base_family <- family[family %in% font_tab$FamilyName][1]
  
  if (is.na(base_family))
    base_family <- "sans"
  base_family
}
