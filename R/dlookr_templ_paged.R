#' @name dlookr_orange_paged
#' @rdname dlookr_orange_paged
#' @title Generate paged HTML document
#'
#' @param ... arguments to be passed to \code{pagedown::\link[pagedown]{html_paged}}.
#' @references \url{https://pagedown.rbind.io}
#' @return document of markdown format.
#' @import pagedown
#' @export dlookr_orange_paged
#'
dlookr_orange_paged <- function(...) {
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "dlookr")
  }

  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "dlookr")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page-orange'), cssfile('custom-orange')), ...)
}

#' @rdname dlookr_orange_paged
#' @export dlookr_blue_paged
dlookr_blue_paged <- function(...) {
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "dlookr")
  }
  
  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "dlookr")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page-blue'), cssfile('custom-blue')), ...)
}
