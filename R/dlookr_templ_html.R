#' @name dlookr_templ_html
#' @rdname dlookr_templ_html
#' @title dlookr HTML template
#'
#' Loads additional style and template file
#'
#' @references https://raw.githubusercontent.com/dr-harper/example-rmd-templates/master/R/my_html_format.R
#'
#' @param toc should a table of contents be displayed?
#' @param ... additional arguments provided to \code{html_document}
#' @return An R Markdown output format.
#' @export dlookr_templ_html
#'
dlookr_templ_html <- function(toc = TRUE, ...) {

  # locations of resource files in the package
  pkg_resource = function(...) {
    system.file(..., package = "dlookr")
  }

  css    = pkg_resource("resources/dlookr-bootstrap.css")
  # header = pkg_resource("resources/header.html")
  header = "header_temp.html"
  footer = pkg_resource("resources/footer.html")  

  # call the base html_document function
  rmarkdown::html_document(
    toc = toc,
    toc_float = TRUE,
    fig_caption = TRUE,
    fig_height = 5,
    fig_width = 8,
    toc_depth = 2,
    code_folding = "show",
    css = css,
    #number_sections = TRUE,
    includes = rmarkdown::includes(in_header = header, after_body = footer),
    ...
  )
}
