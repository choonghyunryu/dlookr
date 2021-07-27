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

#
#   # create file paths to package assets ----
#   css    <- pkg_resource("rmarkdown/resources/html-styles-test.css")
#
#   ## TODO: FAVICON
#   ## 1. Save favicon.png in inst/rmarkdown/resources
#   ## 2. Pass `in_header = favicon_file` as an argument to rmarkdown::includes() below
#   ## 3. Delete these instructions (##)
#   ## If you do NOT want a footer, delete this section and remove the `in_header` line from `includes` below
#   # create temp file for favicon ----
#   favicon_locn <- pkg_resource('rmarkdown/resources/favicon.png')
#   favicon_html <- paste0('<link rel="shortcut icon" href="',favicon_locn,'">')
#   favicon_file <- tempfile()
#   writeLines(favicon_html, favicon_file)
#
#   ## TODO: FOOTER
#   ## 1. Save footer logo as logo-{theme}.png in inst/rmarkdown/resources
#   ## 2. Update file path passed to `footer_logo <- pkg_resource()`
#   ## 3. Update additional rows of HTML. Consider what should be set as an optional argument
#   ## 4. Delete these instructions (##)
#   ## If you do NOT want a footer, delete this section and remove the `after_body` line from `includes` below
#   # create temp file for footer ----
#   footer_logo <- pkg_resource('rmarkdown/resources/logo-test.png')
#   footer_logo_html <- paste0("<img src = '", footer_logo, "' width = 150>")
#   footer_text_html <- '
#     <p>Produced by XXX. All rights reserved. </p>
#     <p>Contact YOUREMAIL@gmail.com for more information.</p>'
#   footer_tmsp_html <- paste("<p>Last generated at", Sys.time(), "</p>")
#   footer_html <- paste(
#     "<footer><hr/><center><br/>",
#     footer_logo_html,
#     footer_text_html,
#     footer_tmsp_html,
#     "</center></footer>")
#   footer_file <- tempfile()
#   writeLines(footer_html, footer_file)
#
#   # call the base html_document function ----
#   rmarkdown::html_document(
#     css = css,
#     includes = rmarkdown::includes(
#       in_header = favicon_file,
#       after_body = footer_file
#       ),
#     ...
#   )
#
# }
