#' @importFrom htmltools div
#' @importFrom stats runif
html_cat <- function(msg, id = paste0("ID", stats::runif(1))) {
  htmltools::div(class = "caption", 
                 id = id,
                 style = "padding-top: 8px; padding-bottom: 8px; color: #777777; 
                 text-align: left;", msg) %>% 
      as.character() %>% 
      cat()
}


#' @importFrom htmltools HTML div
break_page <- function() {
  htmltools::HTML(
    paste0(htmltools::div(class = 'page-break-after', ""))
  )
}


#' @importFrom htmltools HTML div
break_page_asis <- function() {
  htmltools::HTML(
    cat(
      paste0(
        htmltools::div(class = 'page-break-after', "")
      )
    )
  )
}

#' @importFrom htmltools br
break_line_asis <- function(n = 1) {
  for (i in seq(n)) {
    htmltools::br() %>%
      as.character() %>%
      cat()
  }
}

#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom htmltools HTML div
#' @import dplyr
print_tab <- function(tab, n_rows = 25, add_row = 3, caption = "", 
                      full_width = TRUE, font_size = 14, align = NULL, 
                      col.names = NA, digits = 2, big_mark = TRUE) {
  N <- nrow(tab)
  n_pages <- 1
  
  if (N > n_rows) {
    n_pages <- n_pages + ceiling((N - n_rows) / (n_rows + add_row))
  }

  for (i in seq(n_pages)) {
    if (i == 1) {
      idx <- intersect(seq(N), seq(n_rows))
    } else {
      idx <- (max(idx) + 1):(max(idx) + n_rows + add_row) %>% 
        pmin(N) %>% 
        unique()
    }
    
    if (is.null(align)) {
      if (big_mark) {
        ktab <- knitr::kable(tab[idx, ], digits = digits, format = "html", 
                             caption = ifelse(i > 1, paste(caption, "(continued)"), caption),
                             col.names = col.names, format.args = list(big.mark = ","))
      } else {
        ktab <- knitr::kable(tab[idx, ], digits = digits, format = "html", 
                             caption = ifelse(i > 1, paste(caption, "(continued)"), caption),
                             col.names = col.names)
      }
    } else {
      if (big_mark) {
        ktab <- knitr::kable(tab[idx, ], digits = digits, format = "html", align = align,
                             caption = ifelse(i > 1, paste(caption, "(continued)"), caption),
                             col.names = col.names, format.args = list(big.mark = ","))
      } else {
        ktab <- knitr::kable(tab[idx, ], digits = digits, format = "html", align = align,
                             caption = ifelse(i > 1, paste(caption, "(continued)"), caption),
                             col.names = col.names)
      }  
    }
    
    ktab %>%
      kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                position = "left") %>% 
      gsub("font-size: initial !important;", 
           "font-size: 12px !important;", .) %>%      
      cat()
    
    if (n_pages == 1 | i < n_pages) {
      break_page_asis()
    }
  }
}


#' @importFrom grDevices col2rgb rgb
col2hex <- function(col) {
  if (length(grep("^#[[:xdigit:]]{6}$", col)) > 0) {
    return(col)
  }
  
  mat_rgb <- col %>% 
    tolower() %>% 
    grDevices::col2rgb() %>% 
    t() / 255 
  
  grDevices::rgb(mat_rgb)
}


fixed_text <- function(x, max_width = 30) {
  ifelse(nchar(x) > max_width, paste(strtrim(x, max_width - 4), "..."), x)
}

