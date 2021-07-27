#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import ggplot2
#' @import reactable
html_descriptive <- function(.data) {
  N <- nrow(.data)
  
  raws <- .data %>% 
    diagnose() %>% 
    left_join(
      .data %>% 
        diagnose_numeric(),
      by = "variables"
    ) %>% 
    left_join(
      .data %>% 
        dlookr::describe(statistics = c("skewness", "kurtosis")) %>% 
        select(-n, -na),
      by = c("variables" = "variable")) %>% 
    mutate(missing_percent = round(missing_percent, 2),
           unique_rate = round(unique_rate, 3),
           skewness = round(skewness, 3),
           kurtosis = round(kurtosis, 3))
  
  cap <- "Descriptive statistics and visualization of individual variables"
  html_cat(cap)
  
  raws %>% 
    reactable(
      columns = list(
        variables = colDef(
          name = "Variables",
          cell = JS("function(cellInfo) {
                  const title = '<span class=\"variables\">' + cellInfo.value + '</span>'
                  const type = '<span class=\"box-type\">' + cellInfo.row['types'] + '</span>'
                  const details = '<div class=\"variable-info-details\">' + type + '</div>'
                  const text = '<div class=\"variable-info-text\">' + title + details + '</div>'
                  
                  return '<div class=\"variable-info\">' + text + '</div>'
        }"),
        html = TRUE,    
        minWidth = 150,
        maxWidth = 200
        ),
        types = colDef(
          show = FALSE
        ),
        missing_count = colDef(
          name = "Missing (%)",
          cell = JS("function(cellInfo) {
                  const value = cellInfo.value.toLocaleString('en')
                  const ratio = '(' + cellInfo.row['missing_percent'] + ')'
                  return '<div class=\"value-info-text\">' + value + '<br>' + ratio + '</div>'
        }"),
        html = TRUE,
        align = "right"
        ),
        missing_percent = colDef(
          show = FALSE
        ),
        unique_count = colDef(
          name = "Distincts (Ratio)",
          cell = JS("function(cellInfo) {
                  const value = cellInfo.value.toLocaleString('en')
                  const ratio = '(' + cellInfo.row['unique_rate'] + ')'
                  return '<div class=\"value-info-text\">' + value + '<br>' + ratio + '</div>'
          }"),
          html = TRUE,
          align = "right"
        ),
        unique_rate = colDef(
          show = FALSE
        ),        
        zero = colDef(
          name = "Zeros",
          cell = JS("function(cellInfo) {
                    value = cellInfo.value
                  
                    if (value == null) {
                      value = '-'
                    } else {
                      value = value.toLocaleString('en')
                    }
                    return '<div class=\"value-info-text\">' + value + '</div>'
                  }"),
          html = TRUE,
          align = "right"
        ),
        minus = colDef(
          name = "Negatives",
          cell = JS("function(cellInfo) {
                    value = cellInfo.value
                  
                    if (value == null) {
                      value = '-'
                    } else {
                      value = value.toLocaleString('en')
                    }
                    return '<div class=\"value-info-text\">' + value + '</div>'
                  }"),
          html = TRUE,
          align = "right"
        ),        
        outlier = colDef(
          name = "Outliers",
          cell = JS("function(cellInfo) {
                    value = cellInfo.value
                  
                    if (value == null) {
                      value = '-'
                    } else {
                      value = value.toLocaleString('en')
                    }
                    return '<div class=\"value-info-text\">' + value + '</div>'
                  }"),
          html = TRUE,
          align = "right"
        ),
        min = colDef(
          show = FALSE
        ),
        Q1 = colDef(
          show = FALSE
        ),
        mean = colDef(
          show = FALSE
        ),
        median = colDef(
          show = FALSE
        ),
        Q3 = colDef(
          show = FALSE
        ),
        max = colDef(
          show = FALSE
        ),
        skewness = colDef(
          show = FALSE
        ),
        kurtosis = colDef(
          show = FALSE
        )        
      ),
      bordered = FALSE,
      class = "variables-tbl",
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        cellPadding = "5px 3px"
      ),
      details = function(index) {
        variable <- raws$variables[index]
        type <- raws$types[index]
        
        suppressWarnings(
          unique_missing <- raws %>% 
            select(variables, missing_count, unique_count, zero, minus, 
                   outlier) %>% 
            filter(variables %in% variable) %>% 
            select(-variables) %>% 
            tidyr::gather(key = "metric", value = "count") %>% 
            mutate(metric = sub("_[[:print:]]+", "", metric)) %>%         
            mutate(metric = case_when(metric %in% "unique" ~ "Distincts",
                                      metric %in% "missing" ~ "Missing",
                                      metric %in% "zero" ~ "Zeros",
                                      metric %in% "minus" ~ "Negatives",
                                      metric %in% "outlier" ~ "Outliers")) %>% 
            filter(!is.na(count)) %>% 
            mutate(percent = count / N ) %>% 
            arrange(metric) %>% 
            reactable(
              fullWidth = FALSE,
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
              columns = list(
                metric = colDef(name = "Metrics"),
                count = colDef(name = "Frequency", 
                               format = colFormat(separators = TRUE)),
                percent = colDef(name = "Percent", 
                                 format = colFormat(percent = TRUE, digits = 1))
              ))
        )
        
        if (type %in% c("character")) {
          agg_char <- .data %>%
            rename(value = variable) %>%
            select(value) %>%
            mutate(value = as.character(value)) %>%
            mutate(n_character = nchar(value)) %>%
            mutate(n_word = sapply(gregexpr("[[:print:]]+", value),
                                   function(x) sum(x > 0))) %>%
            summarise(min_word = min(n_word, na.rm = TRUE),
                      mean_word = round(mean(n_word, na.rm = TRUE), 1),
                      median_word = median(n_word, na.rm = TRUE),
                      max_word = max(n_word, na.rm = TRUE),
                      min_character = min(n_character, na.rm = TRUE),
                      mean_character = round(mean(n_character, na.rm = TRUE), 1),
                      median_character = median(n_character, na.rm = TRUE),
                      max_character = max(n_character, na.rm = TRUE)) %>%
            unlist()
          
          stat_char <- data.frame(Class = c("Words Count", "Characters Length"),
                                  rbind(agg_char[1:4], agg_char[5:8]))
          names(stat_char) <- names(stat_char) %>% sub("_[[:print:]]+", "", .)
          
          stat_char <- stat_char %>% 
            reactable(fullWidth = FALSE,
                      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
                      columns = list(Class = colDef(width = 150))
            )          
          
          top_rank <- html_toprank(.data, variable, TRUE)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", top_rank, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),          
            shiny::tabPanel("Character Stats", stat_char,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")          
          )
        } else if (type %in% c("factor", "ordered", "Date", "POSIXct")) {
          top_rank <- html_toprank(.data, variable, TRUE)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", top_rank, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")
          )
        } else if (type %in% c("integer", "numeric")) {
          stats <- raws %>% 
            filter(variables %in% variable) %>%           
            select(min:max, skewness, kurtosis) %>% 
            mutate(mean = round(mean, 3)) %>% 
            reactable(fullWidth = FALSE,
                      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"))
          
          p_hist <- htmltools::plotTag({
            plot_hist_numeric(.data, which(names(.data) == variable))
          }, sprintf("A plot of the %s variable", variable), width = 600, 
          height = 400, device = grDevices::png)
          
          diagn_outlier <- .data %>%
            diagnose_outlier(variable)
          
          cols <- c("Outliers count", "Outliers ratio (%)", "Mean of outliers",
                    "Mean with outliers", "Mean without outliers")
          outlier_df <- data.frame(Measures = cols,
                                   Values = as.vector(t(diagn_outlier[, -1]))) 
          
          values <- outlier_df$Values
          
          outlier_df$Values[1] <- round(values[1]) %>% 
            format(big.mark = ",")
          outlier_df$Values[2] <- round(values[2], 2) %>% 
            as.character() %>% 
            paste0("%")
          outlier_df$Values[3] <- values[3] %>% 
            format() %>% 
            as.character()
          outlier_df$Values[4] <- values[4] %>% 
            format() %>% 
            as.character()
          outlier_df$Values[5] <- values[5] %>% 
            format() %>% 
            as.character()
          
          outlier_df <- outlier_df %>% 
            reactable(fullWidth = FALSE,
                      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
                      columns = list(
                        Measures = colDef(width = 200),
                        Values = colDef(align = "right")
                      ))
          
          p_outlier <- htmltools::plotTag({
            plot_outlier(.data, variable)
          }, sprintf("A plot of the %s variable", variable), width = 600,
          height = 400, device = grDevices::png)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", p_hist, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Statistics", stats, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Outliers Plot", p_outlier,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Outliers Stats", outlier_df,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")            
          )
        }
      }      
    )
}


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom purrr map_df
#' @import dplyr
#' @import htmltools
html_normality <- function(.data, theme = c("orange", "blue")[1]) {
  normality_indicator <- function(value, theme) {
    style <- ifelse(theme == "orange", "color: rgb(255, 127, 42)", 
                    "color: rgb(0, 114, 188)")
    
    if (value %in% "Balanced") {
      args <- shiny::icon("balance-scale", 
                          style = style)
    } else if (value %in% "Right-Skewed") {
      args <- shiny::icon("balance-scale-left", 
                          style = style)
    } else if (value %in% "Left-Skewed") {
      args <- shiny::icon("balance-scale-right", 
                          style = style)
    } else if (value %in% "Invalid") {
      args <- shiny::icon("exclamation-triangle", 
                          style = style)
    } 
    
    div(args, paste("", value))
  }
  
  N <- nrow(.data)
  
  describe_num <- dlookr::describe(.data) %>% 
    select(variable, n, na, mean, p00, p25, p50,  p75, p100, skewness, kurtosis) %>% 
    rename("Min" = p00,
           "Max" = p100,
           "Median" = p50,
           "Q1" = p25,
           "Q3" = p75,
           "Missing" = na) %>% 
    mutate(balance = case_when(
      abs(skewness) <= 1.2 ~ "Balanced",
      skewness > 1.2 ~ "Right-Skewed",
      skewness < 1.2 ~ "Left-Skewed",
      is.na(skewness) ~ "Invalid"
    )) %>% 
    mutate(mean = round(mean, 2))
  
  cap <- "Normality test results of numeric variables"
  html_cat(cap)
  
  describe_num %>% 
    reactable(
      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
      columns = list(
        balance = colDef(
          name = "Balance",
          width = 145,
          cell = function(value) normality_indicator(value, theme)
        ),
        variable = colDef(
          name = "Variables",
          width = 125
        ),
        mean = colDef(
          name = "Mean"
        ),        
        n = colDef(
          show = FALSE
        ), 
        Missing = colDef(
          show = FALSE
        ),         
        skewness = colDef(
          show = FALSE
        ),
        kurtosis = colDef(
          show = FALSE
        )         
      ),
      details = function(index) {
        variable <- describe_num$variable[index]
        
        flag_sample <- FALSE
        
        x <- .data[, variable] %>%
          .[!is.na(.)]
        
        if (length(x) > 5000) {
          x <- sample(x, size = 5000, replace = TRUE)
          flag_sample <- TRUE
        }  
        
        if (length(x) == 0) {
          tab_test <- data.frame(alert = "all values are NA") %>% 
            reactable()
          stats <- data.frame(alert = "all values are NA") %>% 
            reactable()
          p_normality <- data.frame(alert = "all values are NA") %>% 
            reactable()          
        } else if (length(x) < 3L || length(unique(x)) < 3L) {
          tab_test <- data.frame(
            alert = "(unique) sample size must be greater then 3") %>% 
            reactable()
          stats <- data.frame(
            alert = "(unique) sample size must be greater then 3") %>% 
            reactable()
          p_normality <- data.frame(
            alert = "(unique) sample size must be greater then 3") %>% 
            reactable()   
        } else if (diff(range(x, na.rm = TRUE)) == 0) {
          tab_test <- data.frame(alert = "all values are identical") %>% 
            reactable() 
          stats <- data.frame(alert = "all values are identical") %>% 
            reactable() 
          p_normality <- data.frame(alert = "all values are identical") %>% 
            reactable()           
        } else {
          y <- shapiro.test(x)
          statistic <- round(y$statistic, 5)
          p_value <- round(y$p.value, 7)
          
          tab_test <- data.frame(
              Statistic = statistic,
              p_value = p_value,
              remark = ifelse(flag_sample, "5000 samples", "No sample"),
              row.names = NULL
            ) %>% 
            reactable(
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
              fullWidth = FALSE,
              columns = list(
                Statistic = colDef(
                  name = "Shapiro-Wilk Statistic",
                  width = 200
                ),
                p_value = colDef(
                  name = "P-value of Test",
                  width = 200
                ),
                remark = colDef(
                  name = "Whether Sample",
                  width = 200
                )               
              )  
            )
          
          if (is.factor(x)) x <- as.numeric(x)
          
          if (sum(x < 0, na.rm = TRUE) > 0) {
            type <- c("original", "log+a transformation", "Box-Cox transformation")
            
            skew <- c(skewness(x, na.rm = TRUE),
                      skewness(get_transform(x, "log+a"), na.rm = TRUE),
                      skewness(get_transform(x, "Box-Cox"), na.rm = TRUE))
            
            kurt <- c(kurtosis(x, na.rm = TRUE),
                      kurtosis(get_transform(x, "log+a"), na.rm = TRUE),
                      kurtosis(get_transform(x, "Box-Cox"), na.rm = TRUE))          
          } else {
            if (any(x == 0, na.rm = TRUE)) {
              type <- c("original", "log+1 transformation", "sqrt transformation")
              
              skew <- c(skewness(x, na.rm = TRUE),
                        skewness(get_transform(x, "log+1"), na.rm = TRUE),
                        skewness(sqrt(x), na.rm = TRUE))
              
              kurt <- c(kurtosis(x, na.rm = TRUE),
                        kurtosis(get_transform(x, "log+1"), na.rm = TRUE),
                        kurtosis(sqrt(x), na.rm = TRUE))         
            } else {
              type <- c("original", "log transformation", "sqrt transformation")
              
              skew <- c(skewness(x, na.rm = TRUE),
                        skewness(log(x), na.rm = TRUE),
                        skewness(sqrt(x), na.rm = TRUE))
              
              kurt <- c(kurtosis(x, na.rm = TRUE),
                        kurtosis(log(x), na.rm = TRUE),
                        kurtosis(sqrt(x), na.rm = TRUE)) 
            }  
          }
          
          stats <- data.frame(type = type, 
                              skewness = round(skew, 4), 
                              kurtosis = round(kurt, 4)) %>% 
            reactable(
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
              fullWidth = FALSE,
              columns = list(
                type = colDef(
                  name = "Data Type",
                  width = 200
                ),
                skewness = colDef(
                  name = "Skewness",
                  width = 200
                ),
                kurtosis = colDef(
                  name = "Kurtosis",
                  width = 200
                )              
              )  
            )
          
          p_normality <- htmltools::plotTag({
            x <- data.frame(x)
            
            if (sum(x < 0, na.rm = TRUE) > 0) {
              plot_normality(x, x, left = "log+a", right = "Box-Cox")
            } else {
              if (any(x == 0, na.rm = TRUE)) 
                plot_normality(x, x, left = "log+1", right = "sqrt")
              else
                plot_normality(x, x)
            }
          }, sprintf("A plot of the %s variable", variable), width = 600,
          height = 400, device = grDevices::png)
        }
        
        shiny::tabsetPanel(
          shiny::tabPanel("Distribution", p_normality,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"), 
          shiny::tabPanel("Skewness and Kurtosis", stats,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Normality test", tab_test,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")
        )
      }      
    )  
} 


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom purrr map_int
#' @importFrom hrbrthemes theme_ipsum scale_fill_ipsum
#' @import reactable
#' @import dplyr
#' @import htmltools
html_compare_category <- function(.data, n_cells = 20, n_levels = 10) {
  plot_compare <- function(x, y, ctab) {
    xvar <- x
    yvar <- y
    
    data <- ctab %>% 
      select(a = xvar, b = yvar, n) 
    
    first <- data[1, 1] %>% pull %>% as.character
    y <- data %>% 
      filter(a %in% first) %>% 
      select(b, n)
    
    y_lab <- y$b %>% rev() %>% as.character()
    y <- y$n %>% rev()
    
    y_cumsum <- cumsum(y)
    y_center <- y / 2
    
    y_pos <- numeric(length(y))
    for (j in seq(y)) {
      if (j == 1) {
        y_pos[j] <- y_center[j]
      } else {
        y_pos[j] <- y_cumsum[j-1] + y_center[j]
      }
      y_pos[j] <- y_pos[j] / sum(y)
    }
    
    suppressWarnings({
      p <- data %>% 
        group_by(a) %>% 
        mutate(x_width = sum(n)) %>% 
        ggplot(aes(x = factor(a), y = n)) +
        geom_col(aes(width = x_width, fill = factor(b)),
                 color = "white", size = 2, 
                 position = position_fill(reverse = FALSE)) +
        facet_grid(~ a, space = "free", scales = "free", switch = "x") +
        scale_x_discrete(name = xvar) +
        scale_y_continuous(name = yvar, breaks = y_pos, labels = y_lab) +
        labs(title = sprintf("Mosaics plot by '%s' vs '%s'", xvar, yvar)) +
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              strip.background = element_blank(),
              panel.spacing = unit(0, "pt")) 
    })
    
      p <- p +
        hrbrthemes::theme_ipsum(base_family = get_font_family()) +
        hrbrthemes::scale_fill_ipsum(na.value = "grey80") +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              panel.spacing = unit(0, "pt"))
      
    suppressWarnings(print(p))
  }
  
  idx <- .data %>% 
    find_class("categorical")
  
  if (length(idx) < 2) {
    html_cat("The number of categorical variables is less than 2.")
  } else {
    is_under_n <- idx %>%
      purrr::map_int(
        function(x) levels(.data[, x]) %>% 
          length()
      ) <= n_levels 
    
    idx_target <- is_under_n %>% 
      which() %>% 
      idx[.]
    
    if (length(idx_target) < 2) {
      html_cat("The valid categorical variables is less than 2.")
    } else {
      cat_compares <-  compare_category(.data[, idx_target]) 
      tabs <- summary(cat_compares, "all", marginal = TRUE, na.rm = FALSE, 
                      verbose = FALSE)
      tab_compare <- tabs$chisq %>% 
        filter(df <= n_cells) %>% 
        filter(!is.nan(statistic))
      
      cap <- "Relationship between two categorical variables"
      html_cat(cap)
      
      tab_compare %>% 
        select(1, 2, 5, 3, 4) %>% 
        reactable(
          defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
          columns = list (
            variable_1 = colDef(
              name = "First Variable"
            ),
            variable_2 = colDef(
              name = "Second Variable"
            ),
            df = colDef(
              name = "Degree of Freedom"
            ),
            statistic = colDef(
              name = "Statistic",
              format = colFormat(digits = 2)
            ),
            p.value = colDef(
              name = "P-Value",
              format = colFormat(digits = 5)
            )         
          ),
          details = function(index) {
            ctable <- tabs$table[[index]]
            
            contingency <- function(tab, relate = FALSE) {
              dname <-  tab %>% dimnames()
              dframe <- tab %>% data.frame()
              
              if (relate) {
                dframe <- round(dframe / dframe[nrow(dframe), ncol(dframe)] * 100, 2)
              }
              
              rownames(dframe) <- tab %>% 
                rownames() %>% 
                ifelse(is.na(.), "<NA>", .)
              
              rname <- dname %>% names() %>% "["(1)
              varname <- ifelse(is.na(dname[[2]]), "<NA>", dname[[2]])
              
              colum_list <- seq(ncol(dframe)) %>% 
                lapply(function(x) {
                  colDef(
                    name = varname[x],
                    format = colFormat(
                      separators = TRUE
                    ),
                    sortable = FALSE
                  )
                }) 
              names(colum_list) <- names(dframe)
              
              colum_list[[".rownames"]] <- colDef(
                name = rname, 
                style = "fontWeight: 'bold';",
                sortable = FALSE
              )
              
              cname <- list(
                colGroup(name = dname %>% names() %>% "["(2), 
                         columns = names(dframe))
              )
              
              dframe %>% 
                reactable(
                  defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
                  columns = colum_list,
                  columnGroups = cname
                )
            }
            
            x <- ctable %>% dimnames() %>% names() %>% "["(1)
            y <- ctable %>% dimnames() %>% names() %>% "["(2)
            idx_nm <- paste(x, y, sep = " vs ")
            
            p_compare <- htmltools::plotTag({
              plot_compare(x, y, cat_compares[[idx_nm]])
            }, sprintf("A plot of the %s variable", idx_nm), width = 600,
            height = 400, device = grDevices::png)
            
            shiny::tabsetPanel(
              shiny::tabPanel("Mosaics Plot", p_compare,
                              hr(style = "border-top: 1px solid black;"),
                              style = "padding-top:5px; padding-bottom:25px;"),
              shiny::tabPanel("Contingency Table", contingency(ctable),
                              hr(style = "border-top: 1px solid black;"),
                              style = "padding-top:5px; padding-bottom:25px;"),
              shiny::tabPanel("Relative Contingency Table", contingency(ctable, TRUE),
                              hr(style = "border-top: 1px solid black;"),
                              style = "padding-top:5px; padding-bottom:25px;")          
            )        
          }  
        )      
    }
  }
}

#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom purrr map_int
#' @importFrom hrbrthemes theme_ipsum
#' @import reactable
#' @import dplyr
html_compare_numerical <- function(.data) {
  idx <- .data %>% 
    find_class("numerical")
  
  if (length(idx) < 2) {
    html_cat("The number of numerical variables is less than 2.")
  } else {
    num_compares <-  compare_numeric(.data)
    
    cap <- "Relationship between two numerical variables"
    html_cat(cap)
    
    num_compares$correlation %>% 
      reactable(
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list (
          var1 = colDef(
            name = "First Variable"
          ),
          var2 = colDef(
            name = "Second Variable"
          ),
          coef_corr = colDef(
            name = "Correlation Coefficient",
            format = colFormat(
              digits = 5
            )
          )       
        ),
        details = function(index) {
          tab_model <- num_compares$linear[index, ] %>% 
            select(-logLik, -AIC, -BIC, -deviance, -df.residual, -nobs) %>% 
            reactable(
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
              columns = list(
                var1 = colDef(
                  name = "First Variable"
                ),
                var2 = colDef(
                  name = "Second Variable"
                ),
                r.squared = colDef(
                  format = colFormat(
                    digits = 3
                  )
                ),
                adj.r.squared = colDef(
                  format = colFormat(
                    digits = 3
                  )
                ),
                sigma = colDef(
                  width = 100,
                  format = colFormat(
                    digits = 2
                  )
                ),
                statistic = colDef(
                  width = 100,
                  format = colFormat(
                    digits = 4
                  )
                ),
                p.value = colDef(
                  width = 100,
                  format = colFormat(
                    digits = 4
                  )
                ),
                df = colDef(
                  width = 50
                )              
              )
            )
          
          num_compare <- num_compares
          
          vars <- attr(num_compare, "combination")[index, ]
          attr(num_compare, "raw") <- attr(num_compare, "raw")[, vars]
          attr(num_compare, "combination") <- vars %>% t()
          
          p_scatter <- htmltools::plotTag({
            plot(num_compare)
          }, sprintf("A plot of the %s variable", vars %>% 
                       paste(collapse = " vs ")), width = 600,
          height = 400, device = grDevices::png)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Scatter Plot", p_scatter,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Linear Model Summaries", tab_model,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")          
          )        
        }  
      )
  }
}


#' @importFrom tidyr spread
#' @import reactable
#' @import dplyr
html_correlation <- function(.data) {
  idx <- .data %>% 
    find_class("numerical")
  
  if (length(idx) < 2) {
    html_cat("The number of numerical variables is less than 2.")
  } else {
    mat_corr <- correlate(.data) %>% 
      tidyr::spread(var2, coef_corr)
    
    cap <- "Correlation coefficient matrix of numeric variables"
    html_cat(cap)

    mat_corr %>% 
      reactable(
        defaultColDef = colDef(
          style = "font-size: 14px;color: hsl(0, 0%, 40%);",
          format = colFormat(
            digits = 3
          )
        ),
        columns = list(
          var1 = colDef(
            name = "First Variable",
            style = list(fontWeight = "bold")
          )
        ),
        columnGroups = list(
          colGroup(name = "Second Variable", columns = names(mat_corr)[-1])
        )    
      )
  }
}


#' @importFrom shiny icon tabsetPanel tabPanel
#' @import reactable
#' @import dplyr
html_target_numerical <- function(.data, target) {
  if (is.null(target)) {
    html_cat("The target variable is not defied.")   
    return()
  }
  
  if (!target %in% names(.data)) {
    html_cat("The data does not contain the variable specified for target.")    
    return()
  }  
  
  nm_numeric <- .data %>% 
    find_class("numerical", index = FALSE) %>% 
    setdiff(target)
  
  if (length(nm_numeric) < 1) {
    html_cat("There are no numeric variables except for the target variable.")
  } else {
    suppressWarnings({
      tab_main <- data.frame(Variable = nm_numeric, target_variable = target)
    })
    
    
    tgt_by <- .data %>% 
      target_by(target)
      
    cap <- "Relationship between two numerical variables by level of target variable"
    html_cat(cap)
    
    tab_main %>% 
      reactable(
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list(
          target_variable = colDef(
            name = "Target Variable"
          )
        ),
        details = function(index) {
          nm_var <- nm_numeric[index]
          
          mat <- relate(tgt_by, all_of(nm_var)) %>% 
            select(-p20, -p30, -p40, -p60, -p70, -p80) %>% 
            select(-variable) %>% 
            t()
          
          colnames(mat) <- mat[1, ] %>%
            ifelse(. %in% "total", "<Total>", .)
          mat  <- mat[-1, ] 
          
          rownames(mat) <- c("N", "Missing", "Mean", "Standard Deviation", 
                             "Standard Error Mean", "IQR", "Skewness", 
                             "Kurtosis", "Min", "1%", "5%", "10%", "25%", 
                             "50%", "75%", "90%", "95%", "99%", "Max")
          
          tab_stat <- mat %>% 
            reactable(
              defaultColDef = colDef(
                style = "font-size: 14px;color: hsl(0, 0%, 40%);",
                align = "right"
              ),
              columns = list(
                .rownames = colDef(
                  name = "Statistics",
                  align = "left"
                )
              ),
              columnGroups = list(
                colGroup(name = target %>% as.character, columns = colnames(mat))
              ) 
            )
          
          p_box <- htmltools::plotTag({
            plot_outlier(tgt_by, all_of(nm_var))
          }, sprintf("A plot of the %s and %s variable", target, nm_var), 
          width = 600, height = 400, device = grDevices::png)
          
          
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution Plot", p_box,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Descriptive Statistics", tab_stat,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")          
          ) 
        }
      )
  }
}


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom stats addmargins
#' @import reactable
#' @import dplyr
html_target_categorical <- function(.data, target) {
  if (is.null(target)) {
    stop("The target variable is not defied.")    
  }
  
  if (!target %in% names(.data)) {
    stop("The data does not contain the variable specified for target.")    
  }  
  
  nm_categorical <- .data %>% 
    find_class("categorical", index = FALSE) %>% 
    setdiff(target)
  
  if (length(nm_categorical) < 1) {
    html_cat("There are no categorical variables except for the target variable.")
  } else {
    suppressWarnings({
      tab_main <- data.frame(Variable = nm_categorical, target_variable = target)
    })
    
    tgt_by <- .data %>% 
      target_by(all_of(target))
    
    cap <- "Relationship between two categorical variables by level of target variable"
    html_cat(cap)
    
    tab_main %>% 
      reactable(
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list(
          target_variable = colDef(
            name = "Target Variable"
          )
        ),
        details = function(index) {
          nm_var <- nm_categorical[index]
          
          freq <- relate(tgt_by, all_of(nm_var)) %>% 
            stats::addmargins() %>% 
            as.data.frame() %>% 
            tidyr::spread(all_of(target), Freq) 
          
          names(freq) <- names(freq) %>% 
            ifelse(. %in% "Sum", "<Total>", .)
          freq[, 1] <- freq[, 1] %>% 
            as.character(.) %>% 
            ifelse(. %in% "Sum", "<Total>", .)
          
          tab_stat <- freq %>% 
            reactable(
              defaultColDef = colDef(
                format = colFormat(separators = TRUE)
              ),
              columnGroups = list(
                colGroup(name = target %>% as.character(), 
                         columns = names(freq)[-1]
                )
              )  
            )
          
          ratio <- freq
          ratio[, -1] <- ratio[, -1] / freq[nrow(freq), ncol(freq)]
          
          tab_ratio <- ratio %>% 
            reactable(
              defaultColDef = colDef(
                style = "font-size: 14px;color: hsl(0, 0%, 40%);",
                format = colFormat(percent = TRUE,
                                   digits = 1)
              ),
              columnGroups = list(
                colGroup(name = target %>% as.character(), 
                         columns = names(freq)[-1]
                )
              )  
            )          
          
          p_mosaics <- htmltools::plotTag({
            plot(relate(tgt_by, all_of(nm_var)))
          }, sprintf("A plot of the %s and %s variable", target, nm_var), 
          width = 600, height = 400, device = grDevices::png)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution Plot", p_mosaics,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Contingency Table", tab_stat,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Relative Contingency Table", tab_ratio,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")            
          ) 
        }
      )
  }
}


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @importFrom tidyr spread
#' @import reactable
#' @import dplyr
html_target_correlation <- function(.data, target) {
  if (is.null(target)) {
    stop("The target variable is not defied.")    
  }
  
  if (!target %in% names(.data)) {
    stop("The data does not contain the variable specified for target.")    
  }  
  
  nm_numeric <- .data %>% 
    find_class("numerical", index = FALSE) %>% 
    setdiff(target)
  
  if (length(nm_numeric) < 2) {
    html_cat("The number of numerical variables is less than 2.")
  } else {
    tab_main <- .data %>% 
      group_by_at(all_of(target)) %>% 
      tally() %>% 
      mutate(ratio = n / sum(n))
    
    tab_main %>% 
      reactable(
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list(
          n = colDef(
            name = "N",
            format = colFormat(
              separators = TRUE
            )
          ),          
          ratio = colDef(
            name = "Percent",
            format = colFormat(
              percent = TRUE,
              digits = 2
            )
          )          
        ),
        details = function(index) {
          value <- tab_main[index, target] %>% 
            pull() %>% 
            as.character()
          
          target2 <- paste0("^", target, "$")
          
          data_filterd <- .data %>% 
            filter_at(vars(matches(target2)), 
                      any_vars(. %in% value))
          
          mat_corr <- data_filterd %>% 
            correlate() %>% 
            tidyr::spread(var2, coef_corr)
          
          mat_corr <- mat_corr %>% 
            reactable(
              defaultColDef = colDef(
                style = "font-size: 14px;color: hsl(0, 0%, 40%);",
                format = colFormat(
                  digits = 3
                )
              ),
              columns = list(
                var1 = colDef(
                  name = "First Variable",
                  style = list(fontWeight = "bold")
                )
              ),
              columnGroups = list(
                colGroup(name = "Second Variable", columns = names(mat_corr)[-1])
              )    
            )

          p_corr <- htmltools::plotTag({
            plot_correlate(data_filterd)
          }, sprintf("A plot of the variable %s == %s", target, value), 
          width = 600, height = 500, device = grDevices::png)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Correlation Matrix", mat_corr,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Correlation Plot", p_corr,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")            
          ) 
        }        
      )
  }
}


#' @import dplyr
html_paged_describe <- function(.data, n_rows = 25, add_row = 3, caption = "", 
                                full_width = TRUE, font_size = 14) {
  tabs <- .data %>% 
    dlookr::describe(statistics = c("mean", "sd", "quantiles"),
                     quantiles = c(0, 0.25, 0.5, 0.75, 1)) %>% 
    select(-n)
  
  colums <- c("variables", "missing", "mean", "sd", "min", "Q1", 
              "median", "Q3", "max")
    
  print_tab(tabs, n_rows = n_rows, add_row = 3, caption = caption, 
            col.names =  colums, full_width = full_width, font_size = font_size)
}


#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
html_paged_describe_detail <- function(.data, n_ind = 4, caption = "", 
                                       full_width = TRUE, font_size = 14) {
  nums <- .data %>% 
    find_class("numerical", index = FALSE)
  
  n_page <- ceiling(length(nums) / n_ind) 
  
  if (n_page > 0) {
    for (i in seq(n_page)) {
      vars_nm <- nums[(((i - 1) * n_ind) + 1):min(i * n_ind, length(nums))]
      
      .data %>% 
        select_at(vars_nm) %>% 
        plot_box_numeric()
      
      break_line_asis(2)
      
      col.names <- c("variables", "data types", "distinct", "skewness", 
                     "kurtosis", "zero", "negative", "outlier")
      .data %>% 
        select_at(vars_nm) %>% 
        diagnose() %>%
        select(variables, types, distinct = unique_count) %>% 
        left_join(
          .data %>% 
            select_at(vars_nm) %>% 
            diagnose_numeric(),
          by = "variables"
        ) %>% 
        left_join(
          .data %>% 
            select_at(vars_nm) %>% 
            dlookr::describe(statistics = c("sd", "skewness", "kurtosis")) %>% 
            select(-n, -na),
          by = c("variables" = "variable")) %>% 
        mutate(skewness = round(skewness, 3),
               kurtosis = round(kurtosis, 3)) %>% 
        select(variables:distinct, skewness, kurtosis, zero, 
               negative = minus, outlier)  %>% 
        knitr::kable(digits = 2, format = "html", col.names = col.names, 
                     format.args = list(big.mark = ",")) %>% 
        kableExtra::kable_styling(full_width = TRUE, font_size = 14, 
                                  position = "left") %>% 
        gsub("font-size: initial !important;", 
             "font-size: 12px !important;", .) %>%      
        cat()
      
      break_page_asis()
    }    
  } else {
    html_cat("The number of numerical variables is 0.")    
  }

}


#' @import dplyr
html_paged_categorical <- function(.data, n_rows = 25, add_row = 3, caption = "", 
                                full_width = TRUE, font_size = 14) {
  tabs <- .data %>% 
    diagnose_category()
  
  colums <- c("variables", "levels", "observations", "frequency", "frequency(%)", "rank")
  
  print_tab(tabs, n_rows = n_rows, add_row = 3, caption = caption, 
            col.names =  colums, full_width = full_width, font_size = font_size)
}


#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
html_paged_categorical_detail <- function(.data, n_ind = 4, caption = "", 
                                       full_width = TRUE, font_size = 14) {
  cats <- .data %>% 
    find_class("categorical", index = FALSE)
  
  n_page <- ceiling(length(cats) / n_ind) 
  
  if (n_page > 0) {
    for (i in seq(n_page)) {
      vars_nm <- cats[(((i - 1) * n_ind) + 1):min(i * n_ind, length(cats))]
      
      .data %>% 
        select_at(vars_nm) %>% 
        plot_bar_category()
      
      break_line_asis(2)
      
      col.names <- c("variables", "missing", "missing(%)", "distinct", "distinct ratio")
      .data %>% 
        select_at(vars_nm) %>% 
        diagnose() %>%
        mutate(missing_percent = round(missing_percent, 2)) %>% 
        mutate(unique_rate = round(unique_rate, 3)) %>% 
        select(-types) %>% 
        knitr::kable(format = "html", col.names = col.names, 
                     format.args = list(big.mark = ",")) %>% 
        kableExtra::kable_styling(full_width = TRUE, font_size = 14, 
                                  position = "left") %>% 
        gsub("font-size: initial !important;", 
             "font-size: 12px !important;", .) %>%      
        cat()
      
      break_page_asis()
    }
  } else {
    html_cat("The number of categorical(factor/ordered) variables is 0.")
  }
}


#' @import dplyr
html_paged_normality <- function(.data, n_rows = 25, add_row = 3, caption = "", 
                                 full_width = TRUE, digits = 1, font_size = 13) {
  tabs <- dlookr::describe(.data) %>% 
    select(variable, p00, p25, p50,  p75, p100, skewness, kurtosis) %>% 
    rename("min" = p00,
           "max" = p100,
           "median" = p50,
           "Q1" = p25,
           "Q3" = p75) %>% 
    mutate(balance = case_when(
      abs(skewness) <= 1.2 ~ "Balanced",
      skewness > 1.2 ~ "Right-Skewed",
      skewness < 1.2 ~ "Left-Skewed",
      is.na(skewness) ~ "Invalid"
    )) 
  
  print_tab(tabs, n_rows = n_rows, add_row = add_row, caption = caption, 
            full_width = full_width, font_size = font_size, 
            digits = digits, big_mark = FALSE)
} 


#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
html_paged_normality_detail <- function(.data) {
  nums <- .data %>% 
    find_class("numerical", index = FALSE)
  
  for (nm in nums) {
    el <- div(h3(nm))
    cat(as.character(el))
    
    flag_sample <- FALSE
    
    x <- .data[, nm] %>%
      .[!is.na(.)]
    
    if (length(x) > 5000) {
      x <- sample(x, size = 5000, replace = TRUE)
      flag_sample <- TRUE
    }  
    
    if (length(x) == 0) {
      el <- div(h4("all values are NA"))
      cat(as.character(el))
      
      break_page_asis()
      
      next
    } else if (length(x) < 3L || length(unique(x)) < 3L) {
      el <- div(h4("(unique) sample size must be greater then 3"))
      cat(as.character(el))
      
      break_page_asis()
      
      next
    } else if (diff(range(x, na.rm = TRUE)) == 0) {
      el <- div(h4("all values are identical"))
      cat(as.character(el))
      
      break_page_asis()
      
      next
    } else {
      y <- shapiro.test(x)
      statistic <- round(y$statistic, 5)
      p_value <- y$p.value
      
      data.frame(statistic = statistic,
                 p_value = p_value,
                 remark = ifelse(flag_sample, "5000 samples", "No sample"),
                 row.names = NULL) %>% 
        dplyr::mutate_if(is.numeric, function(x) {
          as.character(signif(x, digits = 5))
        }) %>%
        knitr::kable(format = "html", digits = 7, align = "rrl",
                     caption = "Shapiro-Wilk normality test") %>% 
        kableExtra::kable_styling(full_width = FALSE, font_size = 14, 
                                  position = "center") %>% 
        gsub("font-size: initial !important;", 
             "font-size: 12px !important;", .) %>%
        cat()
      
      break_line_asis(1)
      
      if (is.factor(x)) x <- as.numeric(x)
      
      if (sum(x < 0, na.rm = TRUE) > 0) {
        type <- c("original", "log+a transformation", "Box-Cox transformation")
        
        skew <- c(skewness(x, na.rm = TRUE),
                  skewness(get_transform(x, "log+a"), na.rm = TRUE),
                  skewness(get_transform(x, "Box-Cox"), na.rm = TRUE))
        
        kurt <- c(kurtosis(x, na.rm = TRUE),
                  kurtosis(get_transform(x, "log+a"), na.rm = TRUE),
                  kurtosis(get_transform(x, "Box-Cox"), na.rm = TRUE))          
      } else {
        if (any(x == 0, na.rm = TRUE)) {
          type <- c("original", "log+1 transformation", "sqrt transformation")
          
          skew <- c(skewness(x, na.rm = TRUE),
                    skewness(get_transform(x, "log+1"), na.rm = TRUE),
                    skewness(sqrt(x), na.rm = TRUE))
          
          kurt <- c(kurtosis(x, na.rm = TRUE),
                    kurtosis(get_transform(x, "log+1"), na.rm = TRUE),
                    kurtosis(sqrt(x), na.rm = TRUE))         
        } else {
          type <- c("original", "log transformation", "sqrt transformation")
          
          skew <- c(skewness(x, na.rm = TRUE),
                    skewness(log(x), na.rm = TRUE),
                    skewness(sqrt(x), na.rm = TRUE))
          
          kurt <- c(kurtosis(x, na.rm = TRUE),
                    kurtosis(log(x), na.rm = TRUE),
                    kurtosis(sqrt(x), na.rm = TRUE)) 
        }
      }
      
      data.frame(type = type, 
                 skewness = round(skew, 4), 
                 kurtosis = round(kurt, 4)) %>% 
        knitr::kable(format = "html", caption = "skewness and kurtosis") %>% 
        kableExtra::kable_styling(full_width = FALSE, font_size = 14, 
                                  position = "center") %>%
        gsub("font-size: initial !important;", 
             "font-size: 12px !important;", .) %>% 
        cat()
      
      break_line_asis(1)
      
      x <- data.frame(x)
      
      if (sum(x < 0, na.rm = TRUE) > 0) {
        plot_normality(x, x, left = "log+a", right = "Box-Cox")
      } else {
        if (any(x == 0, na.rm = TRUE)) 
          plot_normality(x, x, left = "log+1", right = "sqrt")
        else
          plot_normality(x, x)
      }
      
      break_page_asis()
    }    
  }
} 


#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom htmltools div h3
html_paged_compare_numerical <- function(.data, n_rows = 25, add_row = 3, 
                                         caption = "Correlation coefficient", 
                                         full_width = TRUE, digits = 5, 
                                         font_size = 13) {
  idx <- .data %>% 
    find_class("numerical")
  
  if (length(idx) < 2) {
    html_cat("The number of numerical variables is less than 2.")
    break_page_asis()
  } else {
    num_compares <-  compare_numeric(.data)
    
    tabs <- num_compares$correlation %>% 
      rename("first variable" = var1,
             "second variable" = var2,
             "correlation coefficient" = coef_corr) 
    
    print_tab(tabs, n_rows = n_rows, add_row = add_row, caption = caption, 
              full_width = full_width, font_size = font_size, 
              digits = digits, big_mark = FALSE)
    
    df_linear <- num_compares$linear
    
    for (i in seq(NROW(df_linear))) {
      el <- div(h3(paste0("'", df_linear[i, "var1"], "' vs '", 
                          df_linear[i, "var2"], "'")))
      cat(as.character(el))
      
      df_linear[i, ] %>% 
        select(-logLik, -AIC, -BIC, -deviance, -df.residual, -nobs) %>% 
        rename("first variable" = var1,
               "second variable" = var2) %>% 
        knitr::kable(format = "html", caption = "Summary of linear model") %>% 
        kableExtra::kable_styling(full_width = TRUE, font_size = font_size, 
                                  position = "left") %>%
        gsub("font-size: initial !important;", 
             "font-size: 12px !important;", .) %>% 
        cat() 
      
      break_line_asis(1)
      
      num_compare <- num_compares
      
      vars <- attr(num_compare, "combination")[i, ]
      attr(num_compare, "raw") <- attr(num_compare, "raw")[, vars]
      attr(num_compare, "combination") <- vars %>% t()
      
      plot(num_compare)
      
      break_page_asis()
    }      
  }
}


#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom htmltools div h3
html_paged_compare_categorical <- function(.data, n_rows = 25, add_row = 3, 
                                           caption = "Chisqure-Test", 
                                           full_width = TRUE, digits = 5, 
                                           font_size = 13, thres_cells = 20, 
                                           thres_levels = 10) {
  plot_compare <- function(x, y, ctab) {
    xvar <- x
    yvar <- y
    
    data <- ctab %>% 
      select(a = xvar, b = yvar, n) 
    
    first <- data[1, 1] %>% pull %>% as.character
    y <- data %>% 
      filter(a %in% first) %>% 
      select(b, n)
    
    y_lab <- y$b %>% rev() %>% as.character()
    y <- y$n %>% rev()
    
    y_cumsum <- cumsum(y)
    y_center <- y / 2
    
    y_pos <- numeric(length(y))
    for (j in seq(y)) {
      if (j == 1) {
        y_pos[j] <- y_center[j]
      } else {
        y_pos[j] <- y_cumsum[j-1] + y_center[j]
      }
      y_pos[j] <- y_pos[j] / sum(y)
    }
    
    suppressWarnings({
      p <- data %>% 
        group_by(a) %>% 
        mutate(x_width = sum(n)) %>% 
        ggplot(aes(x = factor(a), y = n)) +
        geom_col(aes(width = x_width, fill = factor(b)),
                 color = "white", size = 2, 
                 position = position_fill(reverse = FALSE)) +
        facet_grid(~ a, space = "free", scales = "free", switch = "x") +
        scale_x_discrete(name = xvar) +
        scale_y_continuous(name = yvar, breaks = y_pos, labels = y_lab) +
        labs(title = sprintf("Mosaics plot by '%s' vs '%s'", xvar, yvar)) +
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              strip.background = element_blank(),
              panel.spacing = unit(0, "pt")) 
    })
    
    p <- p +
      hrbrthemes::theme_ipsum(base_family = get_font_family()) +
      hrbrthemes::scale_fill_ipsum(na.value = "grey80") +
      theme(legend.position = "none",
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            panel.spacing = unit(0, "pt"))
    
    suppressWarnings(print(p))
  }
  
  idx <- .data %>% 
    find_class("categorical")
  
  if (length(idx) < 2) {
    html_cat("The number of categorical variables is less than 2.")
    break_page_asis()
  } else {
    n_levels <- idx %>%
      sapply(function(x) {
        levels(.data[, x]) %>% 
          length()
      })
    
    idx_target <- which(n_levels <= thres_levels) %>% 
      idx[.]
    
    if (length(idx_target) < 2) {
      html_cat("The valid categorical variables is less than 2.")
      break_page_asis()
    } else {
      cat_compares <-  compare_category(.data[, idx_target]) 
      tabs <- summary(cat_compares, "all", marginal = TRUE, na.rm = FALSE, 
                      verbose = FALSE)
      tab_compare <- tabs$chisq %>% 
        filter(df <= thres_cells) %>% 
        filter(!is.nan(statistic))
      
      tab_compare <- tab_compare %>% 
        select(1, 2, 5, 3, 4) %>% 
        rename("first variable" = variable_1,
               "second variable" = variable_2,
               "degree of freedom" = df,
               "p-value" = p.value) 
      
      print_tab(tab_compare, n_rows = n_rows, add_row = add_row, caption = caption, 
                full_width = full_width, font_size = font_size, 
                digits = digits, big_mark = FALSE)
      
      for (i in seq(NROW(tab_compare))) {
        el <- div(h3(paste0("'", tab_compare[i, 1], "' vs '", tab_compare[i, 2], "'")))
        cat(as.character(el))
        
        ctable <- tabs$table[[i]]
        
        contingency <- function(tab, relate = FALSE) {
          dname <-  tab %>% dimnames()
          dframe <- tab %>% data.frame()
          
          if (relate) {
            dframe <- round(dframe / dframe[nrow(dframe), ncol(dframe)] * 100, 2)
          }
          
          rownames(dframe) <- tab %>% 
            rownames() %>% 
            ifelse(is.na(.), "<NA>", .)
          
          rname <- dname %>% names() %>% "["(1)
          varname <- ifelse(is.na(dname[[2]]), "<NA>", dname[[2]])
          
          colum_list <- seq(ncol(dframe)) %>% 
            lapply(function(x) {
              colDef(
                name = varname[x],
                format = colFormat(
                  separators = TRUE
                ),
                sortable = FALSE
              )
            }) 
          names(colum_list) <- names(dframe)
          
          cname <- list(
            colGroup(name = dname %>% names() %>% "["(2), 
                     columns = names(dframe))
          )
          
          dframe
        }
        
        x <- ctable %>% dimnames() %>% names() %>% "["(1)
        y <- ctable %>% dimnames() %>% names() %>% "["(2)
        idx_nm <- paste(x, y, sep = " vs ")
        
        header_above <- c(1, NCOL(ctable))
        names(header_above) <- c(tab_compare[i, 1], tab_compare[i, 2])
        
        ctable %>% 
          knitr::kable(format = "html") %>% 
          kableExtra::add_header_above(header_above) %>% 
          kableExtra::kable_styling(full_width = TRUE, font_size = font_size, 
                                    position = "left") %>%
          cat() 
        break_line_asis(1)  
        
        total <- ctable[NROW(ctable),  NCOL(ctable)]
        
        round(ctable / total * 100, 2) %>% 
          knitr::kable(format = "html") %>% 
          kableExtra::add_header_above(header_above) %>% 
          kableExtra::kable_styling(full_width = TRUE, font_size = font_size, 
                                    position = "left") %>%
          cat() 
        break_line_asis(1)  
        
        plot_compare(x, y, cat_compares[[i]])
        break_page_asis()
      } 
    }
  }
}

#' @importFrom tidyr spread
#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above
html_paged_correlation <- function(.data, full_width = TRUE, font_size = 13) {
  idx <- .data %>% 
    find_class("numerical")
  
  if (length(idx) < 2) {
    html_cat("The number of numerical variables is less than 2.")
    break_page_asis()
  } else {
    mat_corr <- correlate(.data) %>% 
      tidyr::spread(var2, coef_corr) 
    
    header_above <- c(1, NCOL(mat_corr) - 1)
    names(header_above) <- c(" ", "second variable")
    
    names(mat_corr)[1] <- "first variable"
    
    caption <- "Matrix table of correlation coefficient"
    
    mat_corr %>% 
      knitr::kable(format = "html", digits = 3, caption = caption) %>% 
      kableExtra::add_header_above(header_above) %>% 
      kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                position = "left") %>%
      gsub("font-size: initial !important;",
           "font-size: 12px !important;", .) %>%        
      cat() 
    
    break_page_asis()
  }
}


#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above
html_paged_target_numerical <- function(.data, target, full_width = TRUE, 
                                        font_size = 13) {
  if (is.null(target)) {
    html_cat("The target variable is not defied.")    
  } else if (!target %in% names(.data)) {
    html_cat("The data does not contain the variable specified for target.")    
  } else {
    nm_numeric <- .data %>% 
      find_class("numerical", index = FALSE) %>% 
      setdiff(target)
    
    if (length(nm_numeric) < 1) {
      html_cat("There are no numerical variables except for the target variable.")
    } else {
      suppressWarnings({
        tab_main <- data.frame(Variable = nm_numeric, target_variable = target)
      })
      
      tgt_by <- .data %>% 
        target_by(target)
      
      for (i in seq(NROW(tab_main))) {
        nm_var <- nm_numeric[i]
        
        el <- div(h3(nm_var))
        cat(as.character(el))
        
        plot_outlier(tgt_by, all_of(nm_var))
        
        if (i == 1) {
          break_line_asis(20)
        } else {
          break_line_asis(1)
        }
        
        mat <- relate(tgt_by, all_of(nm_var)) %>% 
          select(-se_mean, -p10, -p20, -p30, -p40, -p60, -p70, -p80, -p90) %>% 
          select(-variable) %>% 
          t()
        
        colnames(mat) <- mat[1, ] %>%
          ifelse(. %in% "total", "<Total>", .)
        mat  <- mat[-1, ] 
        
        rownames(mat) <- c("N", "Missing", "Mean", "Standard Deviation", 
                           "IQR", "Skewness", "Kurtosis", "Min", "1%", "5%", 
                           "Q1", "Median", "Q3", "95%", "99%", "Max")
        
        header_above <- c(1, NCOL(mat))
        names(header_above) <- c(" ", target)
        
        caption <- "Descriptive statistics with levels of target variable"
        
        mat %>% 
          knitr::kable(format = "html", digits = 3, caption = caption) %>% 
          kableExtra::add_header_above(header_above) %>% 
          kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                    position = "left") %>%
          gsub("font-size: initial !important;",
               "font-size: 12px !important;", .) %>%          
          cat() 
        
        break_page_asis()
      }
    }    
  } 
}


#' @importFrom htmltools div
#' @import dplyr
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above
#' @importFrom stats addmargins
html_paged_target_categorical <- function(.data, target, full_width = TRUE, 
                                        font_size = 13) {
  if (is.null(target)) {
    html_cat("The target variable is not defied.")    
    break_page_asis()
  } else if (!target %in% names(.data)) {
    html_cat("The data does not contain the variable specified for target.")  
    break_page_asis()
  } else {
    nm_categorical <- .data %>% 
      find_class("categorical", index = FALSE) %>% 
      setdiff(target)
    
    if (length(nm_categorical) < 1) {
      html_cat("There are no categorical variables except for the target variable.")
      break_page_asis()
    } else {
      suppressWarnings({
        tab_main <- data.frame(Variable = nm_categorical, target_variable = target)
      })
      
      tgt_by <- .data %>% 
        target_by(target)
      
      for (i in seq(NROW(tab_main))) {
        nm_var <- nm_categorical[i]
        
        el <- div(h3(nm_var))
        cat(as.character(el))
        
        print(plot(relate(tgt_by, all_of(nm_var))))
        
        if (i == 1 & length(levels(.data[, nm_var])) > 12) {
          break_line_asis(20)
        } else {
          break_line_asis(1)
        }
        
        mat <- relate(tgt_by, all_of(nm_var)) %>% 
          stats::addmargins() %>% 
          as.data.frame() %>% 
          tidyr::spread(all_of(target), Freq) 
        
        names(mat) <- names(mat) %>% 
          ifelse(. %in% "Sum", "<Total>", .)
        mat[, 1] <- mat[, 1] %>% 
          as.character(.) %>% 
          ifelse(. %in% "Sum", "<Total>", .)
        
        header_above <- c(1, NCOL(mat) - 1)
        names(header_above) <- c(" ", target)
        
        caption <- "Contingency table with target variable"
        
        mat %>% 
          knitr::kable(format = "html", digits = 3, caption = caption) %>% 
          kableExtra::add_header_above(header_above) %>% 
          kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                    position = "left") %>%
          gsub("font-size: initial !important;",
               "font-size: 12px !important;", .) %>%            
          cat() 
        
        break_page_asis()
      }
    }    
  } 
}


#' @importFrom htmltools h3 h4 div
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above
#' @import dplyr
html_paged_target_correlation <- function(.data, target, full_width = TRUE, 
                                          font_size = 13) {
  if (is.null(target)) {
    html_cat("The target variable is not defied.")    
    break_page_asis()
  } else if (!target %in% names(.data)) {
    html_cat("The data does not contain the variable specified for target.")    
    break_page_asis()
  } else {
    nm_numeric <- .data %>% 
      find_class("numerical", index = FALSE) %>% 
      setdiff(target)
    
    if (length(nm_numeric) < 2) {
      html_cat("The number of numerical variables is less than 2.")
      break_page_asis()
    } else {
      target_levels <- .data[, target] %>% 
        unique()
      
      for (i in seq(target_levels)) {
        value <- target_levels[i]
        
        el <- div(h3(paste(target, value, sep = " : ")))
        cat(as.character(el))
        
        el <- div(h4("Correlation Coefficient Matrix"))
        cat(as.character(el))
        
        target2 <- paste0("^", target, "$")
        
        data_filterd <- .data %>% 
          filter_at(vars(matches(target2)), 
                    any_vars(. %in% value))
        
        mat_corr <- data_filterd %>% 
          correlate() %>% 
          tidyr::spread(var2, coef_corr)
        
        header_above <- c(1, NCOL(mat_corr) - 1)
        names(header_above) <- c(" ", "second variable")
        
        names(mat_corr)[1] <- "first variable"
        
        caption <- "Matrix table of correlation coefficient"
        
        mat_corr %>% 
          knitr::kable(format = "html", digits = 3, caption = caption) %>% 
          kableExtra::add_header_above(header_above) %>% 
          kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                    position = "left") %>%
          gsub("font-size: initial !important;",
               "font-size: 12px !important;", .) %>%             
          cat() 
        
        break_page_asis()
        
        el <- div(h3(paste(target, value, sep = " : ")))
        cat(as.character(el))
        
        el <- div(h4("Correlation Plot"))
        cat(as.character(el))
        
        plot_correlate(data_filterd)
        
        break_page_asis()
      }
    }    
  } 
}



