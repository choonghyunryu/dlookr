#' @importFrom shiny icon
#' @importFrom htmltools div
unique_indicator <- function(value, word = FALSE, theme = c("orange", "blue")[1]) {
  style <- ifelse(theme == "orange", "color: rgb(255, 127, 42)", 
                  "color: rgb(0, 114, 188)")
  
  if (value %in% "identifier") {
    args <- shiny::icon("key", style = style)
    
    div(args, paste("", value))    
  } else if (value %in% "constant") {
    args <- shiny::icon("thumbtack", style = style)
    
    div(args, paste("", value))
  } else if (value %in% "high cardinality") {
    if (word) value <- "high"
    args <- shiny::icon("greater-than", style = style)
    
    div(args, paste("", value))
  } else if (value %in% "low cardinality") {
    if (word) value <- "low"
    args <- shiny::icon("less-than", style = style)
    
    div(args, paste("", value))
  } else {
    args <- list(role = "img")
    args <- c(args, list(shiny::icon("check"), 
                         style = style))    
    do.call(span, args)    
  }
}


#' @importFrom purrr map_df
#' @import dplyr
#' @import reactable
html_toprank <- function(.data, variable = NULL, drop_variable = FALSE) {
  N <- nrow(.data)
  
  top10 <- diagnose_category(.data, top = 10, type = "n") %>% 
    filter(!is.na(levels))
  
  nas <- top10$variables %>% 
    unique %>% 
    purrr::map_df(function(x) {
      freq <- .data[, x] %>% 
        is.na %>% 
        sum
      data.frame(variables = x, levels = "Missing", N = N,
                 freq = freq,
                 ratio =  freq / N, rank = 12)
    }) %>% 
    filter(freq > 0)
  
  merge_top <- top10 %>% 
    bind_rows(nas) 
  
  others <- merge_top %>% 
    group_by(variables) %>% 
    summarise(top_freq = sum(freq), .groups = "drop") %>% 
    mutate(other_freq = N - top_freq) %>% 
    filter(other_freq > 0) %>% 
    mutate(levels = "Other levles",
           N = N,
           freq = other_freq,
           ratio = freq / N,
           rank = 11) %>% 
    select(-top_freq, -other_freq)
  
  top_rank <- merge_top %>% 
    bind_rows(others) %>% 
    mutate(ratio = freq / N) %>% 
    arrange(variables, rank) %>% 
    select(-N, -rank) 
  
  if (!is.null(variable)) {
    top_rank <- top_rank %>% 
      filter(variables %in% variable)
  }
  
  if (drop_variable) {
    top_rank <- top_rank %>% 
      select(-variables)
  }  
  # Render a bar chart in the background of the cell
  bar_style <- function(width = 1, fill = "#e6e6e666", levels = NULL) {
    fill <- ifelse(levels %in% "Missing", "#cdcdcd99", fill)
    fill <- ifelse(levels %in% "Other levles", "#0072bc66", fill)
    
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", 
                     fill, position)
    list(
      backgroundImage = image,
      backgroundSize = "100% 75%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center",
      fontSize = "14px",
      color = "#666666"
    )
  }
  
  reactable(top_rank, defaultPageSize = nrow(top_rank),
            defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
            columns = list(
              levels = colDef(
                name = "Levels"
              ),
              freq = colDef(
                name = "Frequency",
                style = function(value, index) {
                  bar_style(width = value / max(top_rank$freq), fill = "#ff7f2a66",
                            levels = top_rank$levels[index])
                },
                format = colFormat(separators = TRUE),
                align = "left"
              ),
              ratio = colDef(name = "Percent", 
                             format = colFormat(percent = TRUE, digits = 1))  
            ),
            ) 
}


#' @importFrom purrr map_df
#' @import dplyr
#' @import reactable
html_paged_toprank <- function(.data, top = 10, type = "n", variable = NULL, 
                               drop_variable = FALSE) {
  N <- nrow(.data)
  
  top10 <- diagnose_category(.data, top = 10, type = "n") %>% 
    filter(!is.na(levels))
  
  nas <- top10$variables %>% 
    unique %>% 
    purrr::map_df(function(x) {
      freq <- .data[, x] %>% 
        is.na %>% 
        sum
      data.frame(variables = x, levels = "Missing", N = N,
                 freq = freq,
                 ratio =  freq / N, rank = 12)
    }) %>% 
    filter(freq > 0)
  
  merge_top <- top10 %>% 
    bind_rows(nas) 
  
  others <- merge_top %>% 
    group_by(variables) %>% 
    summarise(top_freq = sum(freq), .groups = "drop") %>% 
    mutate(other_freq = N - top_freq) %>% 
    filter(other_freq > 0) %>% 
    mutate(levels = "Other levles",
           N = N,
           freq = other_freq,
           ratio = freq / N,
           rank = 11) %>% 
    select(-top_freq, -other_freq)
  
  top_rank <- merge_top %>% 
    bind_rows(others) %>% 
    mutate(ratio = freq / N) %>% 
    arrange(variables, rank) %>% 
    select(-N, -rank) 
  
  if (!is.null(variable)) {
    top_rank <- top_rank %>% 
      filter(variables %in% variable)
  }
  
  if (drop_variable) {
    top_rank <- top_rank %>% 
      select(-variables)
  }  

  top_rank %>% 
    mutate(ratio = round(ratio * 100, 1)) %>% 
    rename("ratio (%)" = ratio)
}



#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import ggplot2
#' @import reactable
html_variable <- function(.data, thres_uniq_cat = 0.5, thres_uniq_num = 5,
                          theme = c("orange", "blue")[1], base_family = NULL) {
  style <- ifelse(theme == "orange", "color: rgb(255, 127, 42)", 
                  "color: rgb(0, 114, 188)")
  
  alert_indicator <- function(value) {
    args <- list(role = "img")
    
    if (is.na(value)) {
      args <- c(args, list("-", style = "color: #666; font-weight: 700"))
    } else if (value == 1) {
      args <- c(args, list(shiny::icon("exclamation-triangle"), 
                           style = style))
    } else if (value == 0) {
      args <- c(args, list(shiny::icon("check"), style = style))
    } 
    
    do.call(span, args)
  }
  
  in_numeric <- find_class(.data, "numeric") %>% 
    length() %>% 
    as.logical()
  
  N <- nrow(.data)
  
  if (in_numeric) {
    raws <- .data %>% 
      diagnose() %>% 
      left_join(
        .data %>% 
          diagnose_numeric(),
        by = "variables"
      ) %>% 
      rename("zero_count" = zero,
             "minus_count" = minus,
             "outlier_count" = outlier)
  } else {
    raws <- .data %>% 
      diagnose() %>% 
      mutate(min = NA,
             Q1 = NA,
             mean = NA,
             median = NA,
             Q3 = NA,
             max = NA,
             zero_count = NA,
             minus_count = NA,
             outlier_count = NA)
  }
  
  tabs <- raws %>% 
    mutate(missing = ifelse(missing_count > 0, 1, 0)) %>% 
    mutate(cardinality = ifelse(types %in% c("character", "factor", "ordered", 
                                             "Date", "POSIXct") &
                                unique_rate >= thres_uniq_cat & unique_rate < 1, 
                                "high cardinality", NA)) %>%
    mutate(cardinality = ifelse(types %in% c("numeric", "integer") &
                                unique_count <= thres_uniq_num, "low cardinality", 
                                cardinality)) %>% 
    mutate(cardinality = ifelse(unique_rate == 1, "identifier", cardinality)) %>% 
    mutate(cardinality = ifelse(unique_count == 1, "constant", cardinality)) %>%    
    mutate(cardinality = ifelse(is.na(cardinality), "normal", cardinality)) %>%  
    mutate(zero = ifelse(zero_count > 0, 1, 0)) %>% 
    mutate(minus = ifelse(minus_count > 0, 1, 0)) %>% 
    mutate(outlier = ifelse(outlier_count > 0, 1, 0)) %>% 
    select(variables, types, missing:outlier)
  
  reactable(
    tabs,
    # defaultPageSize = nrow(tabs),
    #fullWidth = FALSE,
    defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
    columns = list(
      variables = colDef(
      #   cell = function(value) a(name = value, value)
        name = "Variables",
        #width = 250
      ),
      types = colDef(
        name = "Types",
        #width = 90
      ),      
      missing = colDef(
        name = "Missing",
        align = "center",
        #width = 100,
        cell = function(value) alert_indicator(value)
      ),
      cardinality = colDef(
        name = "Cardinality",
        align = "center",
        #width = 120,
        cell = function(value) unique_indicator(value, TRUE, theme)
      ),
      zero = colDef(
        name = "Zero",
        align = "center",
        #width = 90,
        cell = function(value) alert_indicator(value)
      ),    
      minus = colDef(
        name = "Negative",
        align = "center",
        #width = 100,
        cell = function(value) alert_indicator(value)
      ),
      outlier = colDef(
        name = "Outlier",
        align = "center",
        #width = 90,
        cell = function(value) alert_indicator(value)
      )
    ),
    details = function(index) {
      variable <- tabs$variables[index]
      type <- tabs$types[index]
      
      suppressWarnings(
      unique_missing <- raws %>% 
        select(variables, missing_count, unique_count, zero_count, minus_count, 
               outlier_count) %>% 
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
            metric = colDef(
              name = "Metric"
            ),
            count = colDef(
              name = "Frequency", 
              format = colFormat(separators = TRUE)
            ),
            percent = colDef(
              name = "Percent", 
              format = colFormat(percent = TRUE, digits = 1)
            )
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
          select(min:max) %>% 
          mutate(mean = round(mean, 3)) %>% 
          reactable(fullWidth = FALSE,
                    defaultColDef = colDef(
                      style = "font-size: 14px;color: hsl(0, 0%, 40%);"
                    ),
                    columns = list(
                      min = colDef(
                        name = "Min"
                      ),
                      mean = colDef(
                        name = "Mean"
                      ),
                      median = colDef(
                        name = "Median"
                      ),
                      max = colDef(
                        name = "Max"
                      )      
                    )
          )

        p_hist <- htmltools::plotTag({
          plot_hist_numeric(.data, which(names(.data) == variable), 
                            base_family = base_family)
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
                          style = "padding-top:5px; padding-bottom:25px;")
        )
      }
    }
  )
}


#' @import dplyr
html_paged_variable <- function(.data, thres_uniq_cat = 0.5, thres_uniq_num = 5,
                                n_rows = 25, add_row = 3, caption = "", 
                                full_width = TRUE, font_size = 14) {
  in_numeric <- find_class(.data, "numeric") %>% 
    length() %>% 
    as.logical()
  
  N <- nrow(.data)
  
  if (in_numeric) {
    raws <- .data %>% 
      diagnose() %>% 
      left_join(
        .data %>% 
          diagnose_numeric(),
        by = "variables"
      ) %>% 
      rename("zero_count" = zero,
             "minus_count" = minus,
             "outlier_count" = outlier)
  } else {
    raws <- .data %>% 
      diagnose() %>% 
      mutate(min = NA,
             Q1 = NA,
             mean = NA,
             median = NA,
             Q3 = NA,
             max = NA,
             zero_count = NA,
             minus_count = NA,
             outlier_count = NA)
  }
  
  tabs <- raws %>% 
    mutate(missing = ifelse(missing_count > 0, "X", "")) %>% 
    mutate(cardinality = ifelse(types %in% c("character", "factor", "ordered", 
                                             "Date", "POSIXct") &
                                  unique_rate >= thres_uniq_cat & unique_rate < 1, 
                                "> high", "")) %>%
    mutate(cardinality = ifelse(types %in% c("numeric", "integer") &
                                  unique_count <= thres_uniq_num, "< low", 
                                cardinality)) %>% 
    mutate(cardinality = ifelse(unique_rate == 1, "identifier", cardinality)) %>% 
    mutate(cardinality = ifelse(unique_count == 1, "constant", cardinality)) %>%    
    mutate(cardinality = ifelse(is.na(cardinality), "normal", cardinality)) %>%  
    mutate(zero = ifelse(is.na(zero_count), "", 
                         ifelse(zero_count > 0, "X",""))) %>% 
    mutate(minus = ifelse(is.na(minus_count), "", 
                          ifelse(minus_count > 0, "X",""))) %>% 
    mutate(outlier = ifelse(is.na(outlier_count), "", 
                            ifelse(outlier_count > 0, "X",""))) %>% 
    select(variables, types, missing:outlier)
  
  print_tab(tabs, n_rows = n_rows, add_row = 3, caption = caption, 
            full_width = full_width, font_size = font_size, align = "llccccc")
}


#' @import dplyr
#' @import htmltools
#' @import reactable
html_missing <- function(tab, grade = c("Good" = 0.05, "OK" = 0.1, 
                                        "NotBad" = 0.2, "Bad" = 0.5, 
                                        "Remove" = 1),
                         recommand = c("Delete or Imputation", 
                                       "Delete or Imputation",
                                       "Model based Imputation", 
                                       "Model based Imputation", 
                                       "Remove Variable")) {
  diagn_missing <- tab %>%
    filter(missing_count > 0) %>% 
    select(variables, missing_count, missing_percent) %>% 
    mutate(missing_percent = missing_percent / 100) %>% 
    mutate(status = cut(missing_percent, breaks = c(0, grade),
                        labels = names(grade))) %>% 
    mutate(recommand = recommand[status]) %>%   
    arrange(desc(missing_count))
  
  if (NROW(diagn_missing) > 0) {
    cap <- "Variables that include missing values"
    
    html_cat(cap)
    
    status_badge <- function(color = "#aaa", width = "9px", height = width) {
      span(style = list(
        display = "inline-block",
        marginRight = "8px",
        width = width,
        height = height,
        backgroundColor = color,
        borderRadius = "50%"
      ))
    }
    
    # c("#D9EF8B", "#FEE08B", "#FDAE61", "#F46D43", "#D73027")  
    reactable(
      diagn_missing,
      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
      columns = list(
        variables = colDef(name = "Variables"),  
        missing_count = colDef(name = "Missing",
                               width = 120,
                               format = colFormat(separators = TRUE)),    
        missing_percent = colDef(name = "Missing (%)",
                                 width = 130,
                                 format = colFormat(percent = TRUE, digits = 1)),
        status = colDef(width = 100,
                        name = "Status",
                        cell = function(value) {
                          color <- switch(as.character(value),
                                          Good = "#D9EF8B",
                                          OK = "#FEE08B",
                                          NotBad = "#FDAE61",
                                          Bad = "#F46D43",
                                          Remove = "#D73027")
                          badge <- status_badge(color = color)
                          tagList(badge, value)
                        }),
        recommand = colDef(
          name = "Recommand"
        )
      )
    )
    
  } else {
    html_cat("No variables including missing values")
    break_line_asis(1)
  }
}


#' @import dplyr
#' @import htmltools
#' @import reactable
html_paged_missing <- function(tab, grade = c("Good" = 0.05, "OK" = 0.1, 
                                        "NotBad" = 0.2, "Bad" = 0.5, 
                                        "Remove" = 1),
                               recommand = c("Delete or Imputation", 
                                             "Delete or Imputation",
                                             "Model based Imputation", 
                                             "Model based Imputation", 
                                             "Remove Variable"),
                               n_rows = 25, add_row = 3, caption = "", 
                               full_width = TRUE, font_size = 14) {
  diagn_missing <- tab %>%
    filter(missing_count > 0) %>% 
    select(variables, missing_count, missing_percent) %>% 
    mutate(missing_percent = missing_percent / 100) %>% 
    mutate(status = cut(missing_percent, breaks = c(0, grade),
                        labels = names(grade))) %>% 
    mutate(recommand = recommand[status]) %>%   
    mutate(missing_percent = paste0(round(missing_percent * 100, 1), "%")) %>%     
    arrange(desc(missing_count)) %>% 
    rename("missing" = missing_percent,
           "missing (%)" = missing_percent)
  
  if (NROW(diagn_missing) > 0) {
    cap <- "Variables that include missing values"
    
    print_tab(diagn_missing, n_rows = n_rows, add_row = 3, caption = caption, 
              full_width = full_width, font_size = font_size, align = "lrrll")

  } else {
    html_cat("No variables including missing values")
    break_line_asis(1)
  }
}


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom purrr map_df
#' @import dplyr
#' @import htmltools
html_outlier <- function(.data, theme = c("orange", "blue")[1],
                         base_family = NULL) {
  style <- ifelse(theme == "orange", "color: rgb(255, 127, 42)", 
                  "color: rgb(0, 114, 188)")
  
  outlier_indicator <- function(value) {
    if (value %in% "Both") {
      args <- shiny::icon("arrows-alt-v", style = style)
    } else if (value %in% "Lower") {
      args <- shiny::icon("arrow-alt-circle-down", style = style)
    } else if (value %in% "Upper") {
      args <- shiny::icon("arrow-alt-circle-up", style = style)
    } 
    
    div(args, paste("", value))
  }
  
  .data <- as.data.frame(.data)
  
  N <- nrow(.data)
  
  list_outlier <- diagnose_numeric(.data) %>% 
    select(-mean, -median, -zero, -minus) %>% 
    mutate(rate_outlier = outlier / N) %>% 
    filter(outlier > 0)
  
  suppressWarnings({
  out_position <- list_outlier$variables %>%  
    purrr::map_df(function(x){
      outs <- boxplot.stats(.data[, x])$out

      Q1 <- list_outlier %>% 
        filter(variables %in% x) %>% 
        select(Q1) %>% 
        pull()
      
      Q3 <- list_outlier %>% 
        filter(variables %in% x) %>% 
        select(Q3) %>% 
        pull()      
      
      flag_lower <-any(Q1 > outs)
      flag_upper <-any(Q3 < outs)
      
      position <- ifelse(flag_lower & flag_upper, "Both", 
                         ifelse(flag_lower, "Lower", "Upper"))
      data.frame(variables = x, pos_outlier = position)
    })
  })
  
  tabs <- list_outlier %>% 
    left_join(out_position, by = "variables")
  
  tabs %>% 
    reactable(
      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
      columns = list(
        variables = colDef(
          name = "Variables"
        ),
        min = colDef(
          name = "Min"
        ),  
        max = colDef(
          name = "Max"
        ),          
        rate_outlier = colDef(name = "Outliers (%)",
                              format = colFormat(percent = TRUE, digits = 1)),
        outlier = colDef(name = "Outliers",
                         format = colFormat(separators = TRUE)),
        pos_outlier = colDef(name = "Position",
          cell = function(value) outlier_indicator(value)
        )    
      ),
      details = function(index) {
        variable <- tabs$variables[index]
        
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
          plot_outlier(.data, variable, base_family = base_family)
        }, sprintf("A plot of the %s variable", variable), width = 600,
        height = 400, device = grDevices::png)
        
        shiny::tabsetPanel(
          shiny::tabPanel("Distirubution", p_outlier,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),          
          shiny::tabPanel("Statistics", outlier_df,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")
        )
      }
    )  
} 


#' @importFrom shiny icon
#' @import dplyr
#' @import htmltools
#' @import reactable
html_unique_cat <- function(tab, thres) {
  diagn_uniq_cat <- tab %>%
    filter(types %in% c("character", "factor", "ordered", "Date", "POSIXct")) %>%
    filter(unique_rate >= thres | unique_count == 1) %>%
    select(-missing_count, -missing_percent) %>% 
    mutate(status = ifelse(unique_rate >= thres & unique_rate < 1, 
                           "high cardinality", "")) %>%
    mutate(status = ifelse(unique_rate <= thres, "low cardinality", status)) %>% 
    mutate(status = ifelse(unique_rate == 1, "identifier", status)) %>% 
    mutate(status = ifelse(unique_count == 1, "constant", status)) %>%    
    mutate(recommand = case_when(
      status %in% "constant" ~ "Remove Variable",
      status %in% "identifier" ~ "Use as ID",
      status %in% c("high cardinality", "low cardinality") ~ "Judgment"
    ))
  
  if (NROW(diagn_uniq_cat) > 0) {
    cap <- sprintf("Variables where the proportion of unique data is more than %s or unique is 1.",
                   thres) 
    html_cat(cap)
    
    reactable(
      diagn_uniq_cat,
      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
      columns = list(
        unique_count = colDef(
          name = "unique",
          width = 120,
          format = colFormat(separators = TRUE)
        ),    
        unique_rate = colDef(
          name = "unique (%)",
          width = 130,
          format = colFormat(percent = TRUE, digits = 1)
        ),
        status = colDef(
          name = "status",
          cell = function(value) unique_indicator(value)
        ) 
      )
    )
  } else {
    cap <- sprintf("No variable with a high proportion greater than %s", thres) 
    html_cat(cap)
    
    break_line_asis(1)
  }
}


#' @import dplyr
html_paged_unique_cat <- function(tab, thres, n_rows = 25, add_row = 3, caption = "", 
                                  full_width = TRUE, font_size = 14) {
  
  diagn_uniq_cat <- tab %>%
    filter(types %in% c("character", "factor", "ordered", "Date", "POSIXct")) %>%
    filter(unique_rate >= thres | unique_count == 1) %>%
    select(-missing_count, -missing_percent) %>% 
    mutate(status = ifelse(unique_rate >= thres & unique_rate < 1, 
                           "high cardinality", "")) %>%
    mutate(status = ifelse(unique_rate <= thres, "low cardinality", status)) %>% 
    mutate(status = ifelse(unique_rate == 1, "identifier", status)) %>% 
    mutate(status = ifelse(unique_count == 1, "constant", status)) %>%    
    mutate(unique_rate = paste0(round(unique_rate * 100, 1), "%")) %>%    
    mutate(recommand = case_when(
      status %in% "constant" ~ "Remove Variable",
      status %in% "identifier" ~ "Use as ID",
      status %in% c("high cardinality", "low cardinality") ~ "Judgment"
    )) %>% 
    rename("unique" = unique_count,
           "unique (%)" = unique_rate)
  
  if (NROW(diagn_uniq_cat) > 0) {
    cap <- sprintf("Variables where the proportion of unique data is more than %s or unique is 1.",
                   thres) 
    html_cat(cap)
    
    print_tab(diagn_uniq_cat, n_rows = n_rows, add_row = 3, 
              caption = "Detail warning categorical cardinality", 
              full_width = full_width, font_size = font_size, align = "llrrll")
      
  } else {
    cap <- sprintf("No variable with a high proportion greater than %s", thres) 
    html_cat(cap)
    
    # break_line_asis(1)
    break_page_asis()
  }
}

#' @importFrom shiny icon
#' @import dplyr
#' @import htmltools
#' @import reactable
html_unique_num <- function(tab, thres) {
  diagn_uniq_num <- tab %>%
    filter(types %in% c("numeric", "integer")) %>%
    filter(unique_count <= thres) %>%
    select(-missing_count, -missing_percent) %>% 
    mutate(status = ifelse(unique_count <= thres, "low cardinality", "")) %>% 
    mutate(status = ifelse(unique_rate == 1, "identifier", status)) %>% 
    mutate(status = ifelse(unique_count == 1, "constant", status)) %>%    
    mutate(recommand = case_when(
      status %in% "constant" ~ "Remove Variable",
      status %in% "identifier" ~ "Use as ID",
      status %in% c("low cardinality") ~ "Judgment"
    ))
  
  if (NROW(diagn_uniq_num) > 0) {
    cap <- sprintf("Variables where the unique cases is less than %s or unique is 1.",
                   thres) 
    html_cat(cap)
    
    unique_indicator <- function(value) {
      if (value %in% "identifier") {
        args <- shiny::icon("key", 
                            style = "color: rgb(255, 127, 42)")
      } else if (value %in% "constant") {
        args <- shiny::icon("thumbtack", 
                            style = "color: rgb(255, 127, 42)")
      } else if (value %in% "high cardinality") {
        args <- shiny::icon("greater-than", 
                            style = "color: rgb(255, 127, 42)")
      } else if (value %in% "low cardinality") {
        args <- shiny::icon("less-than", 
                            style = "color: rgb(255, 127, 42)")
      } 
      
      div(args, paste("", value))
    }
    
    reactable(
      diagn_uniq_num, 
      columns = list(
        unique_count = colDef(
          name = "unique",
          width = 120,
          format = colFormat(separators = TRUE)
        ),    
        unique_rate = colDef(
          name = "unique (%)",
          width = 130,
          format = colFormat(percent = TRUE, digits = 1)
        ),
        status = colDef(
          name = "status",
          cell = function(value) unique_indicator(value)
        ) 
      )
    )
  } else {
    cap <- sprintf("No variable with unique data proportion less than %s", thres) 
    html_cat(cap)
    
    break_line_asis(1)
  }
}


#' @import dplyr
html_paged_unique_num <- function(tab, thres, n_rows = 25, add_row = 3, 
                                  caption = "", full_width = TRUE, font_size = 14) {
  diagn_uniq_num <- tab %>%
    filter(types %in% c("numeric", "integer")) %>%
    filter(unique_count <= thres) %>%
    select(-missing_count, -missing_percent) %>% 
    mutate(status = ifelse(unique_count <= thres, "low cardinality", "")) %>% 
    mutate(status = ifelse(unique_rate == 1, "identifier", status)) %>% 
    mutate(status = ifelse(unique_count == 1, "constant", status)) %>%
    mutate(unique_rate = paste0(round(unique_rate * 100, 1), "%")) %>%       
    mutate(recommand = case_when(
      status %in% "constant" ~ "Remove Variable",
      status %in% "identifier" ~ "Use as ID",
      status %in% c("low cardinality") ~ "Judgment"
    )) %>% 
    rename("unique" = unique_count,
           "unique (%)" = unique_rate)
  
  if (NROW(diagn_uniq_num) > 0) {
    cap <- sprintf("Variables where the unique cases is less than %s or unique is 1.",
                   thres) 
    html_cat(cap)
    
    print_tab(diagn_uniq_num, n_rows = n_rows, add_row = 3, 
              caption = "Detail warning numerical cardinality", 
              full_width = full_width, font_size = font_size, align = "llrrll")
  } else {
    cap <- sprintf("No variable with unique data proportion less than %s", thres) 
    html_cat(cap)
    
    # break_line_asis(1)
    break_page_asis()
  }
}



  
