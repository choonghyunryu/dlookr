#' @rdname target_by.data.frame
#' @export
target_by <- function(.data, target, ...) {
  UseMethod("target_by", .data)
}


#' Target by one variables
#'
#' @description
#' In the data analysis, a target_df class is created to identify the
#' relationship between the target variable and the other variable.
#'
#' @details
#' Data analysis proceeds with the purpose of predicting target variables that
#'  correspond to the facts of interest, or examining associations and
#'  relationships with other variables of interest.
#'  Therefore, it is a major challenge for EDA to examine the relationship
#'  between the target variable and its corresponding variable.
#'  Based on the derived relationships, analysts create scenarios for data analysis.
#'
#'  target_by() inherits the \code{\link{grouped_df}} class and returns a target_df
#'  class containing information about the target variable and the variable.
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target target variable.
#' @param ... arguments to be passed to methods.
#' @return an object of target_df class.
#' Attributes of target_df class is as follows.
#' \itemize{
#' \item type_y : the data type of target variable.
#' }
#' @seealso \code{\link{relate}}.
#' @examples
#' \donttest{
#' # If the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#' 
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, sodium)
#' cat_num
#' summary(cat_num)
#' 
#' plot(cat_num)
#' 
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, hblood_pressure)
#' cat_cat
#' summary(cat_cat)
#'  
#' plot(cat_cat)
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' num <- target_by(heartfailure, creatinine)
#' 
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, sodium)
#' num_num
#' summary(num_num)
#' 
#' plot(num_num)
#' 
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, smoking)
#' num_cat
#' summary(num_cat)
#' 
#' plot(num_cat)
#' 
#' # Non typographic
#' plot(num_cat, typographic = FALSE)
#' }
#' 
#' @method target_by data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang enquo
#' @export
target_by.data.frame <- function(.data, target, ...) {
  tryCatch(vars <- tidyselect::vars_select(names(.data), !! rlang::enquo(target)),
    error = function(e) {
      pram <- as.character(substitute(target))
      stop(sprintf("Column %s is unknown", pram))
    }, finally = NULL)

  target_by_impl(.data, vars)
}

#' @import dplyr
#' @importFrom methods is
target_by_impl <- function(.data, target) {
  if (!target %in% names(.data)) {
    stop(sprintf("%s in not variable in %s", target,
      as.character(substitute(.data))))
  }

  target_by <- grouped_df(.data, target)

  attr(target_by , "type_y") <- is(.data[, target][[1]])[1]

  if (attr(target_by, "type_y") == "character") {
    warning("The target variable was assigned a character type.")
  }

  class(target_by) <- append("target_df", class(target_by))

  target_by
}


#' Relationship between target variable and variable of interest
#'
#' @description The relationship between the target variable and
#' the variable of interest (predictor) is briefly analyzed.
#'
#' @details Returns the four types of results that correspond to the combination
#' of the target variable and the data type of the variable of interest.
#'
#' \itemize{
#'   \item target variable: categorical variable
#'   \itemize{
#'     \item predictor: categorical variable
#'     \itemize{
#'       \item contingency table
#'       \item c("xtabs", "table") class
#'     }
#'     \item predictor: numerical variable
#'     \itemize{
#'       \item descriptive statistic for each levels and total observation.
#'     }
#'   }
#'   \item target variable: numerical variable
#'   \itemize{
#'     \item predictor: categorical variable
#'     \itemize{
#'       \item ANOVA test. "lm" class.
#'     }
#'     \item predictor: numerical variable
#'     \itemize{
#'       \item simple linear model. "lm" class.
#'     }
#'   }
#' }
#'
#' @section Descriptive statistic information:
#' The information derived from the numerical data describe is as follows.
#'
#' \itemize{
#' \item mean : arithmetic average
#' \item sd : standard deviation
#' \item se_mean : standrd error mean. sd/sqrt(n)
#' \item IQR : interqurtle range (Q3-Q1)
#' \item skewness : skewness
#' \item kurtosis : kurtosis
#' \item p25 : Q1. 25\% percentile
#' \item p50 : median. 50\% percentile
#' \item p75 : Q3. 75\% percentile
#' \item p01, p05, p10, p20, p30 : 1\%, 5\%, 20\%, 30\% percentiles
#' \item p40, p60, p70, p80 : 40\%, 60\%, 70\%, 80\% percentiles
#' \item p90, p95, p99, p100 : 90\%, 95\%, 99\%, 100\% percentiles
#' }
#'
#' @param .data a target_df.
#' @param predictor variable of interest. predictor.
#'
#' See vignette("relate") for an introduction to these concepts.
#'
#' @return An object of the class as relate.
#' Attributes of relate class is as follows.
#' \itemize{
#' \item target : name of target variable
#' \item predictor : name of predictor
#' \item model : levels of binned value.
#' \item raw : table_df with two variables target and predictor.
#' }
#' @seealso \code{\link{print.relate}}, \code{\link{plot.relate}}.
#'
#' @examples
#' \donttest{
#' # If the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#' 
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, sodium)
#' cat_num
#' summary(cat_num)
#' 
#' plot(cat_num)
#' 
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, hblood_pressure)
#' cat_cat
#' summary(cat_cat)
#'  
#' plot(cat_cat)
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' num <- target_by(heartfailure, creatinine)
#' 
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, sodium)
#' num_num
#' summary(num_num)
#' 
#' plot(num_num)
#' 
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, smoking)
#' num_cat
#' summary(num_cat)
#' 
#' plot(num_cat)
#' 
#' # Not allow typographic
#' plot(num_cat, typographic = FALSE)
#' }
#' 
#' @export
relate <- function(.data, predictor) {
  UseMethod("relate", .data)
}


#' @rdname relate
#' @method relate target_df
#' @export
relate.target_df <- function(.data, predictor) {
  tryCatch(vars <- tidyselect::vars_select(names(.data), !! rlang::enquo(predictor)),
    error = function(e) {
      pram <- as.character(substitute(predictor))
      stop(sprintf("Column %s is unknown", pram))
    }, finally = NULL)

  relate_impl(.data, vars)
}

#' @importFrom stats cor formula lm xtabs
#' @importFrom tidyselect matches
#' @importFrom methods is
relate_impl <- function(.data, predictor) {
  relate_cat_by_num_impl <- function(.data, predictor) {
    indiv <- describe(.data, predictor)

    total <- describe(ungroup(.data),  predictor)
    total <- total %>%
      select(-matches("statistic")) %>% 
      mutate(levlel = "total") %>%
      select(1, 27, 2:26)
    names(total)[2] <- names(indiv)[2]

    relate <- rbind(indiv, total)
    
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      target <- attr(.data, "groups") %>% names() %>% "["(1)
    } else {
      target <- attr(.data, "vars")
    }  
    
    attr(relate, "target") <- target
    attr(relate, "predictor") <- predictor
    attr(relate, "model") <- "describe"
    attr(relate, "raw") <- .data[, c(target , predictor)]

    class(relate) <- append("relate", class(relate))

    relate
  }

  relate_cat_by_cat_impl <- function(.data, predictor) {
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      target <- attr(.data, "groups") %>% names() %>% "["(1)
    } else {
      target <- attr(.data, "vars")
    }  
    
    data <- .data[, c(target, predictor)]

    formula_str <- sprintf(" ~ %s + %s", target, predictor)

    relate <- xtabs(formula_str, data = data, addNA = TRUE)

    attr(relate, "target") <- target
    attr(relate, "predictor") <- predictor
    attr(relate, "model") <- "crosstable"
    attr(relate, "raw") <- data

    class(relate) <- append("relate", class(relate))

    relate
  }


  relate_num_by_num_impl <- function(.data, predictor) {
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      target <- attr(.data, "groups") %>% names() %>% "["(1)
    } else {
      target <- attr(.data, "vars")
    }  
    
    data <- .data[, c(target, predictor)]

    method <- c("pearson", "spearman")

    correlation <- sapply(method, function(x)
      cor(pull(data, 1), pull(data, 2), method = x))

    formula_str <- sprintf("`%s` ~ `%s`", target, predictor)
    relate <- lm(formula_str, data = data)

    attr(relate, "target") <- target
    attr(relate, "predictor") <- predictor
    attr(relate, "correlation") <- correlation
    attr(relate, "model") <- "linear"
    attr(relate, "raw") <- data

    class(relate) <- append("relate", class(relate))

    relate
  }


  relate_num_by_cat_impl <- function(.data, predictor) {
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      target <- attr(.data, "groups") %>% names() %>% "["(1)
    } else {
      target <- attr(.data, "vars")
    }  
    
    data <- .data[, c(target, predictor)]

    # ordered factor to factor
    tmp <- pull(data, 2)
    class(tmp) <- "factor"
    data[, 2] <- tmp

    formula_str <- sprintf("`%s` ~ `%s`", target, predictor)
    relate <- lm(formula(formula_str), data = data)

    attr(relate, "target") <- target
    attr(relate, "predictor") <- predictor
    attr(relate, "model") <- "ANOVA"
    attr(relate, "raw") <- data

    class(relate) <- append("relate", class(relate))

    relate
  }

  if (utils::packageVersion("dplyr") >= "0.8.0") {
    cname <- attr(.data, "groups") %>% names() %>% "["(1)
    type_y <- .data[, cname] %>% pull %>% is %>% "["(1)
  } else {
    type_y <- is(.data[, attr(.data, "vars")][[1]])[1]
  } 
  
  type_x <- is(.data[, predictor][[1]])[1]

  if (type_y %in% c("ordered", "factor", "character") &&
      type_x %in% c("numeric", "integer")) {
    suppressWarnings(
      relate <- relate_cat_by_num_impl(.data, predictor)
    )
  } else if (type_y %in% c("ordered", "factor", "character") &&
             type_x %in% c("ordered", "factor", "character")) {
    suppressWarnings(
      relate <- relate_cat_by_cat_impl(.data, predictor)
    )  
  } else if (type_y %in% c("numeric", "integer") &&
             type_x %in% c("numeric", "integer")) {
    suppressWarnings(
      relate <- relate_num_by_num_impl(.data, predictor)
    )  
  } else if (type_y %in% c("numeric", "integer") &&
             type_x %in% c("ordered", "factor", "character")) {
    suppressWarnings(
      relate <- relate_num_by_cat_impl(.data, predictor)
    )  
  }

  relate
}


#' Summarizing relate information
#'
#' @description print and summary method for "relate" class.
#' @param x an object of class "relate", usually, a result of a call to relate().
#' @param ... further arguments passed to or from other methods.
#' @details
#' print.relate() tries to be smart about formatting four kinds of relate.
#' summary.relate() tries to be smart about formatting four kinds of relate.
#'
#' @seealso \code{\link{plot.relate}}.
#' @examples
#' \dontrun{
#' # If the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#'
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, hblood_pressure)
#' 
#' # Print bins class object
#' cat_cat
#' 
#' summary(cat_cat)
#' }
#'
#' @examples
#' \donttest{
#' # If the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#' 
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, sodium)
#' cat_num
#' summary(cat_num)
#' 
#' plot(cat_num)
#' 
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, hblood_pressure)
#' cat_cat
#' summary(cat_cat)
#'  
#' plot(cat_cat)
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' num <- target_by(heartfailure, creatinine)
#' 
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, sodium)
#' num_num
#' summary(num_num)
#' 
#' plot(num_num)
#' 
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, smoking)
#' num_cat
#' summary(num_cat)
#' 
#' plot(num_cat)
#' 
#' # Not allow typographic
#' plot(num_cat, typographic = FALSE)
#' }
#' 
#' @method print relate
#' @importFrom stats anova
#' @export
print.relate <- function(x, ...) {
  type <- attr(x, "model")

  if (type == "describe") {
    oldClass(x) <- c("tbl_df", "tbl", "data.frame")
    print(x, ...)
  } else if (type == "crosstable") {
    print.table(x, ...)
  } else if (type == "linear") {
    oldClass(x) <- c("lm")
    print(x, ...)
  } else if (type == "ANOVA") {
    print(anova(x), ...)
  }
}


#' Visualize Information for an "relate" Object
#'
#' @description
#' Visualize four kinds of plot by attribute of relate class.
#'
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "relate", usually, a result of a call to relate().
#' @param model logical. This argument selects whether to output the visualization result
#' to the visualization of the object of the lm model to grasp the relationship between
#' the numerical variables.
#' @param hex_thres an integer. Use only when the target and predictor are numeric variables.
#' Used when the number of observations is large. 
#' Specify the threshold of the observations to draw hexabin plots that are not scatterplots. 
#' The default value is 1000.
#' @param pal Color palette to paint hexabin. Use only when the target and predictor are numeric variables.
#' Applied only when the number of observations is greater than hex_thres.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' only applies when the model argument is TRUE, and is used for ... of the plot.lm() function.
#' @seealso \code{\link{relate}}, \code{\link{print.relate}}.
#' @examples
#' \donttest{
#' # If the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#' 
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, sodium)
#' cat_num
#' summary(cat_num)
#' 
#' plot(cat_num)
#' 
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, hblood_pressure)
#' cat_cat
#' summary(cat_cat)
#'  
#' plot(cat_cat)
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' num <- target_by(heartfailure, creatinine)
#' 
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, sodium)
#' num_num
#' summary(num_num)
#' 
#' plot(num_num)
#' 
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, smoking)
#' num_cat
#' summary(num_cat)
#' 
#' plot(num_cat)
#' 
#' # Not allow typographic
#' plot(num_cat, typographic = FALSE)
#' }
#'   
#' @method plot relate
#' @import ggplot2
#' @import hrbrthemes
#' @importFrom gridExtra grid.arrange
#' @importFrom stats complete.cases var
#' @importFrom graphics plot
#' @export
plot.relate <- function(x, model = FALSE, hex_thres = 1000, 
                        pal = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"), 
                        typographic = TRUE, base_family = NULL, ...) {
  type <- attr(x, "model")
  xvar <- attr(x, "predictor")
  yvar <- attr(x, "target")
  
  if (type == "describe") {
    p_desc <- ggplot(aes(x = !!sym(xvar), color = !!sym(yvar)), data = attr(x, "raw")) +
      geom_density() +
      ggtitle(sprintf("%s's density plot by %s", yvar, xvar)) +
      theme_bw(base_family = base_family) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if (typographic) {
      p_desc <- p_desc +
        theme_typographic(base_family) +
        scale_color_ipsum() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12))
    }
    
    suppressWarnings(p_desc)
  } else if (type == "crosstable") {
    data <- as.data.frame(x) %>% 
      select(a = as.character(xvar), b = yvar, n = Freq) 
    
    first <- data[1, 1] %>% as.character
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
    
    suppressWarnings(p_cross <- data %>% 
      group_by(a) %>% 
      mutate(x_width = sum(n)) %>% 
      ggplot(aes(x = factor(a), y = n)) +
      geom_col(aes(width = x_width, fill = factor(b)),
               color = "white", size = 2, 
               position = position_fill(reverse = FALSE)) +
      facet_grid(~ a, space = "free", scales = "free", switch = "x") +
      scale_x_discrete(name = xvar) +
      scale_y_continuous(name = yvar, breaks = y_pos, labels = y_lab) +
      labs(title = sprintf("%s's mosaics plot by %s", yvar, xvar)) +
      theme_grey(base_family = base_family) +    
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.background = element_blank(),
            panel.spacing = unit(0, "pt")))
    
    if (typographic) {
      p_cross <- p_cross +
        theme_typographic(base_family) +
        scale_fill_ipsum(na.value = "grey80") +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              panel.spacing = unit(0, "pt"))
    }
    
    suppressWarnings(p_cross)
  } else if (type == "linear") {
    # for Exception handling in plot.relate() #76
    # Code of MASS::bandwidth.nrd
    bandwidth.nrd <- function (x) {
      r <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
      h <- (r[2L] - r[1L])/1.34
      4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1/5)
    }
    
    if (model) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      
      oldClass(x) <- c("lm")
      
      par(mfrow = c(2, 2))
      plot(x, which = 1, ...)
      plot(x, which = 2, ...)
      plot(x, which = 3, ...)
      plot(x, which = 5, ...)
    } else {
      # for Exception handling in plot.relate() #76
      bandwidth_x_zero <- bandwidth.nrd(attr(x, "raw")[[xvar]]) == 0
      bandwidth_y_zero <- bandwidth.nrd(attr(x, "raw")[[yvar]]) == 0
      
      fig1 <- ggplot(aes(x = !!sym(yvar), y = !!sym(xvar)), data = attr(x, "raw")) 
      if (NROW(attr(x, "raw")) >= hex_thres) {
        fig1 <- fig1 + 
          geom_hex(color = "grey") +
          scale_fill_gradientn(colours = pal)
        
        # for Exception handling in plot.relate() #76
        if (!bandwidth_x_zero & !bandwidth_y_zero) {
          fig1 <- fig1 + 
            geom_density2d(colour = "black")
        }
      } else {
        fig1 <- fig1 + geom_point(alpha = 1/5)
      }
      
      fig1 <- fig1 +   
        stat_smooth(method = lm, formula = 'y ~ x') +
        theme_bw(base_family = base_family) +
        labs(title = sprintf("%s by %s", yvar, xvar)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      if (typographic) {
        fig1 <- fig1 +
          theme_typographic(base_family) +
          theme(axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12))
      }
      
      idx <- complete.cases(attr(x, "raw"))
      Predicted <- x$fitted.values
      Observed <- unlist(attr(x, "raw")[idx, 1])
      
      df <- data.frame(Predicted, Observed)
      
      min_x <- min(c(df$Predicted, df$Observed))
      max_x <- max(c(df$Predicted, df$Observed))
      
      fig2 <- ggplot(aes(x = Observed, y = Predicted), data = df) 
      if (NROW(attr(x, "raw")) >= hex_thres) {
        fig2 <- fig2 + 
          geom_hex(color = "grey") +
          scale_fill_gradientn(colours = pal)
  
        # for Exception handling in plot.relate() #76
        if (!bandwidth_x_zero & !bandwidth_y_zero) {
          fig2 <- fig2 + 
            geom_density2d(colour = "black")
        }              
      } else {
        fig2 <- fig2 + geom_point(alpha = 1/5)
      }
      fig2 <- fig2 + 
        xlim(min_x, max_x) +
        ylim(min_x, max_x) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
        theme_bw(base_family = base_family) +
        labs(title = "Predicted vs Observed",
             x = sprintf("Observed (%s)", yvar),
             y = sprintf("Predicted (%s)", yvar))
      theme(plot.title = element_text(hjust = 0.5))
      
      if (typographic) {
        fig2 <- fig2 +
          theme_typographic(base_family) +
          theme(axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12))
      }
      
      suppressWarnings(gridExtra::grid.arrange(fig1, fig2, ncol = 2))
    }
  } else if (type == "ANOVA") {
    if (model) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      
      oldClass(x) <- c("lm")
      
      par(mfrow = c(2, 2))
      plot(x, which = 1, ...)
      plot(x, which = 2, ...)
      plot(x, which = 3, ...)
      plot(x, which = 5, ...)
    } else {
      p_box <- ggplot(aes(x = !!sym(xvar), y = !!sym(yvar), fill = !!sym(xvar)), 
                      data = attr(x, "raw")) +
        geom_boxplot(alpha = 0.7) +
        ggtitle(sprintf("%s's box plot by %s", yvar, xvar)) +
        theme_bw(base_family = base_family) +
        theme(plot.title = element_text(hjust = 0.5))
      
      if (typographic) {
        p_box <- p_box +
          theme_typographic(base_family) +
          scale_fill_ipsum() +
          theme(axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12))
      }
      
      suppressWarnings(p_box)
    }
  }
}

