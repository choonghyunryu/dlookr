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
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # If the target variable is a categorical variable
#' categ <- target_by(carseats, US)
#'
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, Sales)
#' cat_num
#' summary(cat_num)
#' plot(cat_num)
#'
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, ShelveLoc)
#' cat_cat
#' summary(cat_cat)
#' plot(cat_cat)
#'
#' ##---------------------------------------------------
#' # If the target variable is a categorical variable
#' num <- target_by(carseats, Sales)
#'
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, Price)
#' num_num
#' summary(num_num)
#' plot(num_num)
#'
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, ShelveLoc)
#' num_cat
#' summary(num_cat)
#' plot(num_cat)
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
#' @param .data A target_df.
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
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # If the target variable is a categorical variable
#' categ <- target_by(carseats, US)
#'
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, Sales)
#' cat_num
#' summary(cat_num)
#' plot(cat_num)
#'
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, ShelveLoc)
#' cat_cat
#' summary(cat_cat)
#' plot(cat_cat)
#'
#' ##---------------------------------------------------
#' # If the target variable is a categorical variable
#' num <- target_by(carseats, Sales)
#'
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, Price)
#' num_num
#' summary(num_num)
#' plot(num_num)
#'
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, ShelveLoc)
#' num_cat
#' summary(num_cat)
#' plot(num_cat)
#' @export
relate <- function(.data, predictor) {
  UseMethod("relate", .data)
}


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

#' @importFrom methods is
#' @importFrom stats cor formula lm xtabs
relate_impl <- function(.data, predictor) {
  relate_cat_by_num_impl <- function(.data, predictor) {
    indiv <- describe(.data, predictor)

    total <- describe(ungroup(.data),  predictor)
    total <- total %>%
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

    formula_str <- sprintf("%s ~ %s", target, predictor)
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

    formula_str <- sprintf("%s ~ %s", target, predictor)
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
#' # Generate data for the example
#' diamonds2 <- diamonds
#' diamonds2[sample(seq(NROW(diamonds2)), 250), "price"] <- NA
#' diamonds2[sample(seq(NROW(diamonds2)), 20), "clarity"] <- NA
#'
#' # Binning the carat variable. default type argument is "quantile"
#' bin <- binning(diamonds2$carat)
#'
#' # Print bins class object
#' bin
#'
#' # Summarize bins class object
#' summary(bin)
#' }
#'
#'
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # If the target variable is a categorical variable
#' categ <- target_by(carseats, US)
#'
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, Sales)
#' cat_num
#' summary(cat_num)
#' plot(cat_num)
#'
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, ShelveLoc)
#' cat_cat
#' summary(cat_cat)
#' plot(cat_cat)
#'
#' ##---------------------------------------------------
#' # If the target variable is a categorical variable
#' num <- target_by(carseats, Sales)
#'
#' # If the variable of interest is a numerical variable
#' num_num <- relate(num, Price)
#' num_num
#' summary(num_num)
#' plot(num_num)
#'
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, ShelveLoc)
#' num_cat
#' summary(num_cat)
#' plot(num_cat)
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
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' only applies when the model argument is TRUE, and is used for ... of the plot.lm() function.
#' @seealso \code{\link{relate}}, \code{\link{print.relate}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # If the target variable is a categorical variable
#' categ <- target_by(carseats, US)
#'
#' # If the variable of interest is a numerical variable
#' cat_num <- relate(categ, Sales)
#' cat_num
#' summary(cat_num)
#' plot(cat_num)
#'
#' # If the variable of interest is a categorical variable
#' cat_cat <- relate(categ, ShelveLoc)
#' cat_cat
#' summary(cat_cat)
#' plot(cat_cat)
#'
#' ##---------------------------------------------------
#' # If the target variable is a categorical variable
#' num <- target_by(carseats, Sales)
#'
#' # If the variable of interest is a numarical variable
#' num_num <- relate(num, Price)
#' num_num
#' summary(num_num)
#' plot(num_num)
#' plot(num_num, hex_thres = 400)
#'
#' # If the variable of interest is a categorical variable
#' num_cat <- relate(num, ShelveLoc)
#' num_cat
#' summary(num_cat)
#' plot(num_cat)
#' @method plot relate
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' @importFrom gridExtra grid.arrange
#' @importFrom stats complete.cases
#' @importFrom graphics plot
#' @export
plot.relate <- function(x, model = FALSE, 
  hex_thres = 1000, pal = RColorBrewer::brewer.pal(7,"YlOrRd"), ...) {
  type <- attr(x, "model")
  xvar <- attr(x, "predictor")
  yvar <- attr(x, "target")

  if (type == "describe") {
    ggplot(aes_string(x = xvar, color = yvar), data = attr(x, "raw")) +
      geom_density() +
      ggtitle(sprintf("%s's density plot by %s", yvar, xvar)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  } else if (type == "crosstable") {
    oldClass(x) <- c("xtabs", "table")

    plot(t(x), col = RColorBrewer::brewer.pal(8, "Dark2"),
      main = sprintf("%s's mosaics plot by %s", yvar, xvar))
  } else if (type == "linear") {
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
      fig1 <- ggplot(aes_string(x = xvar, y = yvar), data = attr(x, "raw")) 
      if (NROW(attr(x, "raw")) >= hex_thres) {
        fig1 <- fig1 + 
          geom_hex(color = "grey") +
          scale_fill_gradientn(colours = pal) + 
          geom_density2d(colour = "black")
      } else {
        fig1 <- fig1 + geom_point(alpha = 1/10)
      }
      
      fig1 <- fig1 +   
        stat_smooth(method = lm) +
        ggtitle(sprintf("%s's scatter plot by %s", yvar, xvar)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

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
          scale_fill_gradientn(colours = pal) + 
          geom_density2d(colour = "black")
      } else {
        fig2 <- fig2 + geom_point(alpha = 1/10)
      }
      fig2 <- fig2 + 
        xlim(min_x, max_x) +
        ylim(min_x, max_x) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
        ggtitle(sprintf("Predicted vs Observed (%s)", yvar)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

      gridExtra::grid.arrange(fig1, fig2, ncol = 2)
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
      ggplot(aes_string(x = xvar, y = yvar, fill = xvar), data = attr(x, "raw")) +
        geom_boxplot() +
        ggtitle(sprintf("%s's box plot by %s", yvar, xvar)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  }
}

