#' @rdname normality.data.frame
#' @export
normality <- function(.data, ...) {
  UseMethod("normality", .data)
}


#' @rdname plot_normality.data.frame
#' @export
plot_normality <- function(.data, ...) {
  UseMethod("plot_normality", .data)
}


#' Performs the Shapiro-Wilk test of normality
#'
#' @description The normality() performs Shapiro-Wilk test of normality of numerical values.
#'
#' @details This function is useful when used with the \code{\link{group_by}}
#' function of the dplyr package. If you want to test by level of the categorical
#' data you are interested in, rather than the whole observation,
#' you can use group_tf as the group_by function.
#' This function is computed \code{\link{shapiro.test}} function.
#'
#' @section Normality test information:
#' The information derived from the numerical data test is as follows.
#'
#' \itemize{
#' \item statistic : the value of the Shapiro-Wilk statistic.
#' \item p_value : an approximate p-value for the test. This is said in
#' Roystion(1995) to be adequate for p_value < 0.1.
#' \item sample : the number of samples to perform the test.
#' The number of observations supported by the stats::shapiro.test function is 3 to 5000.
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, normality() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param sample the number of samples to perform the test.
#'
#' See vignette("EDA") for an introduction to these concepts.
#'
#' @return An object of the same class as .data.
#' @seealso \code{\link{normality.tbl_dbi}}, \code{\link{diagnose_numeric.data.frame}}, \code{\link{describe.data.frame}}, \code{\link{plot_normality.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Normality test of numerical variables
#' normality(heartfailure)
#' 
#' # Select the variable to describe
#' normality(heartfailure, platelets, sodium)
#' normality(heartfailure, -platelets, -sodium)
#' normality(heartfailure, 1)
#' normality(heartfailure, platelets, sodium, sample = 200)
#' 
#' # death_eventing dplyr::grouped_dt
#' library(dplyr)
#' 
#' gdata <- group_by(heartfailure, smoking, death_event)
#' normality(gdata, "platelets")
#' normality(gdata, sample = 250)
#' 
#' # death_eventing pipes ---------------------------------
#' # Normality test of all numerical variables
#' heartfailure %>%
#'   normality()
#' 
#' # Positive values select variables
#' heartfailure %>%
#'   normality(platelets, sodium)
#' 
#' # Positions values select variables
#' heartfailure %>%
#'   normality(1)
#' 
#' # death_eventing pipes & dplyr -------------------------
#' # Test all numerical variables by 'smoking' and 'death_event',
#' # and extract only those with 'smoking' variable level is "No".
#' heartfailure %>%
#'   group_by(smoking, death_event) %>%
#'   normality() %>%
#'   filter(smoking == "No")
#' 
#' # extract only those with 'sex' variable level is "Male",
#' # and test 'platelets' by 'smoking' and 'death_event'
#' heartfailure %>%
#'   filter(sex == "Male") %>%
#'   group_by(smoking, death_event) %>%
#'   normality(platelets)
#' 
#' # Test log(platelets) variables by 'smoking' and 'death_event',
#' # and extract only p.value greater than 0.01.
#' heartfailure %>%
#'   mutate(platelets_income = log(platelets)) %>%
#'   group_by(smoking, death_event) %>%
#'   normality(platelets_income) %>%
#'   filter(p_value > 0.01)
#' }
#' 
#' @method normality data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
normality.data.frame <- function(.data, ..., sample = 5000) {
  sample <- min(5000, nrow(.data), sample)
  .data <- sample_n(.data, sample)
  
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  normality_impl(.data, vars, sample)
}

#' @importFrom stats shapiro.test
normality_impl <- function(df, vars, sample) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  num_normal <- function(x) {
    x <- x[which(!is.infinite(x))]
    
    result <- shapiro.test(x)
    
    tibble(statistic = result$statistic, p_value = result$p.value)
  }
  
  statistic <- lapply(vars[idx_numeric], function(x) num_normal(pull(df, x)))
  
  tibble(vars = vars[idx_numeric], statistic, sample = sample) %>%
    tidyr::unnest(cols = c(statistic)) %>%
    select(vars, statistic, p_value, sample)
}


#' @method normality grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
normality.grouped_df <- function(.data, ..., sample = 5000) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  normality_group_impl(.data, vars, sample)
}

#' @importFrom stats shapiro.test
normality_group_impl <- function(df, vars, sample) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  if (utils::packageVersion("dplyr") >= "0.8.0") flag <- 0
  else flag <- 1
  
  num_normal <- function(x, .data, vars, n_sample) {
    nums <- .data[x + flag, vars][[1]]
    
    n_sample <- min(length(nums), n_sample)
    
    x <- sample(nums, n_sample)
    x <- x[which(!is.infinite(x))]
    
    tryCatch(result <- shapiro.test(x),
             error = function(e) NULL,
             finally = NULL)
    
    if (length(ls(pattern = "result")) == 0) {
      tibble(statistic = NA, p_value = NA, sample = n_sample)
    } else {
      tibble(statistic = result$statistic, p_value = result$p.value, 
             sample = n_sample)
    }        
  }
  
  call_normal <- function(vars) {
    #idx <- which(sapply(attr(df, "indices"), length) >= 3)
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      statistic <- purrr::map_df(attr(df, "groups") %>% 
                                   select(tidyselect::matches("\\.rows")) %>% 
                                   pull, num_normal, df, vars, sample)
      
      glables <- attr(df, "groups") %>% 
        select(-tidyselect::matches("\\.rows"))
    } else {
      statistic <- purrr::map_df(attr(df, "indices"),
                                 num_normal, df, vars, sample)
      
      glables <- attr(df, "labels")
    }  
    
    dplyr::bind_cols(tibble(variable = rep(vars, nrow(statistic))),
                     as_tibble(glables), statistic)
  }
  
  statistic <- lapply(vars[idx_numeric], function(x) call_normal(x))
  
  tibble(statistic) %>%
    tidyr::unnest(cols = c(statistic))
}


#' Plot distribution information of numerical data
#'
#' @description The plot_normality() visualize distribution information
#' for normality test of the numerical data.
#'
#' @details The scope of the visualization is the provide a distribution information.
#' Since the plot is drawn for each variable, if you specify more than
#' one variable in the ... argument, the specified number of plots are drawn.
#'
#' The argument values that left and right can have are as follows.:
#' 
#' \itemize{
#'   \item "log" : log transformation. log(x)
#'   \item "log+1" : log transformation. log(x + 1). Used for values that contain 0.
#'   \item "log+a" : log transformation. log(x + 1 - min(x)). Used for values that contain 0.   
#'   \item "sqrt" : square root transformation.
#'   \item "1/x" : 1 / x transformation
#'   \item "x^2" : x square transformation
#'   \item "x^3" : x^3 square transformation
#'   \item "Box-Cox" : Box-Box transformation
#'   \item "Yeo-Johnson" : Yeo-Johnson transformation
#' }
#' 
#' @section Distribution information:
#' The plot derived from the numerical data visualization is as follows.
#'
#' \itemize{
#'   \item histogram by original data
#'   \item q-q plot by original data
#'   \item histogram by log transfer data
#'   \item histogram by square root transfer data
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_normality() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' 
#' See vignette("EDA") for an introduction to these concepts.
#' @param left character. Specifies the data transformation method to draw the histogram in the 
#' lower left corner. The default is "log".
#' @param right character. Specifies the data transformation method to draw the histogram in the 
#' lower right corner. The default is "sqrt".
#' @param col a color to be used to fill the bars. The default is "steelblue".
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' 
#' @seealso \code{\link{plot_normality.tbl_dbi}}, \code{\link{plot_outlier.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Visualization of all numerical variables
#' heartfailure2 <- heartfailure[, c("creatinine", "platelets", "sodium", "sex", "smoking")]
#' plot_normality(heartfailure2)
#'
#' # Select the variable to plot
#' plot_normality(heartfailure2, platelets, sodium)
#' plot_normality(heartfailure2, -platelets, -sodium, col = "gray")
#' plot_normality(heartfailure2, 1)
#'
#' # Change the method of transformation
#' plot_normality(heartfailure2, platelets, right = "1/x")
#' 
#' if (requireNamespace("forecast", quietly = TRUE)) {
#'   plot_normality(heartfailure2, platelets, left = "Box-Cox", right = "Yeo-Johnson")
#' } else {
#'   cat("If you want to use this feature, you need to install the rpart package.\n")
#' }
#' # Not allow typographic elements
#' plot_normality(heartfailure2, platelets, typographic = FALSE)
#' 
#' # Using dplyr::grouped_df
#' library(dplyr)
#'
#' gdata <- group_by(heartfailure2, sex, smoking)
#' plot_normality(gdata)
#' plot_normality(gdata, "creatinine")
#'
#' # Using pipes ---------------------------------
#' # Visualization of all numerical variables
#' heartfailure2 %>%
#'  plot_normality()
#'
#' # Positive values select variables
#' heartfailure2 %>%
#' plot_normality(platelets, sodium)
#'
#' # Positions values select variables
#' # heartfailure2 %>%
#' #  plot_normality(1)
#'
#' # Using pipes & dplyr -------------------------
#' # Plot 'creatinine' variable by 'sex' and 'smoking'
#' heartfailure2 %>%
#'  group_by(sex, smoking) %>%
#'  plot_normality(creatinine)
#'
#' # extract only those with 'sex' variable level is "Male",
#' # and plot 'platelets' by 'smoking'
#' if (requireNamespace("forecast", quietly = TRUE)) {
#'   heartfailure2 %>%
#'    filter(sex == "Male") %>%
#'    group_by(smoking) %>%
#'    plot_normality(platelets, right = "Box-Cox")
#' } else {
#'   cat("If you want to use this feature, you need to install the rpart package.\n")
#' }
#' }
#' @method plot_normality data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
plot_normality.data.frame <- function(.data, ..., left = c("log", "sqrt", "log+1", "log+a", "1/x", "x^2", 
                                                           "x^3", "Box-Cox", "Yeo-Johnson"),
                                      right = c("sqrt", "log", "log+1", "log+a", "1/x", "x^2", 
                                                "x^3", "Box-Cox", "Yeo-Johnson"),
                                      col = "steelblue", typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  left <- match.arg(left)
  right <- match.arg(right)
  
  plot_normality_impl(.data, vars, left, right, col, typographic)
}

#' @importFrom stats qqline qqnorm
plot_normality_impl <- function(df, vars, left, right, col = "steelblue", typographic = TRUE) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  plot_normality <- function(df, var, left, right, col = "steelblue", typographic = TRUE) {
    x <- pull(df, var)
    
    main <- sprintf("Normality Diagnosis Plot (%s)", var) 
    
    plot_normality_raw(x, left, right, main, col, typographic)
  }
  
  invisible(lapply(vars[idx_numeric], function(x) 
    plot_normality(df, x, left, right, col, typographic)))
}


#' @import dplyr
#' @import ggplot2
#' @import hrbrthemes
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
plot_normality_raw <- function(x, left = c("log", "sqrt", "log+1", "log+a", "1/x", "x^2", 
                                           "x^3", "Box-Cox", "Yeo-Johnson"),
                               right = c("sqrt", "log", "log+1", "log+a", "1/x", "x^2", 
                                         "x^3", "Box-Cox", "Yeo-Johnson"),
                               main = NULL, col = "steelblue", typographic = TRUE) {
  left <- match.arg(left)
  right <- match.arg(right)
  
  main <- ifelse(is.null(main), "Normality Diagnose Plot", main)
  
  df <- data.frame(x = x) %>% 
    filter(!is.na(x))
  
  # calulate number of bins using Sturges' formula
  n_bins <- round(log2(nrow(df)) + 1)
  
  null_theme <- theme(
    axis.title = element_text(color = "transparent"),
    axis.text = element_text(color = "transparent"), 
    axis.ticks = element_line(color = "transparent"),
    panel.grid = element_line(color = "transparent"),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
  
  top_left <- df %>% 
    ggplot(aes(x)) +
    geom_histogram(fill = col, color = "black", alpha = 0.8, bins = n_bins) +
    labs(title = "origin", x = "", y = "")
  
  top_right <- df %>% 
    filter(!is.infinite(x)) %>% 
    ggplot(aes(sample = x, group = 1)) +
    stat_qq(color = col) + 
    stat_qq_line() +
    labs(title = "origin: Q-Q plot", x = "", y = "")
  
  suppressWarnings(df_left <- df %>% 
                     mutate(x = get_transform(x, left)))
  
  non_finite_left <- FALSE
  if (sum(is.finite(df_left$x)) == 0)
    non_finite_left <- TRUE
  
  if (non_finite_left) {
    bottom_left <- data.frame(x = 1, y = 1, msg = "All transfomed data is not finite") %>% 
      ggplot(aes(x = x, y = y, label = msg)) +
      geom_text() +
      labs(title = paste(left, "transformation"), x = "", y = "") +
      null_theme
  } else {
    bottom_left <- df_left %>% 
      ggplot(aes(x)) +
      geom_histogram(fill = col, color = "black", alpha = 0.8, bins = n_bins) +
      labs(title = paste(left, "transformation"), x = "", y = "")
  }
  
  suppressWarnings(df_right <- df %>% 
                     mutate(x = get_transform(x, right)))
  
  non_finite_right <- FALSE
  if (sum(is.finite(df_right$x)) == 0)
    non_finite_right <- TRUE
  
  if (non_finite_right) {
    bottom_right <- data.frame(x = 1, y = 1, msg = "All transfomed data is not finite") %>% 
      ggplot(aes(x = x, y = y, label = msg)) +
      geom_text() +
      labs(title = paste(left, "transformation"), x = "", y = "") +
      null_theme
  } else {
    bottom_right <- df_right %>% 
      ggplot(aes(x)) +
      geom_histogram(fill = col, color = "black", alpha = 0.8, bins = n_bins) +
      labs(title = paste(right, "transformation"), x = "", y = "")
  }
  
  if (typographic) {
    top_left <- top_left +
      theme_typographic() +
      theme(plot.title = element_text(size = 15, face = "plain"),
            plot.margin = margin(20, 30, 10, 30))
    
    top_right <- top_right +
      theme_typographic() +
      theme(plot.title = element_text(size = 15, face = "plain"),
            plot.margin = margin(20, 30, 10, 30))
    
    if (non_finite_left) {
      bottom_left <- bottom_left +
        theme_typographic() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(size = 15, face = "plain"),
              plot.margin = margin(10, 30, 20, 30)) +
        null_theme
    } else {
      bottom_left <- bottom_left +
        theme_typographic() +
        theme(plot.title = element_text(size = 15, face = "plain"),
              plot.margin = margin(10, 30, 20, 30))
    }
    
    if (non_finite_right) {
      bottom_right <- bottom_right +
        theme_typographic() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(size = 15, face = "plain"),
              plot.margin = margin(10, 30, 20, 30)) +
        null_theme
    } else {
      bottom_right <- bottom_right +
        theme_typographic() +
        theme(plot.title = element_text(size = 15, face = "plain"),
              plot.margin = margin(10, 30, 20, 30))
    }
    
    fontfamily <- get_font_family()
    
    top <- grid::textGrob(main, gp = grid::gpar(fontfamily = fontfamily, 
                                                fontsize = 18, font = 2),
                          x = unit(0.075, "npc"), just = "left")
    
  } else {
    top <- main
  }
  
  suppressWarnings(gridExtra::grid.arrange(top_left, top_right, bottom_left, bottom_right, 
                                           ncol = 2, nrow = 2, top = top))
}

#' @method plot_normality grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
plot_normality.grouped_df <- function(.data, ..., left = c("log", "sqrt", "log+1", "log+a", "1/x", "x^2", 
                                                           "x^3", "Box-Cox", "Yeo-Johnson"),
                                      right = c("sqrt", "log", "log+1", "log+a", "1/x", "x^2", 
                                                "x^3", "Box-Cox", "Yeo-Johnson"),
                                      col = "steelblue", typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  left <- match.arg(left)
  right <- match.arg(right)
  
  plot_normality_group_impl(.data, vars, left, right, col, typographic)
}


#' @import dplyr
#' @importFrom utils packageVersion
#' @importFrom tidyselect matches
#' @importFrom tibble is_tibble as_tibble
plot_normality_group_impl <- function(df, vars, left, right, col = "steelblue", typographic = TRUE) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- tibble::as_tibble(df)
  
  idx_numeric <- find_class(df[, vars], type = "numerical")
  
  call_plot <- function(var, left, right, col = "steelblue", typographic = TRUE) {
    plot_normality <- function(pos, df, var, col = "steelblue", typographic = TRUE) {
      if (utils::packageVersion("dplyr") >= "0.8.0") {
        x <- unlist(df[(attr(df, "groups") %>% 
                          select(tidyselect::matches("\\.rows")) %>% 
                          pull)[[pos]], var])
        
        label <- attr(df, "groups") %>% select(-tidyselect::matches("\\.rows"))
      } else {
        x <- unlist(df[attr(df, "indices")[[pos]] + 1, var])
        
        label <- attr(df, "labels")
      }  
      
      label <- paste(names(label), "==", unlist(label[pos, ]), collapse = ",")
      
      main <- sprintf("Normality Diagnosis Plot\n(%s by %s)", var, label)
      plot_normality_raw(x, left, right, main, col, typographic)
    }
    
    if (utils::packageVersion("dplyr") >= "0.8.0") {
      cnt <- nrow(attr(df, "groups")) 
    } else {
      cnt <- nrow(attr(df, "labels"))
    } 
    
    lapply(seq(cnt), plot_normality, df, var, col, typographic)
  }
  
  invisible(lapply(vars[idx_numeric], function(x) 
    call_plot(x, left, right, col, typographic)))
}


#' Transform a numeric vector
#'
#' @description The get_transform() gets transformation of numeric variable.
#'
#' @param x numeric. numeric for transform
#' @param method character. transformation method of numeric variable
#' @return numeric. transformed numeric vector.
#' 
#' @details The supported transformation method is follow.: 
#' \itemize{
#'   \item "log" : log transformation. log(x)
#'   \item "log+1" : log transformation. log(x + 1). Used for values that contain 0.
#'   \item "log+a" : log transformation. log(x + 1 - min(x)). Used for values that contain 0.   
#'   \item "sqrt" : square root transformation.
#'   \item "1/x" : 1 / x transformation
#'   \item "x^2" : x square transformation
#'   \item "x^3" : x^3 square transformation
#'   \item "Box-Cox" : Box-Box transformation
#'   \item "Yeo-Johnson" : Yeo-Johnson transformation
#' }
#' 
#' @seealso \code{\link{plot_normality}}.
#' @export
#' @examples
#' \dontrun{
#' # log+a transform 
#' get_transform(iris$Sepal.Length, "log+a")
#'
#' if (requireNamespace("forecast", quietly = TRUE)) {
#'   # Box-Cox transform 
#'   get_transform(iris$Sepal.Length, "Box-Cox")
#' 
#'   # Yeo-Johnson transform 
#'   get_transform(iris$Sepal.Length, "Yeo-Johnson")
#' } else {
#'   cat("If you want to use this feature, you need to install the forecast package.\n")
#' }
#' }
#' 
get_transform <- function(x, method = c("log", "sqrt", "log+1", "log+a", "1/x", 
                                        "x^2", "x^3", "Box-Cox", "Yeo-Johnson")) {
  get_boxcox <- function(x) {
    forecast::BoxCox(x, lambda = "auto")
  }  
  
  get_yjohnson <- function(x) {
    lambda <- forecast::BoxCox.lambda(x)
    lambda <- rep(lambda, length(x))
    
    ifelse(x >= 0, ifelse(lambda != 0, ((x + 1) ^ lambda - 1) / lambda, log(x + 1)),
           ifelse(lambda != 2, -((-1 * x + 1) ^ (2 - lambda) - 1) / (2 - lambda),
                  -1 * log(x + 1)))
  } 
  
  if (method == "log")
    result <- log(x)
  else if (method == "log+1")
    result <- log(x + 1)
  else if (method == "log+a")
    result <- log(x + 1 - min(x, na.rm = TRUE))  
  else if (method == "sqrt")
    result <- sqrt(x)
  else if (method == "1/x")
    result <- 1/x
  else if (method == "x^2")
    result <- x^2
  else if (method == "x^3")
    result <- x^3
  else if (method == "Box-Cox") {
    if (!requireNamespace("forecast", quietly = TRUE)) {
      stop("Package \"forecast\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    result <- get_boxcox(x) 
  }
  else if (method == "Yeo-Johnson") {
    if (!requireNamespace("forecast", quietly = TRUE)) {
      stop("Package \"forecast\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    result <- get_yjohnson(x)
  }
  
  result
}
