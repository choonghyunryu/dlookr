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
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Normality test of numerical variables
#' normality(carseats)
#'
#' # Select the variable to describe
#' normality(carseats, Sales, Price)
#' normality(carseats, -Sales, -Price)
#' normality(carseats, 1)
#' normality(carseats, Sales, Price, sample = 300)
#'
#' # Using dplyr::grouped_dt
#' library(dplyr)
#'
#' gdata <- group_by(carseats, ShelveLoc, US)
#' normality(gdata, "Sales")
#' normality(gdata, sample = 250)
#'
#' # Using pipes ---------------------------------
#' # Normality test of all numerical variables
#' carseats %>%
#'  normality()
#'
#' # Positive values select variables
#' carseats %>%
#'  normality(Sales, Price)
#'
#' # Positions values select variables
#' carseats %>%
#'  normality(1)
#'
#' # Using pipes & dplyr -------------------------
#' # Test all numerical variables by 'ShelveLoc' and 'US',
#' # and extract only those with 'ShelveLoc' variable level is "Good".
#' carseats %>%
#'  group_by(ShelveLoc, US) %>%
#'  normality() %>%
#'  filter(ShelveLoc == "Good")
#'
#' # extract only those with 'Urban' variable level is "Yes",
#' # and test 'Sales' by 'ShelveLoc' and 'US'
#' carseats %>%
#'  filter(Urban == "Yes") %>%
#'  group_by(ShelveLoc, US) %>%
#'  normality(Sales)
#'
#' # Test log(Income) variables by 'ShelveLoc' and 'US',
#' # and extract only p.value greater than 0.01.
#' carseats %>%
#'  mutate(log_income = log(Income)) %>%
#'  group_by(ShelveLoc, US) %>%
#'  normality(log_income) %>%
#'  filter(p_value > 0.01)
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
#' @section Distribution information:
#' The plot derived from the numerical data visualization is as follows.
#'
#' \itemize{
#' \item histogram by original data
#' \item q-q plot by original data
#' \item histogram by log transfer data
#' \item histogram by square root transfer data
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
#'
#' @seealso \code{\link{plot_normality.tbl_dbi}}, \code{\link{plot_outlier.data.frame}}.
#' @export
#' @examples
#' \donttest{
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Visualization of all numerical variables
#' plot_normality(carseats)
#'
#' # Select the variable to plot
#' plot_normality(carseats, Income, Price)
#' plot_normality(carseats, -Income, -Price)
#' plot_normality(carseats, 1)
#'
#' # Using dplyr::grouped_df
#' library(dplyr)
#'
#' gdata <- group_by(carseats, ShelveLoc, US)
#' plot_normality(carseats)
#' plot_normality(carseats, "Sales")
#'
#' # Using pipes ---------------------------------
#' # Visualization of all numerical variables
#' carseats %>%
#'  plot_normality()
#'
#' # Positive values select variables
#' carseats %>%
#' plot_normality(Income, Price)
#'
#' # Positions values select variables
#' carseats %>%
#'  plot_normality(1)
#'
#' # Using pipes & dplyr -------------------------
#' # Plot 'Sales' variable by 'ShelveLoc' and 'US'
#' carseats %>%
#'  group_by(ShelveLoc, US) %>%
#'  plot_normality(Sales)
#'
#' # extract only those with 'ShelveLoc' variable level is "Good",
#' # and plot 'Income' by 'US'
#' carseats %>%
#'  filter(ShelveLoc == "Good") %>%
#'  group_by(US) %>%
#'  plot_normality(Income)
#' }
#' @method plot_normality data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
plot_normality.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  plot_normality_impl(.data, vars)
}

#' @importFrom stats qqline qqnorm
plot_normality_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  plot_normality <- function(df, var) {
    x <- pull(df, var)

    op <- par(no.readonly = TRUE)
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(2, 4, 2, 2))
    on.exit(par(op))

    hist(x, col = "lightblue", las = 1, main = "origin")
    
    x2 <- x[which(!is.infinite(x))]
    
    qqnorm(x2, main = "origin: Q-Q plot")
    qqline(x2)

    hist(log(x), col = "lightblue", las = 1, main = "log")
    hist(sqrt(x), col = "lightblue", las = 1, main = "sqrt")

    title(sprintf("Normality Diagnosis Plot (%s)", var), outer = TRUE)
  }

  tmp <- lapply(vars[idx_numeric], function(x) plot_normality(df, x))
}


#' @method plot_normality grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
plot_normality.grouped_df <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  plot_normality_group_impl(.data, vars)
}

#' @importFrom stats qqline qqnorm
#' @importFrom graphics text
plot_normality_group_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)

  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)

  idx_numeric <- find_class(df[, vars], type = "numerical")

  call_plot <- function(var) {
    plot_normality <- function(pos, df, var) {
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
      
      op <- par(no.readonly = TRUE)
      par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(2, 4, 2, 2))
      on.exit(par(op))

      hist(x, col = "lightblue", las = 1, main = "origin")
      qqnorm(x, main = "origin: Q-Q plot")
      qqline(x)

      finite_cnt <- sum(is.finite(log(x)))
      
      if (finite_cnt == 0) {
        plot(0, axes = FALSE, type = "n", xlab = "", ylab ="")
        text(1, 0, "All log transfomed data is infinite", cex = 1.1)
      } else {
        hist(log(x), col = "lightblue", las = 1, main = "log")
      }
      
      hist(sqrt(x), col = "lightblue", las = 1, main = "sqrt")

      title(sprintf("Normality Diagnosis Plot\n(%s by %s)", var, label),
            outer = TRUE)
    }

    if (utils::packageVersion("dplyr") >= "0.8.0") {
      cnt <- nrow(attr(df, "groups")) 
    } else {
      cnt <- nrow(attr(df, "labels"))
    } 
    
    lapply(seq(cnt), plot_normality, df, var)
  }

  tmp <- lapply(vars[idx_numeric], function(x) call_plot(x))
}
