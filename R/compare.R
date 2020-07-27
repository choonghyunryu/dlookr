#' @rdname compare_category.data.frame
#' @export
compare_category <- function(.data, ...) {
  UseMethod("compare_category", .data)
}


#' @rdname compare_numeric.data.frame
#' @export
compare_numeric <- function(.data, ...) {
  UseMethod("compare_numeric", .data)
}


#' Compare categorical variables
#'
#' @description The compare_category() compute information to examine the relationship 
#' between categorical variables.
#'
#' @details 
#' It is important to understand the relationship between categorical variables in EDA.
#' compare_category() compares relations by pair combination of all categorical variables. 
#' and return compare_category class that based list object.
#'
#' @return An object of the class as compare based list.
#' The information to examine the relationship between categorical variables is as follows each components.
#'
#' \itemize{
#' \item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
#' \item var2 : factor. The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
#' \item n : integer. frequency by var1 and var2.
#' \item rate : double. relative frequency.
#' \item first_rate : double. relative frequency in first variable.
#' \item second_rate : double. relative frequency in second variable.
#' }
#'
#' @section Attributes of return object:
#' Attributes of compare_category class is as follows.
#' \itemize{
#' \item variables : character. List of variables selected for comparison.
#' \item combination : matrix. It consists of pairs of variables to compare.
#' }
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' @seealso \code{\link{summary.compare_category}}, \code{\link{print.compare_category}}, \code{\link{plot.compare_category}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' library(stringr)
#' 
#' # Compare the all categorical variables
#' all_var <- compare_category(carseats)
#' 
#' # Print compare_numeric class object
#' all_var
#' 
#' # Compare the categorical variables that case of joint the US variable
#' all_var %>% 
#'   "["(str_detect(names(all_var), "US"))
#'   
#' # Compare the two categorical variables
#' two_var <- compare_category(carseats, ShelveLoc, Urban)
#' 
#' # Print compare_numeric class object
#' two_var
#' 
#' # Filtering the case of US included NA 
#' two_var %>%
#'   "[["(1) %>% 
#'   filter(!is.na(Urban))
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#' 
#' # component of table 
#' stat$table
#' 
#' # component of chi-square test 
#' stat$chisq
#' 
#' # component of chi-square test 
#' summary(all_var, "chisq")
#' 
#' # component of chi-square test (first, third case)
#' summary(all_var, "chisq", pos = c(1, 3))
#' 
#' # component of relative frequency table 
#' summary(all_var, "relative")
#' 
#' # component of table without missing values 
#' summary(all_var, "table", na.rm = TRUE)
#' 
#' # component of table include marginal value 
#' margin <- summary(all_var, "table", marginal = TRUE)
#' margin
#' 
#' # component of chi-square test 
#' summary(two_var, method = "chisq")
#' 
#' # verbose is FALSE 
#' summary(all_var, "chisq", verbose = FALSE)
#' 
#' #' # Using pipes & dplyr -------------------------
#' # If you want to use dplyr, set verbose to FALSE
#' summary(all_var, "chisq", verbose = FALSE) %>% 
#'   filter(p.value < 0.26)
#'   
#' # Extract component from list by index
#' summary(all_var, "table", na.rm = TRUE, verbose = FALSE) %>% 
#'   "[["(1)
#'
#' # Extract component from list by name
#'   summary(all_var, "table", na.rm = TRUE, verbose = FALSE) %>% 
#'   "[["("ShelveLoc vs Urban")
#'
#' # plot all pair of variables
#' plot(all_var)
#' 
#' # plot a pair of variables
#' plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' plot(all_var, prompt = TRUE)
#' 
#' # plot a pair of variables
#' plot(two_var, las = 1)
#' 
#' @method compare_category data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
compare_category.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  compare_category_impl(.data, vars)
}


#' @import tibble
#' @importFrom utils combn
compare_category_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) < 2) {
    stop("The number of variables selected is less than 2.")
  }
  
  df <- df[, vars]
  
  idx_category <- find_class(df, type = "categorical")
  
  if (length(idx_category) < 2) {
    stop("The number of categorical variables selected is less than 2.")
  }

  combination <- t(utils::combn(names(df)[idx_category], 2))
  x <- combination[, 1]
  y <- combination[, 2]
  
  get_frequency <- function(x, y) {
    suppressWarnings(agg_tab <- df %>% 
                       select(var1 = x, var2 = y) %>% 
                       count(var1, var2) %>% 
                       mutate(rate = n / sum(n)) %>% 
                       group_by(var1) %>% 
                       mutate(var1_rate = n /sum(n)) %>% 
                       group_by(var2) %>% 
                       mutate(var2_rate = n /sum(n)) %>% 
                       ungroup())
    
    names(agg_tab)[1:2] <- c(x, y)
    
    agg_tab
  }
  
  result <- purrr::map2(x, y, get_frequency)
  
  attr(result, "variables") <- names(df)[idx_category]
  attr(result, "combination") <- combination
  
  names(result) <- apply(combination, 1, function(x) paste(x, collapse = " vs "))
  
  class(result) <- append("compare_category", class(result))
  result
}


#' Compare numerical variables
#'
#' @description The compare_numeric() compute information to examine the relationship 
#' between numerical variables.
#'
#' @details 
#' It is important to understand the relationship between numerical variables in EDA.
#' compare_numeric() compares relations by pair combination of all numerical variables. 
#' and return compare_numeric class that based list object.
#'
#' @return An object of the class as compare based list.
#' The information to examine the relationship between numerical variables is as follows each components.
#' - correlation component : Pearson's correlation coefficient.
#' \itemize{
#' \item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
#' \item var2 : factor. The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
#' \item coef_corr : double. Pearson's correlation coefficient.
#' }
#' 
#' - linear component : linear model summaries
#' \itemize{
#' \item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
#' \item var2 : factor.The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
#' \item r.squared : double. The percent of variance explained by the model.
#' \item adj.r.squared : double. r.squared adjusted based on the degrees of freedom.
#' \item sigma : double. The square root of the estimated residual variance.
#' \item statistic : double. F-statistic.
#' \item p.value : double. p-value from the F test, describing whether the full regression is significant.
#' \item df : integer degrees of freedom.
#' \item logLik : double. the log-likelihood of data under the model.
#' \item AIC : double. the Akaike Information Criterion.
#' \item BIC : double. the Bayesian Information Criterion.
#' \item deviance : double. deviance.
#' \item df.residual : integer residual degrees of freedom.
#' }
#'
#' @section Attributes of return object:
#' Attributes of compare_numeric class is as follows.
#' \itemize{
#' \item raw : a data.frame or a \code{\link{tbl_df}}. Data containing variables to be compared. Save it for visualization with plot.compare_numeric().
#' \item variables : character. List of variables selected for comparison. 
#' \item combination : matrix. It consists of pairs of variables to compare.
#' }
#' 
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#'
#' @seealso \code{\link{correlate}}, \code{\link{summary.compare_numeric}}, \code{\link{print.compare_numeric}}, \code{\link{plot.compare_numeric}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' 
#' # Compare the all numerical variables
#' all_var <- compare_numeric(carseats)
#' 
#' # Print compare_numeric class object
#' all_var
#' 
#' # Compare the correlation that case of joint the Price variable
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(var1 == "Price" | var2 == "Price") %>% 
#'   arrange(desc(abs(coef_corr)))
#'   
#' # Compare the correlation that case of abs(coef_corr) > 0.3
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(abs(coef_corr) > 0.3)
#'   
#' # Compare the linear model that case of joint the Price variable  
#' all_var %>% 
#'   "$"(linear) %>% 
#'   filter(var1 == "Price" | var2 == "Price") %>% 
#'   arrange(desc(r.squared))
#'   
#' # Compare the two numerical variables
#' two_var <- compare_numeric(carseats, Price, CompPrice)
#' 
#' # Print compare_numeric class object
#' two_var
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Just correlation
#' summary(all_var, method = "correlation")
#' 
#' # Just correlation condition by r > 0.2
#' summary(all_var, method = "correlation", thres_corr = 0.2)
#' 
#' # linear model summaries condition by R^2 > 0.05
#' summary(all_var, thres_rs = 0.05)
#' 
#' # verbose is FALSE 
#' summary(all_var, verbose = FALSE)
#'
#' # plot all pair of variables
#' plot(all_var)
#' 
#' # plot a pair of variables
#' plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' plot(all_var, prompt = TRUE)
#' 
#' @method compare_numeric data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
compare_numeric.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  compare_numeric_impl(.data, vars)
}


#' @import tibble
#' @importFrom broom glance
#' @importFrom utils combn
compare_numeric_impl <- function(df, vars) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) < 2) {
    stop("The number of variables selected is less than 2.")
  }
  
  df <- df[, vars]
  
  idx_numeric <- find_class(df, type = "numerical")
  
  if (length(idx_numeric) < 2) {
    stop("The number of numerical variables selected is less than 2.")
  }
  
  df <- df[, idx_numeric]
  
  combination <- t(utils::combn(names(df), 2))
  x <- combination[, 1]
  y <- combination[, 2]
  
  cor_mat <- df %>% 
    cor(use = "pairwise.complete.obs")
  
  get_corr <- function(x, y) {
    dname <- dimnames(cor_mat)
    
    idx_x <- which(dname[[1]] %in% x)
    idx_y <- which(dname[[2]] %in% y)
    
    cor_mat[idx_x, idx_y]
  }
  
  get_lm <- function(x, y) {
    lm_formula <- formula(sprintf("%s ~ %s", x, y))
    
    agg_lm <- df %>% 
      select(x, y) %>% 
      lm(lm_formula, data = .) %>% 
      broom::glance()
    
    tibble::as_tibble(data.frame(var1 = x, var2 = y, agg_lm, stringsAsFactors = FALSE))
  }
  
  coef_corr <- purrr::map2_dbl(x, y, get_corr)
  lms <- purrr::map2_dfr(x, y, get_lm)
  correlation <- tibble::as_tibble(data.frame(var1 = x, var2 = y , coef_corr = coef_corr))
  
  result <- list(correlation = correlation, linear = lms)
  
  attr(result, "raw") <- df
  attr(result, "variables") <- names(df)[idx_numeric]
  attr(result, "combination") <- combination
  
  class(result) <- append("compare_numeric", class(result))
  
  result
}


#' Summarizing compare_category information
#'
#' @description print and summary method for "compare_category" class.
#' @param object an object of class "compare_category", usually, a result of a call to compare_category().
#' @param method character. Specifies the type of information to be aggregated. "table" create contingency table, 
#' "relative" create relative contingency table, and "chisq" create information of chi-square test. 
#' and "all" aggregates all information. The default is "all"
#' @param pos integer. Specifies the pair of variables to be summarized by index. 
#' The default is NULL, which aggregates all variable pairs.
#' @param na.rm logical. Specifies whether to include NA when counting the contingency tables or performing a chi-square test. 
#' The default is TRUE, where NA is removed and aggregated.
#' @param marginal logical. Specifies whether to add marginal values to the contingency table.
#' The default value is FALSE, so no marginal value is added.
#' @param verbose logical. Specifies whether to output additional information during the calculation process.
#' The default is to output information as TRUE. In this case, the function returns the value with invisible(). 
#' If FALSE, the value is returned by return().
#' @param ... further arguments passed to or from other methods.
#' @details
#' print.compare_category() displays only the information compared between the variables included in compare_category. 
#' The "type", "variables" and "combination" attributes are not displayed.
#' When using summary.compare_category(), it is advantageous to set the verbose argument to TRUE if the user is only viewing information from the console. 
#' It is also advantageous to specify FALSE if you want to manipulate the results.
#'
#' @seealso \code{\link{plot.compare_category}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' 
#' # Compare the all categorical variables
#' all_var <- compare_category(carseats)
#' 
#' # Print compare class object
#' all_var
#'   
#' # Compare the two categorical variables
#' two_var <- compare_category(carseats, ShelveLoc, Urban)
#' 
#' # Print compare class object
#' two_var
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned object
#' stat
#' 
#' # component of table 
#' stat$table
#' 
#' # component of chi-square test 
#' stat$chisq
#' 
#' # component of chi-square test 
#' summary(all_var, "chisq")
#' 
#' # component of chi-square test (first, third case)
#' summary(all_var, "chisq", pos = c(1, 3))
#' 
#' # component of relative frequency table 
#' summary(all_var, "relative")
#' 
#' # component of table without missing values 
#' summary(all_var, "table", na.rm = TRUE)
#' 
#' # component of table include marginal value 
#' margin <- summary(all_var, "table", marginal = TRUE)
#' margin
#' 
#' # component of chi-square test 
#' summary(two_var, method = "chisq")
#' 
#' # verbose is FALSE 
#' summary(all_var, "chisq", verbose = FALSE)
#' 
#' #' # Using pipes & dplyr -------------------------
#' # If you want to use dplyr, set verbose to FALSE
#' summary(all_var, "chisq", verbose = FALSE) %>% 
#'   filter(p.value < 0.26)
#'   
#' # Extract component from list by index
#' summary(all_var, "table", na.rm = TRUE, verbose = FALSE) %>% 
#'   "[["(1)
#'
#' # Extract component from list by name
#'   summary(all_var, "table", na.rm = TRUE, verbose = FALSE) %>% 
#'   "[["("ShelveLoc vs Urban")
#'   
#' @importFrom tidyr spread
#' @method summary compare_category
#' @export
summary.compare_category <- function(object, method = c("all", "table", "relative", "chisq"), 
                            pos = NULL, na.rm = TRUE, marginal = FALSE, verbose = TRUE, ...) {
  method <- match.arg(method)
  
  variables <- attr(object, "variables")
  combination <- attr(object, "combination")
  
  n <- nrow(combination)
  
  if (!is.null(pos)) {
    if (!all(pos %in% seq(n))) {
      stop("pos argument is wrong. check the position index")
    }
    
    n <- length(pos)
  } else {
    pos <- seq(n)
  }
  
  contingency <- list()
  relative <- list()
  
  chisq <- data.frame(statistic = numeric(n),
                      p.value = numeric(n),
                      parameter = integer(n)) 
  j <- 1
  for (i in pos) {
    var_names <- names(object[[i]] %>% 
                         select(1:2, n) %>% 
                         tidyr::spread(2, n, fill = 0))
    
    if (na.rm) {
      contingency[[j]] <- object[[i]] %>% 
        select(1:2, n) %>% 
        filter(!is.na(.[, 1])) %>% 
        filter(!is.na(.[, 2])) %>% 
        tidyr::spread(2, n, fill = 0) %>% 
        select(-1) %>% 
        as.matrix %>% 
        as.table()
      
      dname <- list(levels(pull(object[[i]][, 1])), levels(pull(object[[i]][, 2])))
      
      if (marginal) {
        contingency[[j]] <- cbind(contingency[[j]], margin.table(contingency[[j]], 1))
        contingency[[j]] <- rbind(contingency[[j]], margin.table(contingency[[j]], 2))
        
        dname[[1]] <- c(dname[[1]], "<Total>")
        dname[[2]] <- c(dname[[2]], "<Total>")
      }
      
      names(dname) <- names(object[[i]])[1:2]
      attr(contingency[[j]], "dimnames") <- dname
    } else {
      contingency[[j]] <- object[[i]] %>% 
        select(1:2, n) %>% 
        tidyr::spread(2, n, fill = 0) %>% 
        select(-1) %>% 
        as.matrix %>% 
        as.table()
      
      var1 <- unique(pull(object[[i]][, 1]))
      var2 <- unique(pull(object[[i]][, 2]))
                   
      dname <- list(var1, var2)
      
      if (marginal) {
        contingency[[j]] <- cbind(contingency[[j]], margin.table(contingency[[j]], 1))
        contingency[[j]] <- rbind(contingency[[j]], margin.table(contingency[[j]], 2))
        
        dname[[1]] <- c(as.character(dname[[1]]), "<Total>")
        dname[[2]] <- c(as.character(dname[[2]]), "<Total>")
      }
      
      names(dname) <- names(object[[i]])[1:2]
      attr(contingency[[j]], "dimnames") <- dname
    }
    
    if (marginal) {
      dims <- dim(contingency[[j]])
      
      relative[[j]] <- contingency[[j]][-dims[1], ]
      relative[[j]] <- relative[[j]][, -dims[2]]
      
      relative[[j]] <- prop.table(relative[[j]])
      
      relative[[j]] <- cbind(relative[[j]], margin.table(relative[[j]], 1))
      relative[[j]] <- rbind(relative[[j]], margin.table(relative[[j]], 2))

      attr(relative[[j]], "dimnames") <- dname
    } else {
      relative[[j]] <- prop.table(contingency[[j]])
    }
    
    suppressWarnings(chisq[j, ] <- contingency[[j]] %>% 
                       chisq.test() %>% 
                       broom::glance() %>% 
                       select(-method))

    j <- j + 1
  }
  
  if (length(pos) == 1) {
    names(contingency) <- paste(combination[pos, ], collapse = " vs ")
    names(relative) <- paste(combination[pos, ], collapse = " vs ")
  } else {
    names(contingency) <- apply(combination[pos, ], 1, 
                                function(x) paste(x, collapse = " vs "))
    names(relative) <- apply(combination[pos, ], 1,
                           function(x) paste(x, collapse = " vs ")) 
  }
  
  chisq <- cbind(data.frame(variable_1 = combination[pos, 1],
                            variable_2 = combination[pos, 2]), chisq)
  names(chisq)[c(1:2, 5)] <- c("variable_1", "variable_2", "df")
  
  if (verbose) {
    if (method %in% c("all", "table")) {
      cli::cat_rule(
        left = "Contingency tables",
        right = paste("Number of table is", n),
        col = "cyan",
        width = 75
      )
      print(contingency)
    }
    
    if (method %in% c("all", "relative")) {
      cli::cat_rule(
        left = "Relative contingency tables",
        right = paste("Number of table is", n),
        col = "cyan",
        width = 75
      )
      print(relative)
    }
    
    if (method %in% c("all", "chisq")) {
      cli::cat_rule(
        left = "Chi-squared contingency table tests",
        right = paste("Number of table is", n),
        col = "cyan",
        width = 75
      )
      print(chisq)
    } 
  }
  
  if (method == "all") {
    result <- list(table = contingency, relative = relative, chisq = chisq)
  } else if (method == "table") {
    result <- contingency
  } else if (method == "relative") {
    result <- relative
  } else if (method == "chisq") {
    result <- chisq
  }  
  
  if (verbose) {
    invisible(result)
  } else {
    return(result)
  }  
}



#' Summarizing compare_numeric information
#'
#' @description print and summary method for "compare_numeric" class.
#' @param object an object of class "compare_numeric", usually, a result of a call to compare_numeric().
#' @param method character. Select statistics to be aggregated. 
#' "correlation" calculates the Pearson's correlation coefficient, and "linear" returns the aggregation of the linear model.
#' "all" returns both information. 
#' However, the difference between summary.compare_numeric() and compare_numeric() is that only cases that are greater than the specified threshold are returned.
#' "correlation" returns only cases with a correlation coefficient greater than the thres_corr argument value. 
#' "linear" returns only cases with R^2 greater than the thres_rs argument.
#' @param thres_corr numeric. This is the correlation coefficient threshold of the correlation coefficient information to be returned. 
#' The default is 0.3.
#' @param thres_rs numeric. R^2 threshold of linear model summaries information to return. 
#' The default is 0.1.
#' @param verbose logical. Specifies whether to output additional information during the calculation process.
#' The default is to output information as TRUE. In this case, the function returns the value with invisible(). 
#' If FALSE, the value is returned by return().
#' @param ... further arguments passed to or from other methods.
#' @details
#' print.compare_numeric() displays only the information compared between the variables included in compare_numeric. 
#' When using summary.compare_numeric(), it is advantageous to set the verbose argument to TRUE if the user is only viewing information from the console. 
#' It is also advantageous to specify FALSE if you want to manipulate the results.
#'
#' @return An object of the class as compare based list.
#' The information to examine the relationship between numerical variables is as follows each components.
#' - correlation component : Pearson's correlation coefficient.
#' \itemize{
#' \item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
#' \item var2 : factor. The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
#' \item coef_corr : double. Pearson's correlation coefficient.
#' }
#' 
#' - linear component : linear model summaries
#' \itemize{
#' \item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
#' \item var2 : factor. The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
#' \item r.squared : double. The percent of variance explained by the model.
#' \item adj.r.squared : double. r.squared adjusted based on the degrees of freedom.
#' \item sigma : double. The square root of the estimated residual variance.
#' \item statistic : double. F-statistic.
#' \item p.value : double. p-value from the F test, describing whether the full regression is significant.
#' \item df : integer degrees of freedom.
#' \item logLik : double. the log-likelihood of data under the model.
#' \item AIC : double. the Akaike Information Criterion.
#' \item BIC : double. the Bayesian Information Criterion.
#' \item deviance : double. deviance.
#' \item df.residual : integer residual degrees of freedom.
#' }
#' 
#' @seealso \code{\link{plot.compare_numeric}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' library(dplyr)
#' 
#' # Compare the all numerical variables
#' all_var <- compare_numeric(carseats)
#' 
#' # Print compare class object
#' all_var
#' 
#' # Compare the correlation that case of joint the Price variable
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(var1 == "Price" | var2 == "Price") %>% 
#'   arrange(desc(abs(coef_corr)))
#'   
#' # Compare the correlation that case of abs(coef_corr) > 0.3
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(abs(coef_corr) > 0.3)
#'   
#' # Compare the linear model that case of joint the Price variable  
#' all_var %>% 
#'   "$"(linear) %>% 
#'   filter(var1 == "Price" | var2 == "Price") %>% 
#'   arrange(desc(r.squared))
#'   
#' # Compare the two numerical variables
#' two_var <- compare_numeric(carseats, Price, CompPrice)
#' 
#' # Print compare class object
#' two_var
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Just correlation
#' summary(all_var, method = "correlation")
#' 
#' # Just correlation condition by r > 0.2
#' summary(all_var, method = "correlation", thres_corr = 0.2)
#' 
#' # linear model summries condition by R^2 > 0.05
#' summary(all_var, thres_rs = 0.05)
#' 
#' # verbose is FALSE 
#' summary(all_var, verbose = FALSE)
#'   
#' @importFrom tidyr spread
#' @method summary compare_numeric
#' @export
summary.compare_numeric <- function(object, method = c("all", "correlation", "linear"), 
                                    thres_corr = 0.3, thres_rs = 0.1, verbose = TRUE, ...) {
  method <- match.arg(method)
  
  variables <- attr(object, "variables")
  combination <- attr(object, "combination")
    
  n <- nrow(combination)
  
  if (method %in% c("all", "correlation")) {
    correlation <- object$correlation %>% 
      arrange(desc(abs(coef_corr))) %>% 
      filter(abs(coef_corr) > thres_corr)
  }  

  if (method %in% c("all", "linear")) {
    linear <- object$linear %>% 
      arrange(-r.squared) %>% 
      filter(r.squared > thres_rs)
  }    
  
  if (verbose) {
    if (method %in% c("all", "correlation")) {
      cli::cat_rule(
        left = sprintf("Correlation check : abs(r) > %g", thres_corr),
        right = sprintf("Number of pairs is %d/%d", nrow(correlation), n),
        col = "cyan",
        width = 75
      )
      print(correlation)
    }
    
    if (method %in% c("all", "linear")) {
      cli::cat_rule(
        left = sprintf("R.squared check : R^2 > %g", thres_rs),
        right = sprintf("Number of pairs is %d/%d", nrow(linear), n),
        col = "cyan",
        width = 75
      )
      print(linear)
    }
  }
  
  if (method == "all") {
    result <- list(correlation = correlation, linear = linear)
  } else if (method == "correlation") {
    result <- correlation
  } else if (method == "linear") {
    result <- linear
  }
  
  if (verbose) {
    invisible(result)
  } else {
    return(result)
  }  
}


#' @param x an object of class "compare_category", usually, a result of a call to compare_category().
#' @param ... further arguments passed to or from other methods.
#' @rdname summary.compare_category
#' @method print compare_category
#' @export
print.compare_category <- function(x, ...) {
  vnames <- names(x)
  attributes(x) <- NULL
  names(x) <- vnames
  
  print(x, ...)
}

#' @param x an object of class "compare_numeric", usually, a result of a call to compare_numeric().
#' @param ... further arguments passed to or from other methods.
#' @rdname summary.compare_numeric
#' @method print compare_numeric
#' @export
print.compare_numeric <- function(x, ...) {
  vnames <- names(x)
  attributes(x) <- NULL
  names(x) <- vnames
  
  print(x, ...)
}


#' Visualize Information for an "compare_category" Object
#'
#' @description
#' Visualize mosaics plot by attribute of compare_category class.
#'
#' @param x an object of class "compare_category", usually, a result of a call to compare_category().
#' @param prompt logical. The default value is FALSE. If there are multiple visualizations to be output, if this argument value is TRUE, a prompt is output each time. 
#' @param na.rm logical. Specifies whether to include NA when plotting mosaics plot. 
#' The default is FALSE, so plot NA.  
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' However, it does not support all parameters.
#' @seealso \code{\link{compare_category}}, \code{\link{print.compare_category}}, \code{\link{summary.compare_category}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Compare the all categorical variables
#' all_var <- compare_category(carseats)
#' 
#' # Print compare class object
#' all_var
#'   
#' # Compare the two categorical variables
#' two_var <- compare_category(carseats, ShelveLoc, Urban)
#' 
#' # Print compare class object
#' two_var
#' 
#' # plot all pair of variables
#' plot(all_var)
#' 
#' # plot a pair of variables
#' plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' plot(all_var, prompt = TRUE)
#' 
#' # plot a pair of variables
#' plot(two_var, las = 1)
#' 
#' @method plot compare_category
#' @export
plot.compare_category <- function(x, prompt = FALSE, na.rm = FALSE, ...) {
  variables <- attr(x, "variables")
  combination <- attr(x, "combination")
  
  n <- nrow(combination)
  
  for (i in seq(n)) {
    rnames <- unique(pull(x[[i]][, 1]))
    cnames <- unique(pull(x[[i]][, 2]))
      
    xvar <- combination[i, 1]
    yvar <- combination[i, 2]  
      
    value <- expand.grid(rnames, cnames)
    names(value) <- c(xvar, yvar)
      
    suppressMessages(value <- value %>% 
                       left_join(x[[i]] %>% select(1:3)) %>% 
                       mutate(n = ifelse(is.na(n), 0, n)))
      
    tab <- matrix(value$n, nrow = length(rnames), ncol = length(cnames),
                  dimnames = list(rnames, cnames))
      
    oldClass(tab) <- c("table")
      
    dimnames(tab)[[1]] <- ifelse(is.na(dimnames(tab)[[1]]), "NA", 
                                 dimnames(tab)[[1]])
    dimnames(tab)[[2]] <- ifelse(is.na(dimnames(tab)[[2]]), "NA", 
                                   dimnames(tab)[[2]])
      
    if (prompt & n > 1) {
      invisible(readline(prompt="Hit <Return> to see next plot:"))
    }
      
    plot(tab, col = RColorBrewer::brewer.pal(8, "Dark2"),
         xlab = xvar, ylab = yvar,
         main = sprintf("Mosaics plot by '%s' vs '%s'", xvar, yvar), ...)
  } 
}


#' Visualize Information for an "compare_numeric" Object
#'
#' @description
#' Visualize scatter plot included box plots by attribute of compare_numeric class.
#'
#' @param x an object of class "compare_numeric", usually, a result of a call to compare_numeric().
#' @param prompt logical. The default value is FALSE. If there are multiple visualizations to be output, if this argument value is TRUE, a prompt is output each time. 
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' However, it does not support.
#' @seealso \code{\link{compare_numeric}}, \code{\link{print.compare_numeric}}, \code{\link{summary.compare_numeric}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Compare the all numerical variables
#' all_var <- compare_numeric(carseats)
#' 
#' # Print compare compare_numeric object
#' all_var
#'   
#' # Compare the two numerical variables
#' two_var <- compare_numeric(carseats, CompPrice, Price)
#' 
#' # Print compare_numeric class object
#' two_var
#' 
#' # plot all pair of variables
#' plot(all_var)
#' 
#' # plot a pair of variables
#' plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' plot(all_var, prompt = TRUE)
#' 
#' @importFrom car scatterplot
#' @method plot compare_numeric
#' @export
plot.compare_numeric <- function(x, prompt = FALSE, ...) {
  variables <- attr(x, "variables")
  combination <- attr(x, "combination")
  
  n <- nrow(combination)
  
  df <- attr(x, "raw")
    
  for (i in seq(n)) {
    xvar <- combination[i, 1]
    yvar <- combination[i, 2]  
      
    datas <- df[ , c(xvar, yvar)]
    str_formula <- formula(sprintf("%s ~ %s", xvar, yvar))
      
    if (prompt & n > 1) {
      invisible(readline(prompt="Hit <Return> to see next plot:"))
    }
      
    car::scatterplot(str_formula, data = datas, ellipse = TRUE,
                     col = "orange", pch = 16,
                     main = sprintf("Scatterplots with %s and %s", xvar, yvar))
  } 
}
