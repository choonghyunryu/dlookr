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
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' 
#' library(dplyr)
#' 
#' # Compare the all categorical variables
#' all_var <- compare_category(heartfailure2)
#' 
#' # Print compare_numeric class objects
#' all_var
#' 
#' # Compare the categorical variables that case of joint the death_event variable
#' all_var %>% 
#'   "["(grep("death_event", names(all_var)))
#' 
#' # Compare the two categorical variables
#' two_var <- compare_category(heartfailure2, smoking, death_event)
#' 
#' # Print compare_category class objects
#' two_var
#' 
#' # Filtering the case of smoking included NA 
#' two_var %>%
#'   "[["(1) %>% 
#'   filter(!is.na(smoking))
#' 
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned objects
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
#' summary(all_var, "table", na.rm = TRUE, verbose = FALSE) %>% 
#'   "[["("smoking vs death_event")
#' 
#' # plot all pair of variables
#' # plot(all_var)
#' 
#' # plot a pair of variables
#' # plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' # plot(all_var, prompt = TRUE)
#' 
#' # plot a pair of variables
#' # plot(two_var, las = 1)
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
#' heartfailure2 <- heartfailure[, c("platelets", "creatinine", "sodium")]
#'
#' library(dplyr)
#' # Compare the all numerical variables
#' all_var <- compare_numeric(heartfailure2)
#' 
#' # Print compare_numeric class object
#' all_var
#' 
#' # Compare the correlation that case of joint the sodium variable
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(var1 == "sodium" | var2 == "sodium") %>% 
#'   arrange(desc(abs(coef_corr)))
#'   
#' # Compare the correlation that case of abs(coef_corr) > 0.1
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(abs(coef_corr) > 0.1)
#'   
#' # Compare the linear model that case of joint the sodium variable  
#' all_var %>% 
#'   "$"(linear) %>% 
#'   filter(var1 == "sodium" | var2 == "sodium") %>% 
#'   arrange(desc(r.squared))
#'   
#' # Compare the two numerical variables
#' two_var <- compare_numeric(heartfailure2, sodium, creatinine)
#' 
#' # Print compare_numeric class objects
#' two_var
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Just correlation
#' summary(all_var, method = "correlation")
#' 
#' # Just correlation condition by r > 0.1
#' summary(all_var, method = "correlation", thres_corr = 0.1)
#' 
#' # linear model summaries condition by R^2 > 0.05
#' summary(all_var, thres_rs = 0.05)
#' 
#' # verbose is FALSE 
#' summary(all_var, verbose = FALSE)
#'   
#' # plot all pair of variables
#' # plot(all_var)
#' 
#' # plot a pair of variables
#' # plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' # plot(all_var, prompt = TRUE)
#' 
#' # plot a pair of variables not focuses on typographic elements
#' # plot(two_var, typographic = FALSE)
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
    lm_formula <- formula(sprintf("`%s` ~ `%s`", x, y))
    
    agg_lm <- df %>% 
      select({{x}}, {{y}}) %>% 
      lm(lm_formula, data = .) %>% 
      get_tab_lm()
    
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
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' 
#' library(dplyr)
#' 
#' # Compare the all categorical variables
#' all_var <- compare_category(heartfailure2)
#' 
#' # Print compare_category class objects
#' all_var
#' 
#' # Compare the two categorical variables
#' two_var <- compare_category(heartfailure2, smoking, death_event)
#' 
#' # Print compare_category class objects
#' two_var
#' 
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Summary by returned objects
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
#' summary(all_var, "table", na.rm = TRUE, verbose = FALSE) %>% 
#'   "[["("smoking vs death_event")
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
        tidyr::spread(2, n, fill = 0) %>% 
        select(!contains("<NA>")) %>% 
        .[!is.na(.[, 1]), ] %>% 
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
                       get_tab_chisq() %>% 
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
      cat_rule(
        left = "Contingency tables",
        right = paste("Number of table is", n),
        col = "cyan",
        width = 75
      )
      print(contingency)
    }
    
    if (method %in% c("all", "relative")) {
      cat_rule(
        left = "Relative contingency tables",
        right = paste("Number of table is", n),
        col = "cyan",
        width = 75
      )
      print(relative)
    }
    
    if (method %in% c("all", "chisq")) {
      cat_rule(
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
#' heartfailure2 <- heartfailure[, c("platelets", "creatinine", "sodium")]
#'
#' library(dplyr)
#' # Compare the all numerical variables
#' all_var <- compare_numeric(heartfailure2)
#' 
#' # Print compare_numeric class object
#' all_var
#' 
#' # Compare the correlation that case of joint the sodium variable
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(var1 == "sodium" | var2 == "sodium") %>% 
#'   arrange(desc(abs(coef_corr)))
#'   
#' # Compare the correlation that case of abs(coef_corr) > 0.1
#' all_var %>% 
#'   "$"(correlation) %>% 
#'   filter(abs(coef_corr) > 0.1)
#'   
#' # Compare the linear model that case of joint the sodium variable  
#' all_var %>% 
#'   "$"(linear) %>% 
#'   filter(var1 == "sodium" | var2 == "sodium") %>% 
#'   arrange(desc(r.squared))
#'   
#' # Compare the two numerical variables
#' two_var <- compare_numeric(heartfailure2, sodium, creatinine)
#' 
#' # Print compare_numeric class objects
#' two_var
#'   
#' # Summary the all case : Return a invisible copy of an object.
#' stat <- summary(all_var)
#' 
#' # Just correlation
#' summary(all_var, method = "correlation")
#' 
#' # Just correlation condition by r > 0.1
#' summary(all_var, method = "correlation", thres_corr = 0.1)
#' 
#' # linear model summaries condition by R^2 > 0.05
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
      cat_rule(
        left = sprintf("Correlation check : abs(r) > %g", thres_corr),
        right = sprintf("Number of pairs is %d/%d", nrow(correlation), n),
        col = "cyan",
        width = 75
      )
      print(correlation)
    }
    
    if (method %in% c("all", "linear")) {
      cat_rule(
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
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. 
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' However, it only support las parameter. las is numeric in {0,1}; the style of axis labels.
#' \itemize{
#'   \item 0 : always parallel to the axis [default],
#'   \item 1 : always horizontal to the axis,
#' }
#'    
#' @seealso \code{\link{compare_category}}, \code{\link{print.compare_category}}, \code{\link{summary.compare_category}}.
#' @examples
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
#' 
#' library(dplyr)
#' 
#' # Compare the all categorical variables
#' all_var <- compare_category(heartfailure2)
#' 
#' # Print compare_numeric class objects
#' all_var
#' 
#' # Compare the two categorical variables
#' two_var <- compare_category(heartfailure2, smoking, death_event)
#' 
#' # Print compare_category class objects
#' two_var
#' 
#' # plot all pair of variables
#' plot(all_var)
#' 
#' # plot a pair of variables
#' plot(two_var)
#'
#' # plot all pair of variables by prompt
#' # plot(all_var, prompt = TRUE)
#'   
#' # plot a pair of variables without NA
#' plot(two_var, na.rm = TRUE)
#' 
#' # plot a pair of variables
#' plot(two_var, las = 1)
#' 
#' # plot a pair of variables not focuses on typographic elements
#' plot(two_var, typographic = FALSE)
#' 
#' @method plot compare_category
#' @export
plot.compare_category <- function(x, prompt = FALSE, na.rm = FALSE, 
                                  typographic = TRUE, base_family = NULL, ...) {
  combination <- attr(x, "combination")
  
  n <- nrow(combination)
  
  arg_par <- list(...)
  
  las <- 0
  if (length(arg_par) > 0 & any(names(arg_par) %in% "las")) {
    las <- arg_par$las
  } 
  
  for (i in seq(n)) {
    xvar <- combination[i, 1]
    yvar <- combination[i, 2]  
    
    if (prompt & n > 1) {
      invisible(readline(prompt="Hit <Return> to see next plot:"))
    }
    
    data <- x[[i]] %>% 
      select(a = !!sym(xvar), b = !!sym(yvar), n) 
    
    if (na.rm) {
      data <- data %>% 
        filter(!is.na(a) & !is.na(b))
    }
    
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
        theme_grey(base_family = base_family) +         
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              strip.background = element_blank(),
              panel.spacing = unit(0, "pt")) 
    })
    
    if (typographic) {
      p <- p +
        theme_typographic(base_family) +
        scale_fill_ipsum(na.value = "grey80") +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              panel.spacing = unit(0, "pt"))
      if (las == 0) {
        p <-  p +
          theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
      }  
      
    } else {
      if (las == 0) {
        p <-  p +
          theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
      }  
    }
    
    suppressWarnings(print(p))
  } 
}


#' Visualize Information for an "compare_numeric" Object
#'
#' @description
#' Visualize scatter plot included box plots by attribute of compare_numeric class.
#'
#' @param x an object of class "compare_numeric", usually, a result of a call to compare_numeric().
#' @param prompt logical. The default value is FALSE. If there are multiple visualizations to be output, 
#' if this argument value is TRUE, a prompt is output each time. 
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' However, it does not support.
#' @seealso \code{\link{compare_numeric}}, \code{\link{print.compare_numeric}}, \code{\link{summary.compare_numeric}}.
#' @examples
#' # Generate data for the example
#' heartfailure2 <- heartfailure[, c("platelets", "creatinine", "sodium")]
#'
#' library(dplyr)
#' # Compare the all numerical variables
#' all_var <- compare_numeric(heartfailure2)
#' 
#' # Print compare_numeric class object
#' all_var
#'   
#' # Compare the two numerical variables
#' two_var <- compare_numeric(heartfailure2, sodium, creatinine)
#' 
#' # Print compare_numeric class objects
#' two_var
#'   
#' # plot all pair of variables
#' plot(all_var)
#' 
#' # plot a pair of variables
#' plot(two_var)
#' 
#' # plot all pair of variables by prompt
#' # plot(all_var, prompt = TRUE)
#' 
#' # plot a pair of variables not focuses on typographic elements
#' plot(two_var, typographic = FALSE)
#' 
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @import ggplot2
#' @method plot compare_numeric
#' @export
plot.compare_numeric <- function(x, prompt = FALSE, typographic = TRUE, 
                                 base_family = NULL, ...) {
  combination <- attr(x, "combination")
  
  n <- nrow(combination)
  
  df <- attr(x, "raw")
  
  for (i in seq(n)) {
    xvar <- combination[i, 1]
    yvar <- combination[i, 2]  
    
    datas <- df[ , c(xvar, yvar)]
    
    if (prompt & n > 1) {
      invisible(readline(prompt="Hit <Return> to see next plot:"))
    }
    
    blank <- ggplot() + geom_blank(aes(1, 1)) +
      theme(
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      )
    
    p_scatter <- datas %>% 
      ggplot(aes(x = !!sym(xvar), y = !!sym(yvar))) +
      geom_point(color = "#FF9900") +
      stat_minmax_ellipse(geom = "polygon", alpha = 0.3, fill = "steelblue", color = "steelblue") + 
      stat_minmax_ellipse(level = 0.5, geom = "polygon", alpha = 0.5, 
                          fill = "steelblue", color = "steelblue") + 
      stat_smooth(method = "lm", formula = y ~ x) +
      theme_grey(base_family = base_family)
    
    box_bottom <- datas %>% 
      ggplot(aes(y = !!sym(xvar))) + 
      geom_boxplot(size = 0.3, fill = "steelblue", alpha = 0.3) +
      xlab(xvar) +
      coord_flip() +      
      theme_grey(base_family = base_family) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(color = "transparent"),
        axis.text.y = element_text(color = "transparent"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))
    
    box_left <- datas %>% 
      ggplot(aes(y = !!sym(yvar))) + 
      geom_boxplot(size = 0.3, fill = "steelblue", alpha = 0.3) + 
      xlab("") +
      theme_grey(base_family = base_family) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.text.x = element_text(color = "transparent"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))
    
    title <- sprintf("Scatterplots with %s and %s", xvar, yvar)
    
    if (typographic) {
      p_scatter <- p_scatter +
        theme_typographic(base_family) +
        theme(
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 13)
        )
      
      box_bottom <- box_bottom  +
        theme_typographic(base_family) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.y = element_text(color = "transparent"),
              axis.text.y = element_text(color = "transparent"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.margin = margin(0, 30, 0, 30))
      
      box_left <- box_left  +
        theme_typographic(base_family) +
        theme(axis.title.x = element_text(color = "transparent"),
              axis.text.x = element_text(color = "transparent"),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),              
              plot.margin = margin(30, 0, 30, 0))
      
      if (is.null(base_family)) {
        base_family <- get_font_family()
      }
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = base_family, fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    suppressWarnings(gridExtra::grid.arrange(box_left, p_scatter, blank, box_bottom, ncol = 2, nrow = 2,
                                             widths = c(1, 25), heights=c(26, 2), top = title))
  } 
}

StatMinMaxEllipse <- ggproto("StatClipEllipse", Stat,
                             required_aes = c("x", "y"),
                             compute_group = function(data, scales, type = "t", level = 0.95,
                                                      segments = 51, na.rm = FALSE) {
                               min_x <- min(data$x)
                               max_x <- max(data$x)
                               min_y <- min(data$y)
                               max_y <- max(data$y)
                               
                               xx <- ggplot2:::calculate_ellipse(data = data, vars = c("x", "y"), type = type,
                                                                 level = level, segments = segments)
                               xx %>% mutate(x=pmax(x, min_x)) %>% 
                                 mutate(x=pmin(x, max_x)) %>% 
                                 mutate(y=pmax(y, min_y)) %>% 
                                 mutate(y=pmin(y, max_y)) 
                             }
)

stat_minmax_ellipse <- function(mapping = NULL, data = NULL,
                                geom = "path", position = "identity",
                                ...,
                                type = "t",
                                level = 0.95,
                                segments = 51,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatMinMaxEllipse,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      level = level,
      segments = segments,
      na.rm = na.rm,
      ...
    )
  )
}
