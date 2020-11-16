#' dlookr: Tools for Data Diagnosis, Exploration, Transformation
#'
#' dlookr provides data diagnosis, data exploration and transformation of variables
#' during data analysis.
#'
#' It has three main goals:
#'
#' \itemize{
#' \item When data is acquired, it is possible to judge whether data is erroneous
#' or to select a variable to be corrected or removed through data diagnosis.
#' \item Understand the distribution of data in the EDA process. It can also
#' understand the relationship between target variables and predictor variables
#' for the prediction model.
#' \item Imputes missing value and outlier to standardization and resolving skewness.
#' And, To convert a continuous variable to a categorical variable, bin the continuous variables.
#' }
#'
#' To learn more about dlookr, start with the vignettes:
#' `browseVignettes(package = "dlookr")`
#'
#' @import dplyr
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "key", "value", "var1", "var2", "outlier", "outliers_cnt",
    "outliers_mean", "outliers_ratio", "p_value", "variable", "with_mean", "without_mean",
    "Q1", "Q3", "minus", "freq", "zero", "obs", "coef_corr", "method", "r.squared", 
    "Var1", "Var2", "pos_x", "pos_y", "na_pct", "n_var", "name_var", "n_obs", "ratio",
    "txt", "name", "cumulative", "y", "frequencies"))
}
