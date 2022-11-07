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
#' @import showtext
#' @import ggplot2
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "key", "value", "var1", "var2", "outlier",
    "outliers_cnt", "outliers_mean", "outliers_ratio", "p_value", "variable",
    "with_mean", "without_mean", "Q1", "Q3", "minus", "freq", "zero", "obs",
    "coef_corr", "method", "r.squared", "Var1", "Var2", "pos_x", "pos_y",
    "na_pct", "n_var", "name_var", "n_obs", "ratio", "txt", "name",
    "cumulative", "y", "frequencies", "A", "B", "a", "b", "AUC", "Bin", 
    "CntCumNeg", "CntCumPos", "CntNeg", "CntPos", "CntRec", "Freq", "LnOdds",
    "Odds", "PctPos", "PctRec", "REV", "RateCumNeg", "RateCumPos", "RateNeg",
    "RatePos", "WoE", "add_pos", "cnt", "datas", "diff_prim", "diff_sec",
    "flag", "indicator", "menas", "min_prim", "min_sec", "msg", "n_missing",
    "na.exclude", "na_flag", "object.size", "reformulate", "target", "x_end",
    "x_start", "x_width", "AIC", "BIC", "deviance", "logLik", "n_case",
    "IQR", "addmargins", "cardinality", "df", "df.residual", "metric", 
    "minus_count", "missing_count", "missing_percent", "n_character", "n_word", 
    "na", "nobs", "other_freq", "outlier_count", "p.value", "p00", "p10", "p100",
    "p20", "p25", "p30", "p40", "p50", "p60", "p70", "p75", "p80", "p90", 
    "rate_missing", "rate_outlier", "rgb", "runif", "se_mean", "statistic", 
    "status", "top_freq", "types", "unique_count", "unique_rate", "variable_1", 
    "variable_2", "variables", "zero_count", "reportData", "predictors", "x",
    "described_variables", "index"))
}