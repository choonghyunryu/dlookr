#' Compute Predictive Power Score
#'
#' @description The pps() compute PPS(Predictive Power Score) 
#' for exploratory data analysis.
#'
#' @details The PPS is an asymmetric, data-type-agnostic score that can detect 
#' linear or non-linear relationships between two variables. 
#' The score ranges from 0 (no predictive power) to 1 (perfect predictive power).
#'
#' @section Information of Predictive Power Score:
#' The information of PPS is as follows.
#'
#' \itemize{
#' \item x : the name of the predictor variable
#' \item y : the name of the target variable
#' \item result_type : text showing how to interpret the resulting score
#' \item pps : the predictive power score
#' \item metric : the evaluation metric used to compute the PPS
#' \item baseline_score : the score of a naive model on the evaluation metric
#' \item model_score : the score of the predictive model on the evaluation metric
#' \item cv_folds : how many cross-validation folds were used
#' \item seed : the seed that was set
#' \item algorithm : text shwoing what algorithm was used
#' \item model_type : text showing whether classification or regression was used
#' }
#'
#' @param .data a target_df.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, describe() will automatically start with all variables.
#' These arguments are automatically quoted and evaluated in a context where column names
#' represent column positions.
#' They support unquoting and splicing.
#' @param do_parallel logical. whether to perform score calls in parallel.
#' @param n_cores	integer. number of cores to use, defaults to maximum minus 1.
#' @param cv_folds integer. number of cross-validation folds.
#'
#' @return An object of the class as pps.
#' Attributes of pps class is as follows.
#' \itemize{
#' \item target : name of target variable
#' \item predictor : name of predictor
#' }
#' @seealso \code{\link{print.relate}}, \code{\link{plot.relate}}.
#'
#' @references 
#' \itemize{
#'   \item RIP correlation. Introducing the Predictive Power Score - by Florian Wetschoreck
#'     \itemize{
#'       \item https://towardsdatascience.com/rip-correlation-introducing-the-predictive-power-score-3d90808b9598
#'     }
#' }
#' 
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # If the target variable is a categorical variable
#' categ <- target_by(heartfailure, death_event)
#' 
#' # compute all variables
#' pps_cat <- pps(categ)
#' pps_cat
#' 
#' # compute age and sodium variable
#' pps_cat <- pps(categ, age, sodium)
#' pps_cat
#' 
#' # Using dplyr
#' pps_cat <- heartfailure %>% 
#'   target_by(death_event) %>% 
#'   pps()
#' 
#' pps_cat
#' 
#' # Using parallel process
#' pps_cat <- heartfailure %>% 
#'   target_by(death_event) %>% 
#'   pps(do_parallel = TRUE)
#' 
#' pps_cat
#' 
#' # summary pps class 
#' summary(pps_cat)
#' 
#' # plot pps class
#' plot(pps_cat)
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' num <- target_by(heartfailure, creatinine)
#' 
#' pps_num <- pps(num)
#' pps_num
#' 
#' # summary pps class 
#' summary(pps_num)
#' 
#' # plot pps class
#' plot(pps_num)
#' }
#' 
#' @export
pps <- function(.data, ...) {
  UseMethod("pps", .data)
}


#' @rdname pps
#' @method pps target_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
pps.target_df <- function(.data, ..., cv_folds = 5, do_parallel = FALSE, 
                          n_cores = -1) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  pps_impl(.data, vars, cv_folds = cv_folds, do_parallel = do_parallel, 
           n_cores = n_cores)
}

#' @importFrom stats cor formula lm xtabs
#' @importFrom tidyselect matches
#' @importFrom methods is
pps_impl <- function(.data, vars, cv_folds, do_parallel, n_cores) {
  if (!requireNamespace("ppsr", quietly = TRUE)) {
    stop("Package \"ppsr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  if (length(vars) == 0) vars <- names(.data)
  
  if (utils::packageVersion("dplyr") >= "0.8.0") {
    target <- attr(.data, "groups") %>% names() %>% "["(1)
  } else {
    target <- attr(.data, "vars")
  }  
  
  .data <- .data[, unique(c(vars, target))] 
  
  tab_pps <- ppsr::score_predictors(df = .data, y = target, cv_folds = cv_folds,
                                    do_parallel = do_parallel, n_cores = n_cores)
  
  attr(tab_pps, "target") <- tab_pps[1, "y"]
  attr(tab_pps, "predictor") <- tab_pps[-NROW(tab_pps), "x"]

  class(tab_pps) <- append("pps", class(tab_pps))
  
  tab_pps
}


#' Summarizing Predictive Power Score
#'
#' @description print and summary method for "pps" class.
#' @param object an object of class "pps", usually, a result of a call to pps().
#' @param ... further arguments passed to or from other methods.
#' @details
#' summary.pps compares the PPS by variables.
#'
#' @seealso \code{\link{pps}}, \code{\link{plot.pps}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # If the target variable is a categorical variable
#' # Using dplyr
#' pps_cat <- heartfailure %>% 
#'   target_by(death_event) %>% 
#'   pps()
#' 
#' pps_cat
#' 
#' # summary pps class 
#' tab <- summary(pps_cat)
#' tab
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' num <- target_by(heartfailure, creatinine)
#' 
#' pps_num <- pps(num)
#' pps_num
#' 
#' # summary pps class 
#' summary(pps_num)
#' }
#' 
#' @method summary pps
#' @import dplyr
#' @export
summary.pps <- function(object, ...) {
  smmry <- object %>% 
    arrange(desc(pps)) %>% 
    select(x, y, pps) %>% 
    rename("predictors" = x) %>% 
    rename("target" = y) 

  cat("* Target variable :", attr(object, "target"), "\n")
  cat("* Model type :", object$model_type[1], "\n")
  
  cat("* Information of Predictive Power Score\n")
  print(smmry)
  
  invisible(smmry)
}


#' Visualize Information for an "pps" Object
#'
#' @description
#' Visualize by attribute of `pps` class.
#' The plot of a PPS(Predictive Power Score) is a bar plot by PPS.
#' 
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "pps", usually, a result of a call to pps().
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' @seealso \code{\link{pps}}, \code{\link{summary.pps}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # If the target variable is a categorical variable
#' # Using dplyr
#' pps_cat <- heartfailure %>% 
#'   target_by(death_event) %>% 
#'   pps()
#' 
#' # plot pps class
#' plot(pps_cat)
#' 
#' ##---------------------------------------------------
#' # If the target variable is a numerical variable
#' # Using dplyr
#' pps_num <- heartfailure %>% 
#'   target_by(creatinine) %>% 
#'   pps()
#' 
#' # plot pps class
#' plot(pps_num)
#' }
#' 
#' @method plot pps
#' @import ggplot2
#' @import dplyr
#' @importFrom hrbrthemes scale_fill_ipsum
#' @export
plot.pps <- function(x, typographic = TRUE, base_family = NULL, ...) {
  p_pps <- x %>%
    ggplot(aes(x = reorder(x, pps), y = pps, fill = pps)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(pps, 2)), hjust = -0.2) +
    coord_flip() +
    scale_fill_gradient(low = "blue", high = "red") + 
    labs(title = "Predictive Power Score", x = "predictors") + 
    theme_grey(base_family = base_family) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  if (typographic) {
    p_pps <- p_pps +
      theme_typographic(base_family) +
      theme(axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13))
  }  
  
  p_pps
}
