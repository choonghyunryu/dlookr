#' Describe overview of data
#'
#' @description
#' Inquire basic information to understand the data in general.
#'
#' @details
#' overview() creates an overview class.
#' The `overview` class includes general information such as the size of the data, the degree of missing values, 
#' and the data types of variables.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @return An object of overview class. 
#' The overview class contains data.frame and two attributes. data.frame has the following 3 variables.:
#' data.frame is as follow.:
#' \itemize{
#'   \item division : division of information.
#'   \itemize{
#'     \item size : indicators of related to data capacity 
#'     \item missing : indicators of related to missing value 
#'     \item data_type : indicators of related to data type 
#'   }
#'   \item metrics : name of metrics.
#'   \itemize{
#'     \item observations : number of observations (number of rows)
#'     \item variables : number of variables (number of columns)
#'     \item values : number of values (number of cells. rows * columns)
#'     \item memory_size : an estimate of the memory that is being used to store an R object.
#'     \item complete_obs : number of complete cases(observations). i.e., have no missing values.
#'     \item missing_obs : number of observations that has missing values.
#'     \item missing_vars : number of variables that has missing values.
#'     \item missing_values : number of values(cells) that has missing values.
#'     \item numerics : number of variables that is data type is numeric.
#'     \item integers : number of variables that is data type is integer.
#'     \item factors : number of variables that is data type is factor.
#'     \item characters : number of variables that is data type is character.
#'     \item others : number of variables that is not above.
#'   }
#'   \item value : value of metrics.
#' }
#' 
#' Attributes of overview class is as follows.:
#' \itemize{
#'   \item na_col : the data type of predictor to replace missing value.
#'   \item info_class : data.frame. variable name and class name that describe the data type of variables.
#'   \itemize{
#'     \item data.frame has a two variables.
#'     \itemize{
#'       \item variable : variable names
#'       \item class : data type
#'     }
#'   }
#' }
#' @seealso \code{\link{summary.overview}}, \code{\link{plot.overview}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' ov <- overview(carseats)
#' ov
#' 
#' summary(ov)
#' 
#' # plot(ov)
#' }
#' 
#' @export
overview <- function(.data) {
  n_row <- dim(.data)[1]
  n_col <- dim(.data)[2]
  
  size <- object.size(.data)
  
  complete <- complete.cases(.data)
  
  na_row <- apply(.data, 1, function(x) any(is.na(x)))
  na_col <- apply(.data, 2, function(x) sum(is.na(x)))
  
  info_class <- get_class(.data)
  
  division_metric <- c("size", "size", "size", "size", 
                    "missing", "missing", "missing", "missing",
                    "data_type", "data_type", "data_type", "data_type", "data_type")
  name_metric <- c("observations", "variables", "values", "memory_size",
                   "complete_obs", "missing_obs", "missing_vars", "missing_values",
                   "numerics", "integers", "factors", "characters", "others")
  
  result <- data.frame(division = division_metric,
                       metrics = name_metric,
                       value = c(n_row, n_col, n_row * n_col, size,
                                 sum(complete), sum(na_row >= 1), 
                                 sum(na_col >= 1), sum(na_col),
                                 sum(info_class$class == "numeric"),
                                 sum(info_class$class == "integer"),
                                 sum(info_class$class == "factor"),
                                 sum(info_class$class == "character"),
                                 sum(!info_class$class %in% 
                                       c("numeric", "integer", "factor", "character"))
                                 )
                       )
  
  attr(result, "na_col") <- na_col
  attr(result, "info_class") <- info_class

  class(result) <- append("overview", class(result))

  result
}


#' Summarizing overview information
#'
#' @description print and summary method for "overview" class.
#' @param object an object of class "overview", usually, a result of a call to overview().
#' @param ... further arguments passed to or from other methods.
#' @details
#' summary.overview() tries to be smart about formatting 14 information of overview.
#' @seealso \code{\link{overview}}, \code{\link{plot.overview}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' ov <- overview(carseats)
#' ov
#' 
#' summary(ov)
#' }
#' 
#' @method summary overview
#' @importFrom cli cat_rule cat_bullet cat_print
#' @export
summary.overview <- function(object, ...)  {
  nms <- c("Number of observations", 
           "Number of variables",
           "Number of values",
           "Size of located memory(bytes)",
           "Number of completed observations", 
           "Number of observations with N/A",
           "Number of variables with N/A",
           "Number of N/A",
           "Number of numeric variables",
           "Number of integer variables",
           "Number of factors variables",
           "Number of character variables",
           "Number of other variables") 
  nms <- format(nms)
  
  vls <- format(object$value, big.mark = ",")
  
  cli::cat_rule(
    left = "Data Scale",
    right = "",
    width = 60
  )

  info_scale <- paste0(nms[1:4], " :  ", vls[1:4])
  cli::cat_bullet(info_scale)
  cat("\n")
  
  cli::cat_rule(
    left = "Missing Data",
    right = "",
    width = 60
  )
  
  info_missing <- paste0(nms[5:8], " :  ", vls[5:8])
  cli::cat_bullet(info_missing)
  cat("\n")
  
  cli::cat_rule(
    left = "Data Type",
    right = "",
    width = 60
  )  

  info_type <- paste0(nms[9:13], " :  ", vls[9:13])
  cli::cat_bullet(info_type)
  cat("\n")
  
  cli::cat_rule(
    left = "Individual variables",
    right = "",
    width = 60
  )    
  
  info_class <- attr(object, "info_class")
  names(info_class) <- c("Variables", "Data Type")
  
  cli::cat_print(info_class)
}

#' Visualize Information for an "overview" Object
#'
#' @description
#' Visualize a plot by attribute of `overview` class.
#' Visualize the data type, number of observations, and number of missing values for each variable.
#'
#' @param x an object of class "overview", usually, a result of a call to overview().
#' @param order_type character. method of order of bars(variables).
#' @param ... further arguments to be passed from or to other methods.
#' @seealso \code{\link{overview}}, \code{\link{summary.overview}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' ov <- overview(carseats)
#' ov
#' 
#' summary(ov)
#' 
#' plot(ov)
#'
#' # sort by name of variables
#' plot(ov, order_type = "name")
#' 
#' # sort by data type of variables
#' plot(ov, order_type = "type")
#' }
#' 
#' @method plot overview
#' @import ggplot2
#' @import dplyr
#' @export
plot.overview <- function(x, order_type = c("none", "name", "type"), ...)  {
  info_class <- attr(x, "info_class")
  na_col <- attr(x, "na_col")
  
  raw <- data.frame(info_class, cnt = x[1, "value"], n_missing = na_col)
  
  raw <- raw %>% 
    mutate(variable = factor(variable, levels = variable))
  
  order_type <- match.arg(order_type)
  
  if (order_type == "name") {
    raw <- raw %>% 
      mutate(variable = as.character(variable))
  } else if (order_type == "type") {
    odr <- raw %>% 
      mutate(variable = as.character(variable)) %>% 
      arrange(class, variable) %>% 
      select(variable) %>% 
      pull()
    raw <- raw %>% 
      mutate(variable = factor(variable, levels = odr))  
  }
  
  raw %>% 
    ggplot() +
    geom_bar(aes(x = variable, y = cnt, fill = class), stat = "identity") +
    geom_point(aes(x = variable, y = n_missing, color = "Missing")) +
    geom_line(aes(x = variable, y = n_missing), group = 1) +
    scale_color_manual("Missing", values = 1, 
                      guide = guide_legend(order = 1),
                      labels = c("")) + 
    guides(fill = guide_legend(title = "Data Type", order = 1),
           color = guide_legend(order = 2)) + 
    ylab("Count") +
    xlab("Variables") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}


