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
#'     \item duplicated : indicators of related to duplicated value 
#'     \item missing : indicators of related to missing value 
#'     \item data_type : indicators of related to data type 
#'   }
#'   \item metrics : name of metrics.
#'   \itemize{
#'     \item observations : number of observations (number of rows)
#'     \item variables : number of variables (number of columns)
#'     \item values : number of values (number of cells. rows * columns)
#'     \item memory size : an estimate of the memory that is being used to store an R object.
#'     \item duplicate observation: number of duplicate cases(observations).
#'     \item complete observation : number of complete cases(observations). i.e., have no missing values.
#'     \item missing observation : number of observations that has missing values.
#'     \item missing variables : number of variables that has missing values.
#'     \item missing values : number of values(cells) that has missing values.
#'     \item numerics : number of variables that is data type is numeric.
#'     \item integers : number of variables that is data type is integer.
#'     \item factors : number of variables that is data type is factor.
#'     \item characters : number of variables that is data type is character.
#'     \item Dates : number of variables that is data type is Date.
#'     \item POSIXcts : number of variables that is data type is POSIXct.
#'     \item others : number of variables that is not above.
#'   }
#'   \item value : value of metrics.
#' }
#' 
#' Attributes of overview class is as follows.:
#' \itemize{
#'   \item duplicated : the index of duplicated observations.
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
#' ov <- overview(jobchange)
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
  
  duplicated <- which(duplicated(.data))
  
  na_row <- apply(.data, 1, function(x) any(is.na(x)))
  na_col <- apply(.data, 2, function(x) sum(is.na(x)))
  
  info_class <- get_class(.data)
  
  division_metric <- c("size", "size", "size", "size", 
                       "duplicated", "missing", "missing", "missing", "missing", 
                       "data type", "data type", "data type", "data type", 
                       "data type", "data type", "data type")
  name_metric <- c("observations", "variables", "values", "memory size",
                   "duplicate observation", "complete observation", 
                   "missing observation", "missing variables", "missing values", 
                   "numerics", "integers", "factors/ordered", "characters", 
                   "Dates", "POSIXcts", "others")
  
  result <- data.frame(
    division = division_metric,
    metrics = name_metric,
    value = c(n_row, n_col, n_row * n_col, size,
              length(duplicated),
              sum(complete), sum(na_row >= 1), 
              sum(na_col >= 1), sum(na_col),
              sum(info_class$class == "numeric"),
              sum(info_class$class == "integer"),
              sum(info_class$class %in% c("factor", "ordered")),
              sum(info_class$class == "character"),
              sum(info_class$class == "Date"),     
              sum(info_class$class == "POSIXct"),                   
              sum(!info_class$class %in% 
                    c("numeric", "integer", "factor", "ordered", "character",
                      "Date", "POSIXct"))
              ),
    stringsAsFactors = FALSE
  )
  
  attr(result, "duplicated") <- duplicated
  attr(result, "na_col") <- na_col
  attr(result, "info_class") <- info_class

  class(result) <- append("overview", class(result))

  result
}


#' Summarizing overview information
#'
#' @description print and summary method for "overview" class.
#' @param object an object of class "overview", usually, a result of a call to overview().
#' @param html logical. whether to send summary results to html. The default is FALSE, 
#' which prints to the R console.
#' @param ... further arguments passed to or from other methods.
#' @details
#' summary.overview() tries to be smart about formatting 14 information of overview.
#' @seealso \code{\link{overview}}, \code{\link{plot.overview}}.
#' @examples
#' \donttest{
#' ov <- overview(jobchange)
#' ov
#' 
#' summary(ov)
#' }
#' 
#' @method summary overview
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
summary.overview <- function(object, html = FALSE, ...)  {
  nms <- c("Number of observations", 
           "Number of variables",
           "Number of values",
           "Size of located memory(bytes)",
           "Number of duplicated observations",            
           "Number of completed observations", 
           "Number of observations with NA",
           "Number of variables with NA",
           "Number of NA",
           "Number of numeric variables",
           "Number of integer variables",
           "Number of factors variables",
           "Number of character variables",
           "Number of Date variables",  
           "Number of POSIXct variables",             
           "Number of other variables") 
  
  nms <- format(nms)
  
  line_break <- function(html = FALSE) {
    if (!html) {
      cat("\n")
    } else {
      cat("<br>")      
    }  
  }
  
  vls <- format(object$value, big.mark = ",")
  
  N <- object$value[1]
  n_dup <- object$value[5]
  n_na <- object$value[7]
  
  p_dup <- paste0("(", round(n_dup / N * 100, 2), "%)")
  p_na <- paste0("(", round(n_na / N * 100, 2), "%)")
  
  vls[5] <- paste(vls[5], p_dup)
  vls[7] <- paste(vls[7], p_na)
  
  if (!html) {
    cat_rule(
      left = "Data Scale",
      right = "",
      width = 60
    )
  } else {
    cat_rule(
      left = "Data Scale",
      right = "",
      width = 60
    ) %>% 
      paste("<br>") %>% 
      cat()
  }

  info_scale <- paste0(nms[1:4], " :  ", vls[1:4])
  
  if (html) {
    info_scale <- paste(info_scale, "<br>")
  }
  
  cat_bullet(info_scale)
  line_break()
  
  if (!html) {
    cat_rule(
      left = "Duplicated Data",
      right = "",
      width = 60
    )
  } else {
    cat_rule(
      left = "Duplicated Data",
      right = "",
      width = 60
    ) %>% 
      paste("<br>") %>% 
      cat()
  }
  
  duplicated <- paste0(nms[5], " :  ", vls[5])
  
  if (html) {
    duplicated <- paste(duplicated, "<br>")
  }
  
  cat_bullet(duplicated)
  line_break()
  
  if (!html) {
    cat_rule(
      left = "Missing Data",
      right = "",
      width = 60
    )
  } else {
    cat_rule(
      left = "Missing Data",
      right = "",
      width = 60
    ) %>% 
      paste("<br>") %>% 
      cat()
  }
  
  info_missing <- paste0(nms[6:9], " :  ", vls[6:9])
  if (html) {
    info_missing <- paste(info_missing, "<br>")
  }
  
  cat_bullet(info_missing)
  line_break()
  
  if (!html) {
    cat_rule(
      left = "Data Type",
      right = "",
      width = 60
    )  
  } else {
    cat_rule(
      left = "Data Type",
      right = "",
      width = 60
    ) %>% 
      paste("<br>") %>% 
      cat()
  }  

  info_type <- paste0(nms[10:16], " :  ", vls[10:16])
  if (html) {
    info_type <- paste(info_type, "<br>")
  }  
  
  cat_bullet(info_type)
  line_break()
  
  if (!html) {
    cat_rule(
      left = "Individual variables",
      right = "",
      width = 60
    ) 
  } else {
    cat_rule(
      left = "Individual variables",
      right = "",
      width = 60
    ) %>% 
      paste("<br>") %>% 
      cat()
  }    
  
  info_class <- attr(object, "info_class")
  names(info_class) <- c("Variables", "Data Type")
  
  if (!html) {
    print(info_class)
  } else {
    info_class %>% 
      knitr::kable(format = "html")%>% 
      kableExtra::kable_styling(full_width = FALSE, font_size = 15, position = "left") 
  }  
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
#' ov <- overview(jobchange)
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


