#' @rdname plot_bar_category.data.frame
#' @export
plot_bar_category <- function(.data, ...) {
  UseMethod("plot_bar_category", .data)
}

#' Plot bar chart  of categorical variables 
#'
#' @description The plot_bar_category() to visualizes the distribution of 
#' categorical data by level or relationship to specific numerical data by level.
#'
#' @details The distribution of categorical variables can be understood by 
#' comparing the frequency of each level. The frequency table helps with this. 
#' As a visualization method, a bar graph can help you understand 
#' the distribution of categorical data more easily than a frequency table.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_bar_category() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#'
#' @param top an integer. Specifies the upper top rank to extract.
#' Default is 10.
#' @param add_character logical. Decide whether to include text variables in the
#' diagnosis of categorical data. The default value is TRUE, which also includes character variables.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' set.seed(123)
#' carseats$Test <- sample(LETTERS[1:15], 400, replace = TRUE)
#' carseats$Test[1:30] <- NA
#'
#' # Visualization of all numerical variables
#' plot_bar_category(carseats)
#'
#' # Select the variable to diagnose
#' plot_bar_category(carseats, "ShelveLoc", "Urban")
#' plot_bar_category(carseats, -ShelveLoc, -Urban)
#'
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Plot of all categorical variables
#' carseats %>%
#'   plot_bar_category()
#'
#' # Visualize just 7 levels of top frequency
#' carseats %>%
#'   plot_bar_category(top = 7)
#'    
#' # Visualize only factor, not character
#' carseats %>%
#'   plot_bar_category(add_character = FALSE) 
#'   
#' @method plot_bar_category data.frame
#' 
#' @importFrom purrr map
#' @importFrom gridExtra grid.arrange
#' @export
plot_bar_category.data.frame <- function(.data, ..., top = 10, add_character = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  plot_bar_category_impl(.data, vars, top, add_character)
}

plot_bar_category_impl <- function(df, vars, top, add_character) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  if (add_character)
    nm_factor <- find_class(df[, vars], type = "categorical2", index = FALSE)
  else
    nm_factor <- find_class(df[, vars], type = "categorical", index = FALSE)
  
  if (length(nm_factor) == 0) {
    message("There is no categorical variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_tally <- function(data, var, top = 10) {
    raws <- data %>% 
      select(levels = var) %>% 
      group_by(levels) %>% 
      tally() %>% 
      mutate(na_flag = is.na(levels)) %>% 
      mutate(menas = round(sum(n) / (length(n) - sum(na_flag)))) %>%     
      arrange(na_flag, desc(n)) %>%     
      mutate(levels = factor(levels, levels = levels)) %>% 
      mutate(rank = row_number())
    
    tops <- raws %>% 
      filter(rank <= top) %>% 
      filter(!na_flag)
    
    others <- raws %>% 
      filter(rank > top) %>% 
      filter(!na_flag) %>% 
      summarise(levels = "otherwise_top",
                n = sum(n),
                na_flag = FALSE,
                menas = 0,
                rank = top + 1) %>% 
      filter(n > 0)
    
    missing <- raws %>% 
      filter(na_flag) %>% 
      mutate(rank = top + 2)
    
    rbind(tops, others,missing) %>% 
      mutate(flag = ifelse(na_flag, "Missing", 
                           ifelse(menas == 0, "OtherTop", "Tops")))
  }
  
  def_colors <- c("Tops" = "#ff7f0e", "OtherTop" = "#1f77b4",
                  "Missing" = "grey50")
  
  plist <- purrr::map(nm_factor, function(x) {
      return(data.frame(variables = x, 
                        get_tally(df, x, top)))
    }) %>% 
    lapply(function(data) {
      ggplot(data, aes(x = levels, y = n)) +
        geom_bar(aes(fill = flag), stat = "identity") +
        scale_colour_manual(values = def_colors,
                            aesthetics = c("colour", "fill")) +
        geom_hline(yintercept = max(data$menas), linetype = "dashed",
                   col = "blue") +
        facet_wrap(~ variables) + 
        scale_x_discrete(limits = rev) + 
        coord_flip() +
        xlab("") + 
        ylab("") + 
        theme(legend.position = "nome")
    })
  
  n <- length(plist)
  n_row <- floor(sqrt(n))
  
  do.call("grid.arrange", c(plist, nrow = n_row))
}
