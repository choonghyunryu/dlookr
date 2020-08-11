#' Combination chart for missing value
#'
#' @description
#' Visualize distribution of missing value by combination of variables.
#'
#' @details Rows are variables containing missing values, and columns are observations. 
#' These data structures were grouped into similar groups by applying hclust. 
#' So, it was made possible to visually examine how the missing values are distributed 
#' for each combination of variables.
#' 
#' @param x data frames, or objects to be coerced to one.
#' @param main character. Main title.
#' @param col.left character. The color of left legend that is frequency of NA. default is "#009E73".
#' @param col.right character. The color of right legend that is percentage of NA. default is "#56B4E9".
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' 
#' # Visualize pareto chart for variables with missing value.
#' plot_na_hclust(carseats)
#' plot_na_hclust(airquality)
#'   
#' 
#' # Visualize pareto chart for variables with missing value.
#' plot_na_hclust(mice::boys)
#' 
#' # Change the main title.
#' plot_na_hclust(mice::boys, main = "Distribution of missing value")
#' 
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
#' 
plot_na_hclust <- function (x, main = NULL, col.left = "#009E73", col.right = "#56B4E9")
{
  N <- nrow(x)
  
  na_obs <- x %>% 
    apply(1, function(x) any(is.na(x)))
  na_obs <- which(na_obs)
  
  na_variable <- x %>% 
    apply(2, function(x) any(is.na(x))) 
  na_variable <- names(na_variable[na_variable == TRUE])
  
  x <- x[na_obs, ] %>% 
    select_at(vars(na_variable)) %>% 
    is.na() %>% 
    as.matrix() + 0
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  if (nr <= 1 || nc <= 1) 
    stop("'x' must have at least 2 rows and 2 columns")
  
  mean_row <- rowMeans(x, na.rm = TRUE)
  mean_col <- colMeans(x, na.rm = TRUE)
  
  dendro_row <- hclust(dist(x)) %>% 
    as.dendrogram() %>% 
    reorder(mean_row)
  
  if (nr != length(idx_row <- order.dendrogram(dendro_row))) 
    stop("row dendrogram ordering gave index of wrong length")
  
  dendro_col <- hclust(dist(t(x))) %>% 
    as.dendrogram() %>% 
    reorder(mean_col)
  
  if (nc != length(idx_col <- order.dendrogram(dendro_col))) 
    stop("column dendrogram ordering gave index of wrong length")
  
  x <- x[rev(idx_row), idx_col]
  
  dimnames(x)[[1]] <- seq(nr)
  
  na_cnt <- colSums(x)
  label_info <- data.frame(pos_x = 0, 
                           pos_y = seq(na_cnt), 
                           na_cnt = na_cnt,
                           na_pct = paste0(round(na_cnt / N * 100, 1), "%"))
  
  dframe <- reshape2::melt(t(x))
  dframe <- dframe[dframe$value != 0, ]
  
  if (is.null(main)) 
    main = "Distribution of missing value by combination of variables"
  
  ggplot(dframe, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill = value)) + 
    scale_fill_gradient(low = "grey", high = "red") +
    geom_label(data = label_info, aes(x = 0, y = pos_y, label = na_cnt), 
               hjust = 1, fill = col.left, size = 3, colour = "white", 
               fontface = "bold") +
    geom_label(data = label_info, aes(x = nr + 2, y = pos_y, label = na_pct), 
               hjust = 0, fill = col.right, size = 3, colour = "white", 
               fontface = "bold") +
    labs(y = "Variables containing missing values", 
         x = "Observations", title = main) +
    theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 11),
          legend.position = "none")
}


#' Pareto chart for missing value
#'
#' @description
#' Visualize pareto chart for variables with missing value.
#'
#' @param x data frames, or objects to be coerced to one.
#' @param only_na logical. The default value is FALSE. 
#' If TRUE, only variables containing missing values are selected for visualization. 
#' If FALSE, all variables are included.
#' @param relative logical. If this argument is TRUE, it sets the unit of the left y-axis to relative frequency. 
#' In case of FALSE, set it to frequency.
#' @param grade list. Specifies the cut-off to set the grade of the variable according to the ratio of missing values.
#' The default values are Good: [0, 0.05], OK: (0.05, 0.4], Bad: (0.4, 0.8], Remove: (0.8, 1].
#' @param main character. Main title.
#' @param col character. The color of line for display the cumulative percentage.
#' @param plot logical. If this value is TRUE then visualize plot. else if FALSE, return aggregate information about missing values.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Diagnose the data with missing_count using diagnose() function
#' library(dplyr)
#' carseats %>% 
#'   diagnose %>% 
#'   arrange(desc(missing_count))
#' 
#' # Visualize pareto chart for variables with missing value.
#' plot_na_pareto(carseats)
#' plot_na_pareto(airquality)
#'   
#' # Diagnose the data with missing_count using diagnose() function
#' mice::boys %>% 
#'   diagnose %>% 
#'   arrange(desc(missing_count))
#' 
#' # Visualize pareto chart for variables with missing value.
#' plot_na_pareto(mice::boys, col = "darkorange")
#' 
#' # Visualize only variables containing missing values
#' plot_na_pareto(mice::boys, only_na = TRUE)
#' 
#' # Display the relative frequency 
#' plot_na_pareto(mice::boys, relative = TRUE)
#' 
#' # Change the grade
#' plot_na_pareto(mice::boys, grade = list(High = 0.1, Middle = 0.6, Low = 1))
#' 
#' # Change the main title.
#' plot_na_pareto(mice::boys, relative = TRUE, only_na = TRUE, 
#' main = "Pareto Chart for mice::boys")
#' 
#' # Return the aggregate information about missing values.
#' plot_na_pareto(mice::boys, only_na = TRUE, plot = FALSE)
#' 
#' @importFrom purrr map_int
#' @importFrom tibble enframe
#' @importFrom dplyr rename
#' @importFrom forcats fct_reorder
#' @import ggplot2
#' @export
plot_na_pareto <- function (x, only_na = FALSE, relative = FALSE, main = NULL, col = "black",
                            grade = list(Good = 0.05, OK = 0.4, Bad = 0.8, Remove = 1),
                            plot = TRUE)
{
  if (sum(is.na(x)) == 0) {
    stop("Data have no missing value.")
  }
  
  info_na <- purrr::map_int(x, function(x) sum(is.na(x))) %>% 
    tibble::enframe() %>% 
    dplyr::rename(variable = name, frequency = value) %>% 
    arrange(desc(frequency), variable) %>% 
    mutate(ratio = frequency / nrow(x)) %>% 
    mutate(grade = cut(ratio, breaks = c(-1, unlist(grade)), labels = names(grade))) %>% 
    mutate(cumulative = cumsum(frequency) / sum(frequency) * 100) %>% 
    mutate(variable = forcats::fct_reorder(variable, frequency, .desc = TRUE)) 
  
  if (only_na) {
    info_na <- info_na %>% 
      filter(frequency > 0)
    xlab <- "Variable Names with Missing Value"
  } else {
    xlab <- "All Variable Names"
  }
  
  if (relative) {
    info_na$frequency <- info_na$frequency / nrow(x)
    ylab <- "Relative Frequency of Missing Values"
  } else {
    ylab <- "Frequency of Missing Values"
  }
  
  if (is.null(main)) 
    main = "Pareto chart of variables with missing values"
  
  scaleRight <- max(info_na$cumulative) / info_na$frequency[1]
  
  if (!plot) {
    return(info_na)
  }
  
  ggplot(info_na, aes(x = variable)) +
    geom_bar(aes(y = frequency, fill = grade), stat = "identity") +
    geom_path(aes(y = cumulative / scaleRight, group = 1), 
              colour = col, size = 0.4) +
    geom_point(aes(y = cumulative / scaleRight, group = 1), 
               colour = col, size = 1.5) +
    scale_y_continuous(sec.axis = sec_axis(~.*scaleRight, name = "Cumulative (%)")) +
    labs(title = main, x = xlab, y = ylab) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "top")
}

plot_na_intersect <- function (x, only_na = TRUE, main = NULL)
{
  #library(ggpubr)
  
  N <- nrow(x)
  
  if (only_na) {
    na_obs <- x %>% 
      apply(1, function(x) any(is.na(x))) %>% 
      which()
    
    x <- x[na_obs, ]
  } 

  
  na_variable <- x %>% 
    apply(2, function(x) any(is.na(x))) 
  na_variable <- names(na_variable[na_variable == TRUE])
  
  x <- x %>% 
    select_at(vars(all_of(na_variable))) %>% 
    is.na() %>% 
    as.data.frame() %>% 
    group_by_at(na_variable) %>% 
    tally() %>% 
    arrange(desc(n))
  
  dframe <- reshape2::melt(t(x))
  dframe <- dframe[dframe$value != 0, ]
  
  dframe <- dframe %>% 
    filter(!Var1 %in% c("n"))
  
  tmp <- apply(x, 1, sum)
  x$n <- x$n / (tmp - x$n) 
  
  marginal <- data.frame(Var2 = seq(x$n), n = x$n)
  
  dframe <- dframe %>% 
    left_join(marginal) 
  
  if (is.null(main)) 
    main = "Missing information for intersection of variables"
  
  body <- ggplot(dframe, aes(x = Var1, y = Var2)) + 
    geom_tile(aes(fill = value), color = "black", size = 0.5) + 
    coord_fixed() + 
    scale_fill_gradient(low = "grey", high = "red") +
    #labs(x = "Variables", 
    #     y = "Case of Intersection", title = main) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 11),
          legend.position = "none")
  
  right <- ggplot(dframe, aes(x = Var2, y = n)) +
    geom_col() +
    coord_flip()
  
  body + right 
}  

#plot_na_intersect(mice::boys) 
#plot_na_intersect(mice::boys, only_na = FALSE)
#plot_na_intersect(carseats) 
#plot_na_intersect(naniar::riskfactors) 
#plot_na_intersect(naniar::riskfactors, only_na = FALSE) 
  
