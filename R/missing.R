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
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. 
#' @examples
#' # Generate data for the example
#' set.seed(123L)
#' jobchange2 <- jobchange[sample(nrow(jobchange), size = 1000), ]
#' 
#' # Visualize hcluster chart for variables with missing value.
#' plot_na_hclust(jobchange2)
#' 
#' # Change the main title.
#' plot_na_hclust(jobchange2, main = "Distribution of missing value")
#' 
#' # Not support typographic elements
#' plot_na_hclust(jobchange2, typographic = FALSE)
#' 
#' @importFrom stats hclust dist order.dendrogram as.dendrogram reorder
#' @import ggplot2
#' @import hrbrthemes
#' @export
#' 
plot_na_hclust <- function (x, main = NULL, col.left = "#009E73", 
                            col.right = "#56B4E9", typographic = TRUE,
                            base_family = NULL)
{
  N <- nrow(x)
  
  na_obs <- x %>% 
    apply(1, function(x) any(is.na(x)))
  na_obs <- which(na_obs)
  
  na_variable <- x %>% 
    apply(2, function(x) any(is.na(x))) 
  na_variable <- names(na_variable[na_variable == TRUE])
  
  x <- x[na_obs, ] %>% 
    select_at(na_variable) %>% 
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
  
  dframe <- get_melt(x)
  dframe <- dframe[dframe$value != 0, ]
  
  if (is.null(main)) 
    main = "Distribution of missing value by combination of variables"
  
  p <- ggplot(dframe, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill = value)) + 
    scale_fill_gradient(low = "grey", high = "#d18975") +
    geom_label(data = label_info, aes(x = 0, y = pos_y, label = na_cnt), 
               hjust = 1, fill = col.left, size = 3, colour = "white", 
               fontface = "bold") +
    geom_label(data = label_info, aes(x = nr + 2, y = pos_y, label = na_pct), 
               hjust = 0, fill = col.right, size = 3, colour = "white", 
               fontface = "bold") +
    labs(y = "Variables with missing values", 
         x = "Number of observations", title = main) +
    theme_grey(base_family = base_family) +    
    theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 11),
          legend.position = "none")
  
  if (typographic) {
    p <- p +
      theme_typographic(base_family) +
      theme(legend.position = "none",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
  }
  
  suppressWarnings(p)
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
#' The default values are Good: [0, 0.05], OK: (0.05, 0.1], NotBad: (0.1, 0.2], Bad: (0.2, 0.5], Remove: (0.5, 1].
#' @param main character. Main title.
#' @param col character. The color of line for display the cumulative percentage.
#' @param plot logical. If this value is TRUE then visualize plot. else if FALSE, return aggregate information about missing values.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. 
#' @examples
#' # Generate data for the example
#' set.seed(123L)
#' jobchange2 <- jobchange[sample(nrow(jobchange), size = 1000), ]
#' 
#' # Diagnose the data with missing_count using diagnose() function
#' library(dplyr)
#' 
#' jobchange2 %>% 
#'   diagnose %>% 
#'   arrange(desc(missing_count))
#' 
#' # Visualize pareto chart for variables with missing value.
#' plot_na_pareto(jobchange2)
#' 
#' # Visualize pareto chart for variables with missing value.
#' plot_na_pareto(jobchange2, col = "blue")
#' 
#' # Visualize only variables containing missing values
#' plot_na_pareto(jobchange2, only_na = TRUE)
#' 
#' # Display the relative frequency 
#' plot_na_pareto(jobchange2, relative = TRUE)
#' 
#' # Change the grade
#' plot_na_pareto(jobchange2, grade = list(High = 0.1, Middle = 0.6, Low = 1))
#' 
#' # Change the main title.
#' plot_na_pareto(jobchange2, relative = TRUE, only_na = TRUE, 
#'                main = "Pareto Chart for jobchange")
#'   
#' # Return the aggregate information about missing values.
#' plot_na_pareto(jobchange2, only_na = TRUE, plot = FALSE)
#' 
#' # Not support typographic elements
#' plot_na_pareto(jobchange2, typographic = FALSE)
#' 
#' @importFrom purrr map_int
#' @importFrom tibble enframe
#' @importFrom dplyr rename
#' @import ggplot2
#' @export
plot_na_pareto <- function (x, only_na = FALSE, relative = FALSE, main = NULL, col = "black",
                            grade = list(Good = 0.05, OK = 0.1, NotBad = 0.2, Bad = 0.5, Remove = 1),
                            plot = TRUE, typographic = TRUE, base_family = NULL)
{
  if (sum(is.na(x)) == 0) {
    stop("Data have no missing value.")
  }
  
  info_na <- purrr::map_int(x, function(x) sum(is.na(x))) %>% 
    tibble::enframe() %>% 
    dplyr::rename(variable = name, frequencies = value) %>% 
    arrange(desc(frequencies), variable) %>% 
    mutate(ratio = frequencies / nrow(x)) %>% 
    mutate(grade = cut(ratio, breaks = c(-1, unlist(grade)), labels = names(grade))) %>% 
    mutate(cumulative = cumsum(frequencies) / sum(frequencies) * 100) %>% 
    #mutate(variable = forcats::fct_reorder(variable, frequencies, .desc = TRUE)) 
    arrange(desc(frequencies)) %>% 
    mutate(variable = factor(variable, levels = variable))
  
  if (only_na) {
    info_na <- info_na %>% 
      filter(frequencies > 0)
    xlab <- "Variable Names with Missing Value"
  } else {
    xlab <- "All Variable Names"
  }
  
  if (relative) {
    info_na$frequencies <- info_na$frequencies / nrow(x)
    ylab <- "Relative Frequency of Missing Values"
  } else {
    ylab <- "Frequency of Missing Values"
  }
  
  if (is.null(main)) 
    main = "Pareto chart with missing values"
  
  scaleRight <- max(info_na$cumulative) / info_na$frequencies[1]
  
  if (!plot) {
    return(info_na)
  }
  
  labels_grade <- paste0(names(grade),paste0("\n(<=", unlist(grade) * 100, "%)"))
  n_pal <- length(labels_grade)
  
  if (n_pal <= 3) {
    pals <- c("#FFEDA0", "#FEB24C", "#F03B20")
  } else if (n_pal == 4) {
    pals <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C")
  } else if (n_pal == 5) {
    pals <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  } else if (n_pal == 6) {
    pals <- c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026")
  } else if (n_pal == 7) {    
    pals <- c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", 
              "#B10026")
  } else if (n_pal == 8) {
    pals <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", 
              "#E31A1C", "#B10026")
  } else if (n_pal >= 9) {
    pals <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", 
              "#E31A1C", "#BD0026", "#800026")
  }  
  
  p <- ggplot(info_na, aes(x = variable)) +
    geom_bar(aes(y = frequencies, fill = grade), color = "darkgray", stat = "identity") +
    geom_text(aes(y = frequencies, 
                  label = paste(round(ratio * 100, 1), "%")),
              position = position_dodge(width = 0.9), vjust = -0.25) + 
    geom_path(aes(y = cumulative / scaleRight, group = 1), 
              colour = col, size = 0.4) +
    geom_point(aes(y = cumulative / scaleRight, group = 1), 
               colour = col, size = 1.5) +
    scale_y_continuous(sec.axis = sec_axis(~.*scaleRight, name = "Cumulative (%)")) +
    labs(title = main, x = xlab, y = ylab) + 
    theme_grey(base_family = base_family) +    
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "top") +
    scale_fill_manual(values = pals, 
                      drop = FALSE,
                      name = "Missing Grade", 
                      labels = labels_grade)
  
  if (typographic) {
    p <- p +
      theme_typographic(base_family) +
      theme(legend.position = "top",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.title.y.right = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }
  
  suppressWarnings(p)
}


#' Plot the combination variables that is include missing value
#'
#' @description
#' Visualize the combinations of missing value across cases.
#'
#' @details 
#' The visualization consists of four parts.
#' The bottom left, which is the most basic, visualizes the case of cross(intersection)-combination. 
#' The x-axis is the variable including the missing value, and the y-axis represents the case of a combination of variables.
#' And on the marginal of the two axes, the frequency of the case is expressed as a bar graph. 
#' Finally, the visualization at the top right expresses the number of variables including missing values in the data set, 
#' and the number of observations including missing values and complete cases .
#' 
#' @param x data frames, or objects to be coerced to one.
#' @param only_na logical. The default value is FALSE. 
#' If TRUE, only variables containing missing values are selected for visualization. 
#' If FALSE, included complete case.
#' @param n_intersacts integer. Specifies the number of combinations of variables including missing values. 
#' The combination of variables containing many missing values is chosen first.
#' @param n_vars integer. Specifies the number of variables that contain missing values to be visualized. 
#' The default value is NULL, which visualizes variables containing all missing values. 
#' If this value is greater than the number of variables containing missing values, 
#' all variables containing missing values are visualized. Variables containing many missing values are chosen first.
#' @param main character. Main title.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. 
#' @examples
#' # Generate data for the example
#' set.seed(123L)
#' jobchange2 <- jobchange[sample(nrow(jobchange), size = 1000), ]
#' 
#' # Visualize the combination variables that is include missing value.
#' plot_na_intersect(jobchange2)
#' 
#' # Diagnose the data with missing_count using diagnose() function
#' library(dplyr)
#' 
#' jobchange2 %>% 
#'   diagnose %>% 
#'   arrange(desc(missing_count))
#' 
#' # Visualize the combination variables that is include missing value
#' plot_na_intersect(jobchange2)
#' 
#' # Visualize variables containing missing values and complete case
#' plot_na_intersect(jobchange2, only_na = FALSE)
#' 
#' # Using n_vars argument
#' plot_na_intersect(jobchange2, n_vars = 5) 
#' 
#' # Using n_intersects argument
#' plot_na_intersect(jobchange2, only_na = FALSE, n_intersacts = 7)
#' 
#' # Not allow typographic elements
#' plot_na_intersect(jobchange2, typographic = FALSE)
#' 
#' @importFrom purrr map_int
#' @importFrom tibble enframe
#' @importFrom gridExtra grid.arrange
#' @importFrom utils head
#' @import ggplot2
#' @import hrbrthemes
#' @import dplyr
#' @export
plot_na_intersect <- function (x, only_na = TRUE, n_intersacts = NULL, 
                               n_vars = NULL, main = NULL, typographic = TRUE,
                               base_family = NULL)
{
  N <- nrow(x)
  
  if (sum(is.na(x)) == 0) {
    stop("Data have no missing value.")
  }
  
  if (only_na) {
    na_obs <- x %>% 
      apply(1, function(x) any(is.na(x))) %>% 
      which()
    
    x <- x[na_obs, ]
  } 
  
  marginal_var <- purrr::map_int(x, function(x) sum(is.na(x))) %>% 
    tibble::enframe(name = "name_var", value = "n_var") %>% 
    filter(n_var > 0) %>% 
    arrange(desc(n_var)) %>% 
    mutate(Var1 = seq(name_var))
  
  N_var_na <- nrow(marginal_var)
  
  if (!is.null(n_vars)) {
    marginal_var <- marginal_var %>% 
      head(n = n_vars)
  }
  
  na_variable <- marginal_var$name_var
  
  if (length(na_variable) == 1) {
    stop("Supported only when the number of variables including missing values is 2 or more.")
  }  
  
  x <- x %>% 
    select_at(vars(na_variable)) %>% 
    is.na() %>% 
    as.data.frame() %>% 
    group_by_at(na_variable) %>% 
    tally() %>% 
    arrange(desc(n))
  
  N_na <- sum(x$n[which(apply(select(x, -n), 1, sum) > 0)])
  
  if (!is.null(n_intersacts)) {
    if (n_intersacts < nrow (x)) {
      x <- x[seq(n_intersacts), ]
      x <- x[, which(apply(x, 2, function(x) sum(x) > 0))]
      
      marginal_var <- marginal_var %>% 
        filter(name_var %in% names(x)) %>% 
        mutate(Var1 = seq(name_var))
      
      na_variable <- setdiff(names(x), "n")
    }  
  }
  
  dframe <- get_melt(x) %>% 
    filter(value > 0) %>% 
    filter(!Var1 %in% c("n")) %>% 
    mutate(Var1 = as.numeric(Var1))
  
  flag <- x %>% 
    select(-n) %>% 
    apply(1, function(x) factor(ifelse(sum(x), "#F8766D", "#00BFC4")))
  
  marginal_obs <- data.frame(Var2 = seq(x$n), n_obs = x$n)
  
  N_complete <- N - N_na
  
  breaks <- pretty(marginal_obs$n_obs)
  
  if (is.null(main)) 
    main = "Missing with intersection of variables"
  
  # Create center plot
  body <- ggplot(dframe, aes(x = Var1, y = Var2)) + 
    geom_tile(aes(fill = value), color = "black", size = 0.5) + 
    scale_fill_gradient(low = "grey", high = "red") +
    scale_x_continuous(breaks = seq(length(na_variable)), 
                       labels = na_variable,
                       limits = c(0, length(na_variable)) + 0.5) +
    scale_y_continuous(breaks = seq(nrow(marginal_obs)), 
                       labels = marginal_obs$Var2,
                       limits = c(0, nrow(marginal_obs)) + 0.5) +    
    xlab("Variables")
  
  # Create plot of top
  top <- ggplot(marginal_var, aes(x = Var1, y = n_var)) +
    geom_col(fill = "#69b3a2", color = "darkgray") +
    ylab("Frequency")
  
  # for display the axis label
  max_char <- max(nchar(na_variable))
  
  formula <- paste0("%", max_char , "s")
  breaks_label <- sprintf(formula, breaks)
  
  # Create plot of right
  right <- ggplot(marginal_obs, aes(x = Var2, y = n_obs)) +
    geom_col(fill = "#69b3a2", color = "darkgray") +
    coord_flip() +
    ylab("Frequency")
  
  legend_txt <- paste(c("#Missing Vars:", "#Missing Obs:", "#Complete Obs:"), 
                      c(N_var_na, N_na, N_complete))
  legend_df <- data.frame(x = c(0.1, 0.1, 0.1), y = c(0.1, 0.3, 0.5), 
                          txt = factor(legend_txt, labels = legend_txt))
  
  # Create information plot
  blank <- ggplot(data = legend_df, aes(x, y, label = txt)) + 
    geom_label(fill = c("steelblue", "#F8766D", "#F8766D"), colour = "white", 
               fontface = "bold", size = 3, hjust = 0) + 
    xlim(c(0, 1)) +
    ylim(c(0, 0.6)) +
    theme(legend.position = "none",
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
  
  if (typographic) {
    top <- top +
      theme_typographic(base_family) +
      scale_x_continuous(breaks = seq(marginal_var$Var1), 
                         labels = marginal_var$n_var,
                         limits = c(0, length(na_variable)) + 0.5) +
      scale_y_continuous(position = "left") + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = margin(10, 10, 0, 10))
    
    body <- body +
      theme_typographic(base_family) +
      theme(legend.position = "none",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = margin(0, 10, 30, 10))
      
    right <- right +
      theme_typographic(base_family) +
      scale_x_continuous(breaks = seq(marginal_obs$Var2), 
                         labels = marginal_obs$n_obs,
                         limits = c(0, nrow(marginal_obs)) + 0.5) +    
      scale_y_continuous(breaks = breaks, 
                         labels = breaks_label,
                         limits = range(c(0, breaks))) +    
      theme(axis.title.y = element_blank(),
            axis.title.x = element_text(color = "transparent"),
            axis.text.x = element_text(color = "transparent"),
            plot.margin = margin(0, 10, 30, 0))
    
    if (is.null(base_family)) {
      base_family <- get_font_family()
    }
    
    main <- grid::textGrob(main, gp = grid::gpar(fontfamily = base_family, 
                                                 fontsize = 18, font = 2),
                          x = unit(0.075, "npc"), just = "left")
    
  } else {
    body <- body +
      theme_grey(base_family = base_family) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                       family = "mono"),
            axis.title.y = element_blank(), axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none")
    
    top <- top +
      scale_y_continuous(position = "right") + 
      scale_x_continuous(breaks = seq(marginal_var$Var1), 
                         labels = marginal_var$n_var,
                         limits = c(0, length(na_variable)) + 0.5) +
      theme_grey(base_family = base_family) +
      theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(),
            axis.title.y = element_blank(), axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
      
    right <- right +
      scale_x_continuous(breaks = seq(marginal_obs$Var2), 
                         labels = marginal_obs$n_obs,
                         limits = c(0, nrow(marginal_obs)) + 0.5) +    
      scale_y_continuous(breaks = breaks, 
                         labels = breaks_label,
                         limits = range(c(0, breaks))) +
      theme_grey(base_family = base_family) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, 
                                       family = "mono", color = "transparent"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "none",
            axis.title.x = element_text(color = "transparent"))
  }
  
  suppressWarnings(gridExtra::grid.arrange(top, blank, body, right,
               ncol = 2, nrow = 2, widths = c(9, 2), heights = c(1, 5),
               top = main))
} 
