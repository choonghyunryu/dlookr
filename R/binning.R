#' Binning the Numeric Data
#'
#' @description The binning() converts a numeric variable to a categorization variable.
#'
#' @details This function is useful when used with the mutate/transmute
#' function of the dplyr package.
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param x numeric. numeric vector for binning.
#' @param nbins integer. number of intervals(bins). required. if missing, nclass.Sturges is used.
#' @param type character. binning method. Choose from "quantile", "equal", "pretty", "kmeans" and "bclust".
#' The "quantile" sets breaks with quantiles of the same interval.
#' The "equal" sets breaks at the same interval.
#' The "pretty" chooses a number of breaks not necessarily equal
#' to nbins using base::pretty function.
#' The "kmeans" uses stats::kmeans function to generate the breaks.
#' The "bclust" uses e1071::bclust function to generate the breaks using bagged clustering.
#' "kmeans" and "bclust" was implemented by classInt::classIntervals() function.
#' @param ordered logical. whether to build an ordered factor or not.
#' @param labels character. the label names to use for each of the bins.
#' @param approxy.lab logical. If TRUE, large number breaks are approximated to pretty numbers. 
#' If FALSE, the original breaks obtained by type are used.
#' @return An object of bins class.
#' Attributes of bins class is as follows.
#' \itemize{
#' \item class : "bins"
#' \item type : binning type, "quantile", "equal", "pretty", "kmeans", "bclust".
#' \item breaks : breaks for binning. the number of intervals into which x is to be cut.
#' \item levels : levels of binned value.
#' \item raw : raw data, numeric vector corresponding to x argument.
#' }
#' @seealso \code{\link{binning_by}}, \code{\link{print.bins}}, \code{\link{summary.bins}},
#'  \code{\link{plot.bins}}.
#' @export
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#' 
#' # Binning the platelets variable. default type argument is "quantile"
#' bin <- binning(heartfailure2$platelets)
#' # Print bins class object
#' bin
#' 
#' # Summarise bins class object
#' summary(bin)
#' 
#' # Plot bins class object
#' plot(bin)
#' 
#' # Using labels argument
#' bin <- binning(heartfailure2$platelets, nbins = 4,
#'               labels = c("LQ1", "UQ1", "LQ3", "UQ3"))
#' bin
#' 
#' # Using another type argument
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "equal")
#' bin
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "pretty")
#' bin
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "kmeans")
#' bin
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "bclust")
#' bin
#' 
#' x <- sample(1:1000, size = 50) * 12345679
#' bin <- binning(x)
#' bin
#' bin <- binning(x, approxy.lab = FALSE)
#' bin
#'
#' # extract binned results
#' extract(bin)
#' 
#' # -------------------------
#' # Using pipes & dplyr
#' # -------------------------
#' library(dplyr)
#'
#' # Compare binned frequency by death_event
#' heartfailure2 %>%
#'   mutate(platelets_bin = binning(heartfailure2$platelets) %>% 
#'            extract()) %>%
#'   group_by(death_event, platelets_bin) %>%
#'   summarise(freq = n()) %>%
#'   arrange(desc(freq)) %>%
#'   head(10)
#'  
#'  # Compare binned frequency by death_event using Viz
#'  heartfailure2 %>%
#'    mutate(platelets_bin = binning(heartfailure2$platelets) %>% 
#'             extract()) %>%
#'    target_by(death_event) %>% 
#'    relate(platelets_bin) %>% 
#'    plot()
#' }
#'  
#' @importFrom grDevices nclass.Sturges
#' @importFrom stats na.omit quantile
#' @export
binning <- function(x, nbins,
                    type = c("quantile", "equal", "pretty", "kmeans", "bclust"),
                    ordered = TRUE, labels = NULL, approxy.lab = TRUE) {
  if (is.factor(x))
    stop("x is categorical value")
  if (!is.numeric(x))
    stop("x is not numeric value")
  
  if (missing(nbins))
    nbins <- nclass.Sturges(x)
  if (nbins < 2)
    stop("nbins less than 2")
  
  nuniq <- length(unique(x))
  if (nuniq == 1)
    stop("There is only one unique value of the x.")
  
  type <- match.arg(type)
  
  if (nbins > nuniq) {
    msg <- "nbins is greater than the number of unique values for x.
    So we assign 'nbins' the number of unique values of x."
    warning(msg)
    
    nbins <- nuniq
  }
  
  if (nbins == nuniq) {
    msg <- "The type argument is replaced by equal because nbins has the
    same number of unique values for x."
    warning(msg)
    
    nbins <- nuniq
    
    sorted <- sort(unique(x))
    interval <- diff(sorted)
    
    breaks <- c(min(sorted) - interval[1] / 2,
                sorted[1:(nbins - 1)] + interval / 2,
                max(sorted) + interval[length(interval)] / 2)
    
    type <- "equal"
  } else if (type == "quantile") {
    breaks <- quantile(x, probs = seq(0, 1, by = 1 / nbins),
                       na.rm = TRUE, type = 8)
    breaks <- unique(breaks)
    breaks[1] <- min(x, na.rm = TRUE)
    breaks[length(breaks)] <- max(x, na.rm = TRUE)
    
    names(breaks) <- NULL
  } else if (type == "equal") {
    breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
                  length.out = (nbins + 1))
  } else if (type == "pretty") {
    breaks <- pretty(x = x, n = nbins)
  } else {
    if (!requireNamespace("classInt", quietly = TRUE)) {
      stop("Package \"classInt\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    xx <- na.omit(x)
    if (type == "bclust") {
      ci <- classInt::classIntervals(var = xx, n = nbins, style = type, verbose = FALSE)
    } else {
      ci <- classInt::classIntervals(var = xx, n = nbins, style = type)
    }
    
    breaks <- ci$brks
  }
  
  breaks <- unique(breaks)
  
  dig.lab <- breaks %>% 
    nchar() %>% 
    max() %>% 
    min(7)
  
  fct <- cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, 
             dig.lab = dig.lab)
  
  if (is.null(labels)) {
    pretty.lab <- levels(fct) %>% 
      gsub("[(\\[]|]", "", .) %>% 
      strsplit(",") %>% 
      sapply(c) %>% 
      as.numeric() %>% 
      unique()
    
    pretty.lab[1] <- min(x, na.rm = TRUE)
    pretty.lab[length(pretty.lab)] <- max(x, na.rm = TRUE)
    
    if (approxy.lab) {
      if (!all(pretty.lab == breaks)) {
        fct <- cut(x, breaks = pretty.lab, labels = levels(fct), include.lowest = TRUE)
      }
    } else {
      if (!all(pretty.lab == breaks)) {
        lab <- NULL
        for (i in 2:length(breaks)) {
          lab <- c(lab, paste0("(", prettyNum(breaks[i - 1]), ",", prettyNum(breaks[i]), "]"))
        }
        lab[1] <- sub("^\\(", "[", lab[1])
        
        levels(fct) <- lab
      }    
    } 
  }
  
  if (ordered == TRUE)
    fct <- ordered(fct, levels = levels(fct))
  
  results <- fct
  attr(results, "type") <- type
  attr(results, "breaks") <- breaks
  attr(results, "levels") <- levels(fct)
  attr(results, "raw") <- x
  class(results) <- append("bins", class(results))
  
  results
}


#' Summarizing Binned Variable
#'
#' @description summary method for "bins" and "optimal_bins".
#' @param object an object of "bins" and "optimal_bins",
#' usually, a result of a call to binning().
#' @param ... further arguments to be passed from or to other methods.
#' @details
#' print.bins() prints the information of "bins" and "optimal_bins" objects nicely.
#' This includes frequency of bins, binned type, and number of bins.
#' summary.bins() returns data.frame including frequency and relative frequency for each levels(bins).
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @return
#' The function summary.bins() computes and returns a data.frame of summary statistics of the
#' binned given in object. Variables of data frame is as follows.
#' \itemize{
#' \item levels : levels of factor.
#' \item freq : frequency of levels.
#' \item rate : relative frequency of levels. it is not percentage.
#' }
#' @seealso \code{\link{binning}}
#' @examples
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#'
#' # Binning the platelets variable. default type argument is "quantile"
#' bin <- binning(heartfailure2$platelets)
#'
#' # Print bins class object
#' bin
#'
#' # Summarize bins class object
#' summary(bin)
#' 
#' @method summary bins
#' @export
summary.bins <- function(object, ...) {
  tab <- table(object, exclude = NULL)
  data.frame(levels = names(tab),
             freq = as.numeric(tab),
             rate = as.numeric(tab) / length(object))
}

#' @param x an object of class "bins" and "optimal_bins",
#' usually, a result of a call to binning().
#' @param ... further arguments passed to or from other methods.
#' @rdname summary.bins
#' @method print bins
#' @export
print.bins <- function(x, ...) {
  cat("binned type: ", attr(x, "type"), "\n", sep = "")
  cat("number of bins: ", length(levels(x)), "\n", sep = "")
  
  print(table(x, exclude = NULL))
}

#' Visualize Distribution for a "bins" object
#'
#' @description
#' Visualize two plots on a single screen.
#' The plot at the top is a histogram representing the frequency of the level.
#' The plot at the bottom is a bar chart representing the frequency of the level.
#' 
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "bins", usually, a result of a call to binning().
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' @seealso \code{\link{binning}}, \code{\link{print.bins}}, \code{\link{summary.bins}}.
#' @examples
#' # Generate data for the example
#' \donttest{
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 20), "platelets"] <- NA
#'
#' # Binning the platelets variable. default type argument is "quantile"
#' bin <- binning(heartfailure2$platelets, nbins = 5)
#' plot(bin)
#' 
#' # Using another type arguments
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "equal")
#' plot(bin)
#' 
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "pretty")
#' plot(bin)
#' 
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "kmeans")
#' plot(bin)
#' 
#' bin <- binning(heartfailure2$platelets, nbins = 5, type = "bclust")
#' plot(bin)
#' }
#' 
#' @export
#' @method plot bins
#' @importFrom gridExtra grid.arrange
#' @import hrbrthemes
#' @import ggplot2
plot.bins <- function(x, typographic = TRUE, base_family = NULL, ...) {
  brks <- attr(x, "breaks")
  type <- attr(x, "type")
  levels <- attr(x, "levels")
  
  bins <- factor(x, levels = levels(x))
  n <- seq(levels)
  
  deltas <- diff(brks)
  height <- table(bins) / deltas / length(bins)
  data_top <- data.frame(x_start = brks[-length(brks)], x_end = brks[-1],
                         height = height)
  
  p_top <- data_top %>% 
    ggplot() + 
    geom_rect(aes(xmin = x_start, xmax = x_end,
                  ymin = 0, ymax = height), 
              fill = "#69b3a2", alpha = 0.8, color = "black") +
    xlim(min(brks), max(brks)) +
    labs(title = sprintf("Density of original data using '%s' method", type),
         y = "Density") +
    theme_grey(base_family = base_family)
  
  if (typographic) {
    p_top <- p_top +
      theme_typographic(base_family) 
  }
  
  data_bottom <- data.frame(bins = seq(levels), freq = as.integer(table(bins)) / length(bins))
  
  p_bottom <- data_bottom %>% 
    ggplot(aes(x = bins, y = freq)) + 
    geom_bar(stat = "identity", fill = "#404080", width = 0.8,
             alpha = 0.8, color = "black") +
    labs(title = sprintf("Relative frequency by bins using '%s' method", type),
         x = "", y = "Relative Frequency") +
    scale_x_continuous(breaks = seq(levels), 
                       labels = levels,
                       limits = c(0.5, length(levels) + 0.5)) +
    theme_grey(base_family = base_family) 
  
  if (typographic) {
    p_bottom <- p_bottom +
      theme_typographic(base_family) +
      theme(plot.margin = margin(5, 20, 10, 20))
  }
  
  p_bottom <- p_bottom +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  suppressWarnings(gridExtra::grid.arrange(p_top, p_bottom, nrow = 2, ncol = 1)) 
}


#' Optimal Binning for Scoring Modeling
#'
#' @description The binning_by() finding intervals for numerical variable
#' using optical binning. Optimal binning categorizes a numeric characteristic
#' into bins for ulterior usage in scoring modeling.
#'
#' @details This function is useful when used with the mutate/transmute
#' function of the dplyr package. And this function is implemented using
#' smbinning() function of smbinning package.
#'
#' @section attributes of "optimal_bins" class:
#' Attributes of the "optimal_bins" class that is as follows.
#' \itemize{
#' \item class : "optimal_bins".
#' \item levels : character. factor or ordered factor levels
#' \item type : character. binning method
#' \item breaks : numeric. breaks for binning
#' \item raw : numeric. before the binned the raw data
#' \item ivtable : data.frame. information value table
#' \item iv : numeric. information value
#' \item target : integer. binary response variable
#' }
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param .data a data frame.
#' @param y character. name of binary response variable(0, 1). 
#' The variable must contain only the integers 0 and 1 as element. 
#' However, in the case of factor having two levels, it is performed while type conversion is performed in the calculation process.
#' @param x character. name of continuous characteristic variable. At least 5 different values. and Inf is not allowed.
#' @param p numeric. percentage of records per bin. Default 5\% (0.05).
#' This parameter only accepts values greater that 0.00 (0\%) and lower than 0.50 (50\%).
#' @param ordered logical. whether to build an ordered factor or not.
#' @param labels character. the label names to use for each of the bins.
#' @return an object of "optimal_bins" class.
#' Attributes of "optimal_bins" class is as follows.
#' \itemize{
#' \item class : "optimal_bins".
#' \item type : binning type, "optimal".
#' \item breaks : numeric. the number of intervals into which x is to be cut.
#' \item levels : character. levels of binned value.
#' \item raw : numeric. raw data, x argument value.
#' \item ivtable : data.frame. information value table.
#' \item iv : numeric. information value.
#' \item target : integer. binary response variable.
#' }
#' @seealso \code{\link{binning}}, \code{\link{plot.optimal_bins}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#'
#' # optimal binning using character
#' bin <- binning_by(heartfailure2, "death_event", "creatinine")
#' 
#' # optimal binning using name
#' bin <- binning_by(heartfailure2, death_event, creatinine)
#' bin
#' 
#' # performance table
#' attr(bin, "performance")
#' 
#' # summary optimal_bins class
#' summary(bin)
#' 
#' # visualize all information for optimal_bins class
#' plot(bin)
#' 
#' # visualize WoE information for optimal_bins class
#' plot(bin, type = "WoE")
#' 
#' # visualize all information without typographic
#' plot(bin, typographic = FALSE)
#' 
#' # extract binned results
#' extract(bin) %>% 
#'   head(20)
#' }
#' 
#' @export
#' @importFrom tibble is_tibble
#' @importFrom partykit ctree ctree_control width
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @import dplyr
binning_by <- function(.data, y, x, p = 0.05, ordered = TRUE, labels = NULL) {
  y <- tidyselect::vars_select(names(.data), !! enquo(y))
  x <- tidyselect::vars_select(names(.data), !! enquo(x))
  
  if (tibble::is_tibble(.data)) {
    .data <- as.data.frame(.data)
  }
  
  uniq_y <- length(unique(.data[, y]))
  type_y <- class(.data[, y])[1]
  type_x <- class(.data[, x])[1]
  
  if (!is.data.frame(.data)) 
    stop("Data is not a data.frame.")
  
  if (!type_x %in% c("integer", "numeric")) 
    stop("x is not numeric value.")
  
  if (uniq_y != 2) {
    stop("The number of levels of the y variable is not 2.")
  }
  
  if (type_y %in% c("integer", "numeric")) {
    if (!all(unique(.data[, y]) %in% c(0, 1))) {
      stop("If y is a numeric variable, it can have only values of 0 and 1. (1: positive, 0: Negative)")
    }
  }
  
  if (type_y %in% c("factor", "ordered")) {
    yval <- levels(.data[, y])
    
    .data[, y] <- as.integer(.data[, y]) - 1
    msg <- sprintf("'%s' changed to 1 (positive) and '%s' changed to 0 (negative).", yval[2], yval[1])
    warning(paste("The factor y has been changed to a numeric vector consisting of 0 and 1.", msg, sep = "\n"))    
  }
  
  if (type_y %in% c("character")) {
    yval <- levels(factor(.data[, y]))
    
    .data[, y] <- as.integer(factor(.data[, y]))- 1
    msg <- sprintf("'%s' changed to 1 (positive) and '%s' changed to 0 (negative).", yval[2], yval[1])
    warning(paste("The character y has been changed to a numeric vector consisting of 0 and 1.", msg, sep = "\n"))    
  }
  
  if (any(is.infinite(.data[, x]))) 
    stop("x with an Inf. Replace by NA. or Remove the Inf.")
  
  if (p <= 0 | p > 0.5)
    stop("p must be, 0 < p <= 0.5")
  
  if (length(unique(.data[, x])) < 5) 
    stop("x must be number of unique values greater then 4.")
  
  ctree <- partykit::ctree(
    formula(paste("`", y,"`", " ~ ", "`", x, "`", sep = "")), 
    data = .data, na.action = na.exclude,
    control = partykit::ctree_control(
      minbucket = ceiling(round(p * nrow(.data)))
      )
    )
  
  bins <- width(ctree)
  if (bins < 2) {
    msg <- "No significant splits"
    
    results <- msg
    class(results) <- append("optimal_bins", class(results))
    
    warning(msg, "\n")
    return(results)
  }
  
  cutvct <- seq(ctree) %>%
    map_dbl(function(x) {
      breaks <- ctree[x]$node$split$breaks
      ifelse(is.null(breaks), NA, breaks)
    }) %>% 
    sort() 
  
  min_max <- range(.data[, x], na.rm = TRUE)
  cufoff <- c(min_max[1], cutvct, min_max[2])
  
  breaks <- cufoff
  tab <- table(breaks)
  
  dup <- as.numeric(names(tab)[which(tab == 2)])
  if (length(dup) > 0) {
    breaks <- unique(c(ifelse(min(breaks) == dup, dup - 1, dup + 1), breaks))
  }
  
  fct <- cut(.data[, x], breaks = breaks, labels = labels, include.lowest = TRUE)
  
  if (ordered == TRUE)
    fct <- ordered(fct)
  
  perf <- performance_bin(.data[, y], fct)
  
  results <- fct
  attr(results, "type") <- "optimal"
  attr(results, "breaks") <- breaks
  attr(results, "levels") <- levels(fct)
  attr(results, "raw") <- .data[, x]
  class(results) <- append("bins", class(results))
  attr(results, "performance") <- perf
  attr(results, "target") <- .data[, y]
  
  class(results) <- append("optimal_bins", class(results))
  
  results
}


#' Summarizing Performance for Optimal Bins
#'
#' @description summary method for "optimal_bins". summary metrics to evaluate the performance 
#' of binomial classification model.
#' @param object an object of class "optimal_bins", usually, a result of a call to binning_by().
#' @param ... further arguments to be passed from or to other methods.
#' @details
#' print() to print only binning table information of "optimal_bins" objects.
#' summary.performance_bin() includes general metrics and result of significance tests life follows.:
#' \itemize{
#'   \item Binning Table : Metrics by bins.
#'   \itemize{
#'     \item CntRec, CntPos, CntNeg, RatePos, RateNeg, Odds, WoE, IV, JSD, AUC.
#'   }
#'   \item General Metrics.
#'   \itemize{
#'     \item Gini index.
#'     \item Jeffrey's Information Value.
#'     \item Jensen-Shannon Divergence.
#'     \item Kolmogorov-Smirnov Statistics.
#'     \item Herfindahl-Hirschman Index.
#'     \item normalized Herfindahl-Hirschman Index.
#'     \item Cramer's V Statistics.
#'   } 
#'   \item Table of Significance Tests.
#' }
#' @return NULL.
#' @seealso \code{\link{binning_by}}, \code{\link{plot.optimal_bins}}
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#'
#' # optimal binning
#' bin <- binning_by(heartfailure2, "death_event", "creatinine")
#' bin
#'
#' # summary optimal_bins class
#' summary(bin)
#'
#' # performance table
#' attr(bin, "performance")
#'
#' # visualize all information for optimal_bins class
#' plot(bin)
#' 
#' # visualize WoE information for optimal_bins class
#' plot(bin, type = "WoE")
#' 
#' # visualize all information without typographic
#' plot(bin, typographic = FALSE)
#' 
#' # extract binned results
#' extract(bin) %>% 
#'   head(20)
#' }
#' 
#' @method summary optimal_bins
#' @export
summary.optimal_bins <- function(object, ...) {
  perf <- attr(object, "performance")
  
  summary(perf)
}

#' Visualize Distribution for an "optimal_bins" Object
#'
#' @description
#' It generates plots for understand distribution, frequency, bad rate, and weight of evidence using optimal_bins.
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "optimal_bins", usually, a result of a call to binning_by().
#' @param type character. options for visualization. Distribution ("dist"), Relateive Frequency ("freq"),
#' Positive Rate ("posrate"), and Weight of Evidence ("WoE"). and default "all" draw all plot.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param rotate_angle integer. specifies the rotation angle of the x-axis label. 
#' This is useful when the x-axis labels are long and overlap. 
#' The default is 0 to not rotate the label.
#' @param ... further arguments to be passed from or to other methods.
#' @seealso \code{\link{binning_by}}, \code{\link{summary.optimal_bins}}
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#'
#' # optimal binning using binning_by()
#' bin <- binning_by(heartfailure2, "death_event", "creatinine")
#' bin
#' 
#' # summary optimal_bins class.
#' summary(bin)
#'
#' # visualize all information for optimal_bins class
#' plot(bin)
#' 
#' # rotate the x-axis labels by 45 degrees so that they do not overlap.
#' plot(bin, rotate_angle = 45)
#' 
#' # visualize WoE information for optimal_bins class
#' plot(bin, type = "WoE")
#' 
#' # visualize all information with typographic
#' plot(bin)
#' }
#' 
#' @import ggplot2
#' @import hrbrthemes
#' @importFrom gridExtra grid.arrange
#' @export
#' @method plot optimal_bins
plot.optimal_bins <- function(x, type = c("all", "dist", "freq", "posrate", "WoE"), 
                              typographic = TRUE, base_family = NULL,
                              rotate_angle = 0, ...) {
  if (is.character(x)) {
    cat("binn is the optimal_bins object that can not be binned : \n",
        x, "\n", sep = "")
  } else {
    ivt <- attr(x, "performance") 
    
    type <- match.arg(type)
    
    if (type %in% c("all", "dist")) {
      p_dist <- data.frame(indicator = attr(x, "raw"), target = as.character(attr(x, "target"))) %>% 
        ggplot(aes(x = target, y= indicator, group = target)) +
        geom_boxplot(fill = "slategrey", color = "darkslategrey", width = 0.3) +
        coord_flip() +
        ggtitle("Distribution of target") +
        theme_grey(base_family = base_family) + 
        theme(axis.text.x = element_text(angle = rotate_angle, 
                                         vjust = 0.5, hjust = 0.5)) 
      
      if (typographic) {
        p_dist <- p_dist +
          theme_typographic(base_family) +
          theme(axis.text.x = element_text(angle = rotate_angle, 
                                           vjust = 0.5, hjust = 0.5)) 
        
        if (type %in% c("all")) {
          p_dist <- p_dist +
            theme(axis.title.y = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  plot.margin = margin(10, 10, 10, 10))
        } 
      }
    }
    
    if (type %in% c("all", "freq")) {
      p_freq <- ivt %>% 
        filter(!Bin %in% "Total") %>% 
        mutate(Bin = factor(Bin, levels = attr(x, "levels"))) %>% 
        mutate(PctRec = round(CntRec / sum(CntRec) * 100, 1)) %>% 
        mutate(add_pos = max(PctRec) * 0.1) %>% 
        ggplot(aes(x = Bin, y = PctRec, fill = Bin)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_brewer(na.value = "grey") +    
        geom_text(aes(label = PctRec, y = PctRec + add_pos)) +
        xlab("") +
        ylab("") +
        ggtitle("Percentage of frequency") +
        theme_grey(base_family = base_family) + 
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = rotate_angle, 
                                         vjust = 0.5, hjust = 0.5)) 
      
      if (typographic) {
        p_freq <- p_freq +
          theme_typographic(base_family) +
          theme(axis.text.x = element_text(angle = rotate_angle, 
                                           vjust = 0.5, hjust = 0.5)) 
        
        if (type %in% c("all")) {
          p_freq <- p_freq +
            theme(legend.position = "none",
                  plot.margin = margin(10, 10, 10, 10))
        } 
      }
    }
    
    if (type %in% c("all", "posrate")) {
      p_badrate <- ivt %>% 
        filter(!Bin %in% "Total") %>% 
        mutate(Bin = factor(Bin, levels = attr(x, "levels"))) %>% 
        mutate(PctPos = round(RatePos * 100, 1)) %>%         
        mutate(add_pos = max(PctPos) * 0.1) %>%         
        ggplot(aes(x = Bin, y = PctPos, fill = Bin)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_brewer(na.value = "grey") +
        geom_text(aes(label = PctPos, y = PctPos + add_pos)) +
        xlab("") +
        ylab("") +
        ggtitle("Percentage of positive") +
        theme_grey(base_family = base_family) + 
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = rotate_angle, 
                                         vjust = 0.5, hjust = 0.5)) 
      
      if (typographic) {
        p_badrate <- p_badrate +
          theme_typographic(base_family) +
          theme(axis.text.x = element_text(angle = rotate_angle, 
                                           vjust = 0.5, hjust = 0.5)) 
        
        if (type %in% c("all")) {
          p_badrate <- p_badrate +
            theme(legend.position = "none",
                  plot.margin = margin(10, 10, 10, 10))
        } 
      }
    }
    
    if (type %in% c("all", "WoE")) {
      p_woe <- ivt %>% 
        filter(!Bin %in% "Total") %>% 
        mutate(Bin = factor(Bin, levels = attr(x, "levels"))) %>%       
        mutate(add_pos = max(WoE) * 0.1) %>%  
        ggplot(aes(x = Bin, y = WoE, fill = Bin)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_brewer(na.value = "grey") +
        geom_text(aes(label = round(WoE, 2), y = WoE + add_pos)) +
        xlab("") +
        ylab("") +
        ggtitle("Weight of Evidence") +
        theme_grey(base_family = base_family) + 
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = rotate_angle, 
                                         vjust = 0.5, hjust = 0.5)) 
      
      if (typographic) {
        p_woe <- p_woe +
          theme_typographic(base_family) +
          theme(axis.text.x = element_text(angle = rotate_angle, 
                                           vjust = 0.5, hjust = 0.5)) 
        
        if (type %in% c("all")) {
          p_woe <- p_woe +
            theme(legend.position = "none",
                  plot.margin = margin(10, 10, 10, 10))
        }  
      }
    }
    
    if (type %in% c("all")) {
      p_dist <- p_dist +
        theme(plot.title = element_text(size = 15))
      
      p_freq <- p_freq +
        theme(plot.title = element_text(size = 15))
      
      p_badrate <- p_badrate +
        theme(plot.title = element_text(size = 15))
      
      p_woe <- p_woe +
        theme(plot.title = element_text(size = 15))  
      
      suppressWarnings(gridExtra::grid.arrange(p_dist, p_freq, p_badrate, p_woe, nrow = 2, ncol = 2)) 
    } else if (type %in% c("dist")) {
      suppressWarnings(p_dist)
    } else if (type %in% c("freq")) {
      suppressWarnings(p_freq)
    } else if (type %in% c("posrate")) {
      suppressWarnings(p_badrate)
    } else if (type %in% c("WoE")) {
      suppressWarnings(p_woe)
    }
  }  
}

#' @rdname extract.bins
#' @export
extract <- function(x) {
  UseMethod("extract", x)
}

#' Extract bins from "bins"
#'
#' @description The extract() extract binned variable from "bins", "optimal_bins" class object.
#' 
#' @details The "bins" and "optimal_bins" class objects use the summary() and plot() functions to diagnose 
#' the performance of binned results. This function is used to extract the binned result if you are satisfied 
#' with the result.
#'
#' @param x a bins class or optimal_bins class.
#' 
#' @return factor.
#' @seealso \code{\link{binning}}, \code{\link{binning_by}}.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#'
#' # optimal binning using binning_by()
#' bin <- binning_by(heartfailure2, "death_event", "creatinine")
#' bin
#'
#' # extract binning result
#' extract(bin) %>% 
#'   head(20)
#' 
#' @export
#' @method extract bins
extract.bins <- function(x) {
  object <- unclass(x)
  
  idx <- as.integer(object)
  levels <- attr(object,"levels")
  
  raw <- factor(levels[idx], levels = levels)
  class(raw) <- grep("bins", class(x), invert = TRUE, value = TRUE)
  
  raw
}


#' Diagnose Performance Binned Variable
#'
#' @description The performance_bin() calculates metrics to evaluate the performance of binned variable for 
#' binomial classification model.
#'
#' @details This function is useful when used with the mutate/transmute
#' function of the dplyr package.
#' 
#' @param y character or numeric, integer, factor. a binary response variable (0, 1). 
#' The variable must contain only the integers 0 and 1 as element. 
#' However, in the case of factor/character having two levels, it is performed while type conversion is performed in the calculation process.
#' @param x integer or factor, character. At least 2 different values. and Inf is not allowed.
#' @param na.rm logical. a logical indicating whether missing values should be removed.
#' @return an object of "performance_bin" class. vaue of data.frame is as follows.
#' \itemize{
#' \item Bin : character. bins.
#' \item CntRec : integer. frequency by bins.
#' \item CntPos : integer. frequency of positive  by bins.
#' \item CntNeg : integer. frequency of negative  by bins.
#' \item CntCumPos : integer. cumulate frequency of positive  by bins.
#' \item CntCumNeg : integer. cumulate frequency of negative  by bins.
#' \item RatePos : integer. relative frequency of positive  by bins.
#' \item RateNeg : integer. relative frequency of negative  by bins.
#' \item RateCumPos : numeric. cumulate relative frequency of positive  by bins.
#' \item RateCumNeg : numeric. cumulate relative frequency of negative  by bins.
#' \item Odds : numeric. odd ratio.
#' \item LnOdds : numeric. loged odd ratio.
#' \item WoE : numeric. weight of evidence.
#' \item IV : numeric. Jeffrey's Information Value.
#' \item JSD : numeric. Jensen-Shannon Divergence.
#' \item AUC : numeric. AUC. area under curve.
#' }
#' Attributes of "performance_bin" class is as follows.
#' \itemize{
#' \item names : character. variable name of data.frame with "Binning Table".
#' \item class : character. name of class. "performance_bin" "data.frame".
#' \item row.names : character. row name of data.frame with "Binning Table".
#' \item IV : numeric. Jeffrey's Information Value.
#' \item JSD : numeric. Jensen-Shannon Divergence.
#' \item KS : numeric. Kolmogorov-Smirnov Statistics.
#' \item gini : numeric. Gini index.
#' \item HHI : numeric. Herfindahl-Hirschman Index.
#' \item HHI_norm : numeric.normalized Herfindahl-Hirschman Index.
#' \item Cramer_V : numeric. Cramer's V Statistics.
#' \item chisq_test : data.frame. table of significance tests. name is as follows.
#' \itemize{
#'   \item Bin A : character. first bins.
#'   \item Bin B : character. second bins.
#'   \item statistics : numeric. statistics of Chi-square test.
#'   \item p_value : numeric. p-value of Chi-square test.
#'   }
#' }
#' @seealso \code{\link{summary.performance_bin}}, \code{\link{plot.performance_bin}}, \code{\link{binning_by}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' 
#' set.seed(123)
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#' 
#' # Change the target variable to 0(negative) and 1(positive).
#' heartfailure2$death_event_2 <- ifelse(heartfailure2$death_event %in% "Yes", 1, 0)
#' 
#' # Binnig from creatinine to platelets_bin.
#' breaks <- c(0,  1,  2, 10)
#' heartfailure2$creatinine_bin <- cut(heartfailure2$creatinine, breaks)
#' 
#' # Diagnose performance binned variable
#' perf <- performance_bin(heartfailure2$death_event_2, heartfailure2$creatinine_bin) 
#' perf
#' summary(perf)
#' 
#' plot(perf)
#' 
#' # Diagnose performance binned variable without NA
#' perf <- performance_bin(heartfailure2$death_event_2, heartfailure2$creatinine_bin, na.rm = TRUE) 
#' perf
#' summary(perf)
#' 
#' plot(perf)
#' }
#' 
#' @export
#' @import dplyr
#' 
performance_bin <- function (y, x, na.rm = FALSE) {
  uniq_y <- length(unique(y))
  type_y <- class(y)[1]
  type_x <- class(x)[1]
  
  if (uniq_y != 2) {
    stop("The number of levels of the y variable is not 2.")
  }
  
  if (type_y %in% c("integer", "numeric")) {
    if (!all(unique(y) %in% c(0, 1))) {
      stop("If y is a numeric variable, it can have only values of 0 and 1. (1: positive, 0: Negative)")
    }
  }
  
  if (type_y %in% c("factor", "ordered")) {
    yval <- levels(y)
    
    y <- as.integer(y) - 1
    msg <- sprintf("'%s' changed to 1 (positive) and '%s' changed to 0 (negative).", yval[2], yval[1])
    warning(paste("The factor y has been changed to a numeric vector consisting of 0 and 1.", msg, sep = "\n"))
  }
  
  if (type_y %in% c("character")) {
    yval <- levels(factor(y))
    
    y <- as.integer(factor(y))- 1
    msg <- sprintf("'%s' changed to 1 (positive) and '%s' changed to 0 (negative).", yval[2], yval[1])
    warning(paste("The character y has been changed to a numeric vector consisting of 0 and 1.", msg, sep = "\n"))
  }
  
  if (any(is.infinite(x))) 
    stop("x with an Inf. Replace by NA. or Remove the Inf.")
  
  if (length(unique(x)) < 2) 
    stop("No significant bins. x must be number of unique values greater then 1.")
  
  .data <- data.frame(Bin = x, y = y) 
  
  if (na.rm == TRUE) {
    .data <- .data %>% 
      filter(!is.na(Bin)) 
  }
  
  suppressWarnings(tab_metric <- .data %>% 
    filter(!is.na(y)) %>% 
    mutate(Bin =  factor(Bin)) %>% 
    group_by(Bin) %>% 
    summarise(CntRec = n(),
              CntPos = sum(ifelse(y == 1, 1, 0), na.rm = TRUE),
              CntNeg = sum(ifelse(y == 0, 1, 0), na.rm = TRUE),
              CntCumPos = NA,
              CntCumNeg = NA, .groups = 'drop') %>% 
    mutate(CntCumPos = cumsum(CntPos),
           CntCumNeg = cumsum(CntNeg)) %>% 
    bind_rows(
      .data %>%
        filter(!is.na(y)) %>% 
        mutate(Bin = "Total") %>% 
        group_by(Bin) %>% 
        summarise(CntRec = n(),
                  CntPos = sum(ifelse(y == 1, 1, 0), na.rm = TRUE),
                  CntNeg = sum(ifelse(y == 0, 1, 0), na.rm = TRUE),
                  .groups = 'drop')
    )) 
  
  tab_metric <- tab_metric %>% 
    mutate(RatePos = CntPos / (sum(CntPos) / 2),
           RateNeg = CntNeg / (sum(CntNeg) / 2),
           RateCumPos = ifelse(!Bin %in% "Total", cumsum(RatePos), NA),
           RateCumNeg = ifelse(!Bin %in% "Total", cumsum(RateNeg), NA),
           Odds = CntPos / CntNeg,
           Odds = ifelse(is.infinite(Odds), NA, Odds),
           LnOdds = log(Odds),
           WoE = log(RatePos / RateNeg),
           WoE = ifelse(is.infinite(WoE), NA, WoE),
           IV = (RatePos - RateNeg) * WoE,
           JSD = jsd(RatePos,  RateNeg),
           JSD = ifelse(is.nan(JSD), NA, JSD))
  
  tab_metric <- tab_metric %>% 
    mutate(RatePos = ifelse(Bin %in% "Total", 0, RatePos),
           REV = rev(cumsum(rev(RatePos))),
           AUC = (1 - REV) * RateNeg + RatePos * RateNeg / 2) %>% 
    mutate(RatePos = ifelse(Bin %in% "Total", 1, RatePos),
           WoE = ifelse(Bin %in% "Total", NA, WoE),
           IV = ifelse(Bin %in% "Total", sum(IV), IV),
           JSD = ifelse(Bin %in% "Total", sum(JSD), JSD),
           AUC = ifelse(Bin %in% "Total", 0, AUC),
           AUC = ifelse(Bin %in% "Total", sum(AUC), AUC)) %>% 
    select(-REV) 
  
  # case when using old group_by() of dplyr
  # dplyr package version is less than 0.8.0
  tab_metric <- tab_metric[, !names(tab_metric) %in% ".groups"]
  
  # Information Value
  IV <- tab_metric %>% 
    filter(Bin == "Total") %>% 
    select(IV) %>% 
    pull()
  
  # Jensen-Shannon Divergence
  JSD <- tab_metric %>% 
    filter(Bin == "Total") %>% 
    select(JSD) %>% 
    pull()
  
  # Kolmogorov-Smirnov statistics
  KS <- max(abs(tab_metric$RateCumPos - tab_metric$RateCumNeg), na.rm = TRUE)
  
  # Gini Coefficient 
  gini <- 2 * max(tab_metric$AUC, na.rm = TRUE) - 1
  
  ## Herfindahl-Hirschman Index
  hhi <- tab_metric %>%
    filter(!Bin %in% "Total") %>% 
    select(CntRec) %>% 
    pull() %>% 
    prop.table() 
  hhi <- hhi ^ 2 %>% 
    sum()
  
  ## normalized Herfindahl-Hirschman Index
  n <- tab_metric %>%
    filter(!Bin %in% "Total") %>% 
    nrow()
  
  hhi_norm <- (hhi - 1 / n) / (1 - 1 / n)
  
  # Cramer's V
  tab <- table(x, y)
  chisq_hat <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab)
  V <- as.numeric(sqrt(chisq_hat / (n * (min(dim(tab)) - 1))))
  
  # Rounding
  tab_metric[, -1] <- round(tab_metric[, -1] , 5) 
  
  # Chi Square Test
  bins <- setdiff(tab_metric$Bin, c("Total"))
  bins <- data.frame(A = bins, B = lead(bins)) %>% 
    filter(!is.na(A) & !is.na(B))
  
  chisq_test <- seq(nrow(bins)) %>% 
    map_dfr(function(x) {
      a <- .data[.data$Bin %in% bins[x, "A"], "y"]
      b <- .data[.data$Bin %in% bins[x, "B"], "y"]
      chisq <- suppressWarnings(chisq.test(rbind(table(a), table(b)), correct = FALSE))
      list(`Bin A` = bins[x, "A"], `Bin B` = bins[x, "B"], statistics = chisq$statistic,
           p_value = chisq$p.value)
    })
  
  list(metrics_table = data.frame(tab_metric), IV = IV, KS = KS, JSD = JSD, gini = gini, HHI = hhi, 
       HHI_norm = hhi_norm, Cramer_V = V, chisq_test = as.data.frame(chisq_test), cutoff = NULL)
  
  results <- data.frame(tab_metric)
  attr(results, "IV") <- IV
  attr(results, "KS") <- KS
  attr(results, "gini") <- gini
  attr(results, "HHI") <- hhi
  attr(results, "HHI_norm") <- hhi_norm  
  attr(results, "Cramer_V") <- V  
  attr(results, "chisq_test") <- as.data.frame(chisq_test)  
  attr(results, "cutoff") <- NULL  
  class(results) <- append("performance_bin", class(results))
  
  results
}


#' Summarizing Performance for Binned Variable
#'
#' @description summary method for "performance_bin". summary metrics to evaluate the performance 
#' of binomial classification model.
#' @param object an object of class "performance_bin", usually, a result of a call to performance_bin().
#' @param ... further arguments to be passed from or to other methods.
#' @details
#' print() to print only binning table information of "performance_bin" objects.
#' summary.performance_bin() includes general metrics and result of significance tests life follows.:
#' \itemize{
#'   \item Binning Table : Metrics by bins.
#'   \itemize{
#'     \item CntRec, CntPos, CntNeg, RatePos, RateNeg, Odds, WoE, IV, JSD, AUC.
#'   }
#'   \item General Metrics.
#'   \itemize{
#'     \item Gini index.
#'     \item Jeffrey's Information Value.
#'     \item Jensen-Shannon Divergence.
#'     \item Kolmogorov-Smirnov Statistics.
#'     \item Herfindahl-Hirschman Index.
#'     \item normalized Herfindahl-Hirschman Index.
#'     \item Cramer's V Statistics.
#'   } 
#'   \item Table of Significance Tests.
#' }
#' @return NULL.
#' @seealso \code{\link{performance_bin}}, \code{\link{plot.performance_bin}}, \code{\link{binning_by}}, 
#' \code{\link{summary.optimal_bins}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' 
#' set.seed(123)
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#' 
#' # Change the target variable to 0(negative) and 1(positive).
#' heartfailure2$death_event_2 <- ifelse(heartfailure2$death_event %in% "Yes", 1, 0)
#' 
#' # Binnig from creatinine to platelets_bin.
#' breaks <- c(0,  1,  2, 10)
#' heartfailure2$creatinine_bin <- cut(heartfailure2$creatinine, breaks)
#' 
#' # Diagnose performance binned variable
#' perf <- performance_bin(heartfailure2$death_event_2, heartfailure2$creatinine_bin) 
#' perf
#' summary(perf)
#' 
#' # plot(perf)
#' 
#' # Diagnose performance binned variable without NA
#' perf <- performance_bin(heartfailure2$death_event_2, heartfailure2$creatinine_bin, na.rm = TRUE) 
#' perf
#' summary(perf)
#' 
#' plot(perf)
#' }
#' 
#' @method summary performance_bin
#' @export
#' 
summary.performance_bin <- function(object, ...) {
  x <- object
  
  cat_rule(
    left = "Binning Table",
    right = "Several Metrics",
    width = 60
  )
  
  x %>% 
    select(-CntCumPos, -CntCumNeg, -RateCumPos, -RateCumNeg, -LnOdds) %>% 
    print()
  
  cat("\n")  
  
  nms <- c("Gini index", 
           "IV (Jeffrey)",
           "JS (Jensen-Shannon) Divergence",
           "Kolmogorov-Smirnov Statistics",
           "HHI (Herfindahl-Hirschman Index)", 
           "HHI (normalized)",
           "Cramer's V") 
  nms <- format(nms)
  
  vls <- c(attr(x, "gini"), max(x[, "IV"]), max(x[, "JSD"]),
           attr(x, "KS"), attr(x, "HHI"), attr(x, "HHI_norm"), attr(x, "Cramer_V"))
  
  cat_rule(
    left = "General Metrics",
    right = "",
    width = 60
  )
  
  info_scale <- paste0(nms, " :  ", round(vls, 5))
  cat_bullet(info_scale)
  cat("\n")
  
  cat_rule(
    left = "Significance Tests",
    right = "Chisquare Test",
    width = 60
  )
  
  attr(x, "chisq_test") %>% 
    print()
  cat("\n")  
}


#' Visualize Performance for an "performance_bin" Object
#'
#' @description
#' It generates plots for understand frequency, WoE by bins using performance_bin.
#'
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "performance_bin", usually, a result of a call to performance_bin().
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... further arguments to be passed from or to other methods.
#' @seealso \code{\link{performance_bin}}, \code{\link{summary.performance_bin}}, \code{\link{binning_by}}, 
#' \code{\link{plot.optimal_bins}}.
#' @examples
#' \donttest{
#' # Generate data for the example
#' heartfailure2 <- heartfailure
#' 
#' set.seed(123)
#' heartfailure2[sample(seq(NROW(heartfailure2)), 5), "creatinine"] <- NA
#' 
#' # Change the target variable to 0(negative) and 1(positive).
#' heartfailure2$death_event_2 <- ifelse(heartfailure2$death_event %in% "Yes", 1, 0)
#' 
#' # Binnig from creatinine to platelets_bin.
#' breaks <- c(0,  1,  2, 10)
#' heartfailure2$creatinine_bin <- cut(heartfailure2$creatinine, breaks)
#' 
#' # Diagnose performance binned variable
#' perf <- performance_bin(heartfailure2$death_event_2, heartfailure2$creatinine_bin) 
#' perf
#' summary(perf)
#' 
#' plot(perf)
#' 
#' # Diagnose performance binned variable without NA
#' perf <- performance_bin(heartfailure2$death_event_2, heartfailure2$creatinine_bin, na.rm = TRUE) 
#' perf
#' summary(perf)
#' 
#' plot(perf)
#' plot(perf, typographic = FALSE)
#' }
#' 
#' @import ggplot2
#' @import hrbrthemes
#' @import dplyr
#' @export
#' @method plot performance_bin
plot.performance_bin <- function(x, typographic = TRUE, base_family = NULL, ...) {
  scaleFactor <- 
    x %>% 
    filter(!Bin %in% "Total") %>% 
    select(Bin, CntPos, CntNeg, WoE) %>% 
    mutate(Bin = factor(Bin, levels = Bin)) %>% 
    tidyr::gather("target", "freq", -Bin, -WoE)  %>% 
    summarise(diff_prim = diff(range(freq, na.rm = TRUE)),
              diff_sec = diff(range(WoE, na.rm = TRUE)),
              min_prim = min(freq, na.rm = TRUE),
              min_sec = min(WoE, na.rm = TRUE)) %>% 
    transmute(b = diff_prim / diff_sec,
              a = b * (min_prim - min_sec)) 
  
  p <- x %>% 
    filter(!Bin %in% "Total") %>% 
    select(Bin, CntPos, CntNeg, WoE) %>% 
    mutate(Bin = factor(Bin, levels = Bin)) %>% 
    tidyr::gather("target", "freq", -Bin, -WoE) %>% 
    ggplot(aes(x = Bin)) +  
    geom_bar(aes(y = freq, fill = target), stat = "identity", width = 0.6) +
    geom_point(aes(y = scaleFactor$a + WoE * scaleFactor$b, group = 1), size = 2) +
    geom_line(aes(y = scaleFactor$a + WoE * scaleFactor$b, group = 1)) +
    labs(title = "Frequency and WoE by Bins", y = "Bin frequency") +
    scale_y_continuous(
      # Features of the first axis
      name = "Bin frequency",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~(. - scaleFactor$a) / scaleFactor$b, name = "WoE")
    ) +
    theme_grey(base_family = base_family) 
  
  if (!typographic) {
    suppressWarnings({p +
        scale_fill_discrete(labels = c("Negative", "Positive"))}) 
  } else {
    suppressWarnings({p +
        theme_typographic(base_family) +
        scale_fill_ipsum(labels = c("Negative", "Positive")) +
        theme(
          axis.title.y = element_text(size = 13),
          axis.title.y.right = element_text(size = 13)
        )})
  }
}


#' Binning by recursive information gain ratio maximization
#'
#' @description The binning_rgr() finding intervals for numerical variable
#' using recursive information gain ratio maximization. 
#'
#' @details This function can be usefully used when developing a model that predicts y.
#'
#' @param .data a data frame.
#' @param y character. name of binary response variable. 
#' The variable must character of factor.
#' @param x character. name of continuous characteristic variable. 
#' At least 5 different values. and Inf is not allowed.
#' @param min_perc_bins numeric. minimum percetange of rows for each split or 
#' segment (controls the sample size), 0.1 (or 10 percent) as default.
#' @param max_n_bins integer. maximum number of bins or segments 
#' to split the input variable, 5 bins as default. 
#' @param ordered logical. whether to build an ordered factor or not.
#' @return an object of "infogain_bins" class.
#' Attributes of "infogain_bins" class is as follows.
#' \itemize{
#' \item class : "infogain_bins".
#' \item type : binning type, "infogain".
#' \item breaks : numeric. the number of intervals into which x is to be cut.
#' \item levels : character. levels of binned value.
#' \item raw : numeric. raw data, x argument value.
#' \item target : integer. binary response variable.
#' \item x_var : character. name of x variable.
#' \item y_var : character. name of y variable.
#' }
#' @seealso \code{\link{binning}}, \code{\link{binning_by}}, \code{\link{plot.infogain_bins}}.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # binning by recursive information gain ratio maximization using character
#' bin <- binning_rgr(heartfailure, "death_event", "creatinine")
#' 
#' # binning by recursive information gain ratio maximization using name
#' bin <- binning_rgr(heartfailure, death_event, creatinine)
#' bin
#' 
#' # summary optimal_bins class
#' summary(bin)
#' 
#' # visualize all information for optimal_bins class
#' plot(bin)
#' 
#' # visualize WoE information for optimal_bins class
#' plot(bin, type = "cross")
#' 
#' # visualize all information without typographic
#' plot(bin, type = "cross", typographic = FALSE)
#' 
#' # extract binned results
#' extract(bin) %>% 
#'   head(20)
#' }
#' 
#' @export
#' @importFrom tibble is_tibble
#' @importFrom tidyselect vars_select
#' @importFrom rlang enquo
#' @import dplyr
binning_rgr <- function(.data, y, x, min_perc_bins = 0.1, max_n_bins = 5, ordered = TRUE) {
  y <- tidyselect::vars_select(names(.data), !! enquo(y))
  x <- tidyselect::vars_select(names(.data), !! enquo(x))
  
  if (tibble::is_tibble(.data)) {
    .data <- as.data.frame(.data)
  }
  
  uniq_y <- length(unique(.data[, y]))
  type_y <- class(.data[, y])[1]
  type_x <- class(.data[, x])[1]
  
  if (!is.data.frame(.data)) 
    stop("Data is not a data.frame.")
  
  if (!type_x %in% c("integer", "numeric")) 
    stop("x is not numeric value.")
  
  if (uniq_y != 2) {
    stop("The number of levels of the y variable is not 2.")
  }
  
  if (!type_y %in% c("character", "factor", "ordered")) {
    stop("y is not character or (ordered)factor.")
  }
  
  if (any(is.na(.data[, x]))) 
    stop("x with a NA. This fuction not support missing.")
  
  if (any(is.na(.data[, y]))) 
    stop("y with a NA. This fuction not support missing.")
  
  if (length(unique(.data[, x])) < 5) 
    stop("x must be number of unique values greater then 4.")
  
  if (requireNamespace("funModeling", quietly = TRUE)) {
    bins <- funModeling::discretize_rgr(
      .data[, x],
      .data[, y],
      min_perc_bins = min_perc_bins,
      max_n_bins = max_n_bins
    )
  } else {
    stop("Package 'funModeling' needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  
  bin_levels <- levels(bins)
  
  # calculate breakpoints
  breaks <- gsub(pattern = "[]\\[)]", "", bin_levels) 
  
  breaks_max <- gsub(pattern = "^[[:print:]]*,", "", breaks) %>% 
    as.numeric() %>% 
    max()
  
  breaks <- gsub(pattern = ",[[:print:]]*$", "", breaks) %>% 
    as.numeric() %>% 
    c(breaks_max)
  
  if (ordered == TRUE)
    bins <- ordered(bins)
  
  results <- bins
  attr(results, "type") <- "infogain"
  attr(results, "breaks") <- breaks
  attr(results, "levels") <- bin_levels
  attr(results, "raw") <- .data[, x]
  attr(results, "x_var") <- x  
  attr(results, "y_var") <- y
  class(results) <- append("bins", class(results))
  attr(results, "target") <- .data[, y]
  
  class(results) <- append("infogain_bins", class(results))
  
  results
}


#' Visualize Distribution for an "infogain_bins" Object
#'
#' @description
#' It generates plots for understand distribution and  distribution by target variable using infogain_bins.
#'
#' @details The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
#' "NanumSquare", "Noto Sans Korean". If you want to use a different font, 
#' use it after loading the Google font with import_google_font().
#' 
#' @param x an object of class "infogain_bins", usually, a result of a call to binning_rgr().
#' @param type character. options for visualization. Distribution("bar"), Relative Frequency by target ("cross").
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @param base_family character. The name of the base font family to use 
#' for the visualization. If not specified, the font defined in dlookr is applied. (See details)
#' @param ... further arguments to be passed from or to other methods.
#' @seealso \code{\link{binning_rgr}}, \code{\link{summary.bins}}
#' @examples
#' \donttest{
#' # binning by recursive information gain ratio maximization using character
#' bin <- binning_rgr(heartfailure, "death_event", "creatinine")
#' 
#' # binning by recursive information gain ratio maximization using name
#' bin <- binning_rgr(heartfailure, death_event, creatinine)
#' bin
#' 
#' # summary optimal_bins class
#' summary(bin)
#' 
#' # visualize all information for optimal_bins class
#' plot(bin)
#' 
#' # visualize WoE information for optimal_bins class
#' plot(bin, type = "cross")
#' 
#' # visualize all information without typographic
#' plot(bin, type = "cross", typographic = FALSE)
#' }
#' 
#' @import ggplot2
#' @import hrbrthemes
#' @importFrom gridExtra grid.arrange
#' @export
#' @method plot infogain_bins
plot.infogain_bins <- function(x, type = c("bar", "cross"), 
                                 typographic = TRUE, base_family = NULL, ...) {
  type <- match.arg(type)
  
  if (type %in% c("bar")) {
    plot.bins(x, typographic = typographic, base_family = base_family)
  } else if (type %in% c("cross")) {
    x_var = attr(x, "x_var")
    y_var = attr(x, "y_var")
    
    dfm <- data.frame(x = x, y = attr(x, "target"))
    
    dfm_summary <- dfm %>% 
      group_by(x) %>%
      count(y) %>% 
      mutate(ratio = round(n / sum(n) * 100, 2))
    
    p_stack <- dfm %>% 
      ggplot(aes(x = x, fill = y)) +
      geom_bar(position = "fill", show.legend = FALSE) +
      geom_text(data = dfm_summary,
                aes(y = n, label = ratio), color = "black",
                position = position_fill(vjust = 0.5)) +
      labs(title = sprintf("Relative frequency by bins"),
           x = x_var, y = "Relative Frequency (%)")
    
    p_dodge <- dfm_summary %>% 
      ggplot(aes(x = x, y = n, fill = y)) +
      geom_bar(stat="identity", position = position_dodge()) +
      geom_text(aes(label = n), vjust = 1.6, color = "black",
                position = position_dodge(0.9), size = 3.5)+
      labs(title = sprintf("Frequency by bins"),
           x = x_var, y = "Frequency (Count)") +
      guides(fill = guide_legend(title = y_var))
    
    if (typographic) {
      p_stack <- p_stack +
        theme_typographic(base_family) 
      
      p_dodge <- p_dodge +
        theme_typographic(base_family)             
    }
    
    p_dodge
    
    suppressWarnings(gridExtra::grid.arrange(p_stack, p_dodge, nrow = 1, ncol = 2)) 
  }  
}  

