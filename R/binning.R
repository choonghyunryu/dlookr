#' Binning the Numeric Data
#'
#' @description The binning() converts a numeric variable to a categorization variable.
#'
#' @details This function is useful when used with the mutate/transmute
#' function of the dplyr package.
#'
#' @section "bins" class attributes information:
#' Attributes of the "bins" classs that is as follows.
#'
#' \itemize{
#' \item class : "bins".
#' \item levels : factor or ordered factor levels
#' \item type : binning method
#' \item breaks : breaks for binning
#' \item raw : before the binned the raw data
#' }
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param x numeric vector for binning.
#' @param nbins number of classes. required. if missing, nclass.Sturges is used.
#' @param type binning method. one of "quantile", "equal", "equal", "pretty", "kmeans", "bclust"
#' The "quantile" style provides quantile breaks.
#' The "equal" style divides the range of the variable into nbins parts.
#' The "pretty" style chooses a number of breaks not necessarily equal
#' to nbins using base::pretty function.
#' The "kmeans" style uses stats::kmeans function to generate the breaks.
#' The "bclust" style uses e1071::bclust function to generate the breaks using bagged clustering.
#' "kmeans" and "bclust" type logic was implemented by classInt::classIntervals function.
#' @param ordered whether to build an ordered factor or not.
#' @param labels the label names to use for each of the bins.
#' @param approxy.lab logical. If TRUE, large number breaks are approximated to pretty numbers. 
#' If FALSE, the original breaks obtained by type are used.
#' @return An object of bins class.
#' Attributes of bins class is as follows.
#' \itemize{
#' \item type : binning type, "quantile", "equal", "pretty", "kmeans", "bclust".
#' \item breaks : the number of intervals into which x is to be cut.
#' \item levels : levels of binned value.
#' \item raw : raw data, x argument value.
#' }
#' @seealso \code{\link{binning_by}}, \code{\link{print.bins}}, \code{\link{summary.bins}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' # Binning the carat variable. default type argument is "quantile"
#' bin <- binning(carseats$Income)
#' # Print bins class object
#' bin
#' # Summarise bins class object
#' summary(bin)
#' # Plot bins class object
#' plot(bin)
#' # Using labels argument
#' bin <- binning(carseats$Income, nbins = 4,
#'               labels = c("LQ1", "UQ1", "LQ3", "UQ3"))
#' bin
#' # Using another type argument
#' bin <- binning(carseats$Income, nbins = 5, type = "equal")
#' bin
#' bin <- binning(carseats$Income, nbins = 5, type = "pretty")
#' bin
#' bin <- binning(carseats$Income, nbins = 5, type = "kmeans")
#' bin
#' bin <- binning(carseats$Income, nbins = 5, type = "bclust")
#' bin
#' 
#' x <- sample(1:1000, size = 50) * 12345679
#' bin <- binning(x)
#' bin
#' bin <- binning(x, approxy.lab = FALSE)
#' bin
#'
#' # -------------------------
#' # Using pipes & dplyr
#' # -------------------------
#' library(dplyr)
#'
#' carseats %>%
#'  mutate(Income_bin = binning(carseats$Income)) %>%
#'  group_by(ShelveLoc, Income_bin) %>%
#'  summarise(freq = n()) %>%
#'  arrange(desc(freq)) %>%
#'  head(10)
#' @importFrom classInt classIntervals
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
    xx <- na.omit(x)
    if (type == "bclust") {
      ci <- classInt::classIntervals(var = xx, n = nbins, style = type, verbose = FALSE)
    } else {
      ci <- classInt::classIntervals(var = xx, n = nbins, style = type)
    }
    breaks <- ci$brks
  }

  breaks <- unique(breaks)
  fct <- cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  if (is.null(labels)) {
    pretty.lab <- levels(fct) %>% 
      gsub("[(\\[]|]", "", .) %>% 
      strsplit(",") %>% 
      sapply(c) %>% 
      as.numeric() %>% 
      unique()
    pretty.lab[1] <- min(x, na.rm = TRUE)
    
    if (approxy.lab) {
      if (!all(pretty.lab == breaks)) {
        fct <- cut(x, breaks = pretty.lab, labels = levels(fct), include.lowest = TRUE)
      }
    } else {
      if (!all(pretty.lab == breaks)) {
        lab <- NULL
        for (i in 2:length(breaks)) {
          lab <- c(lab, paste0("(", prettyNum(breaks[i-1]), ",", prettyNum(breaks[i]), "]"))
        }
        lab[1] <- sub("^\\(", "[", lab[1])
        
        levels(fct) <- lab
      }    
    } 
  }

  if (ordered == TRUE)
    fct <- ordered(fct)

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
#' @description summary method for class "bins" and "optimal_bins".
#' @param object an object of class "bins" and "optimal_bins",
#' usually, a result of a call to binning().
#' @details
#' print.bins() tries to be smart about formatting the frequency of bins, binned type, number of bins.
#' summary.bins() tries to be smart about formatting the levles, frequency of levels(bins),
#' the ratio of levels in total observations. And this information is data.frame object.
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @return
#' The function summary.bins() computes and returns a data.frame of summary statistics of the
#' binned given in object. Variables of data frame is as follows.
#' \itemize{
#' \item levels : levles of factor.
#' \item freq : frequency of levels.
#' \item rate : ratio of levels in total observations. it is not percentage.
#' }
#' @seealso \code{\link{binning}}
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Binning the carat variable. default type argument is "quantile"
#' bin <- binning(carseats$Income)
#'
#' # Print bins class object
#' bin
#'
#' # Summarise bins class object
#' summary(bin)
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

#' Visualize Distribution for an "bins" object
#'
#' @description
#' Visualize both plots on a single screen.
#' The plot at the top is a histogram representing the frequency of the level.
#' The plot at the bottom is a bar chart representing the frequency of the level.
#' @param x an object of class "bins", usually, a result of a call to binning().
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' @seealso \code{\link{binning}}, \code{\link{print.bins}}, \code{\link{summary.bins}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Binning the carat variable. default type argument is "quantile"
#' bin <- binning(carseats$Income, nbins = 5)
#' plot(bin)
#' # Using another type argument
#' bin <- binning(carseats$Income, nbins = 5, type = "equal")
#' plot(bin)
#' bin <- binning(carseats$Income, nbins = 5, type = "pretty")
#' plot(bin)
#' bin <- binning(carseats$Income, nbins = 5, type = "kmeans")
#' plot(bin)
#' bin <- binning(carseats$Income, nbins = 5, type = "bclust")
#' plot(bin)
#' @export
#' @method plot bins
#' @importFrom graphics plot
plot.bins <- function(x, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  brks <- attr(x, "breaks")

  par(mfrow = c(2, 1), mar = c(3, 4, 3, 1))
  hist(attr(x, "raw"), breaks = brks, col = "lightblue",
    main = sprintf("Histogram of original data using '%s' method",
      attr(x, "type")),
    xlab = "original data", ...)

  plot(factor(x), col = "lightgrey",
    ylab = "Frequency",
    main = sprintf("Bar plot of levles frequency using '%s' method",
      attr(x, "type")), ...)
}


#' Optimal Binning for Scoring Modeling
#'
#' @description The binning_by() finding class intervals for numerical variable
#' using optical binning. Optimal binning categorizes a numeric characteristic
#' into bins for ulterior usage in scoring modeling.
#'
#' @details This function is useful when used with the mutate/transmute
#' function of the dplyr package. And this function is implemented using
#' smbinning() function of smbinning package.
#'
#' @section "optimal_bins" class attributes information:
#' Attributes of the "optimal_bins" classs that is as follows.
#' \itemize{
#' \item class : "optimal_bins".
#' \item levels : factor or ordered factor levels
#' \item type : binning method
#' \item breaks : breaks for binning
#' \item raw : before the binned the raw data
#' \item ivtable : information value table
#' \item iv : information value
#' \item target : binary response variable
#' }
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param df a data frame.
#' @param y binary response variable (0,1). Integer(int) is required.
#' Name of y must not have a dot. Name "default" is not allowed.
#' @param x continuous characteristic. At least 5 different values.
#' Value Inf is not allowed. Name of x must not have a dot.
#' @param p percentage of records per bin. Default 5\% (0.05).
#' This parameter only accepts values greater that 0.00 (0\%) and lower than 0.50 (50\%).
#' @param ordered whether to build an ordered factor or not.
#' @param labels the label names to use for each of the bins.
#' @return an object of optimal_bins class.
#' Attributes of optimal_bins class is as follows.
#' \itemize{
#' \item type : binning type, "optimal".
#' \item breaks : the number of intervals into which x is to be cut.
#' \item levels : levels of binned value.
#' \item raw : raw data, x argument value.
#' \item ivtable : information value table
#' \item iv : information value
#' \item flag : information value
#' }
#' @seealso \code{\link{binning}}, \code{\link{smbinning}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # optimal binning
#' bin <- binning_by(carseats, "US", "Advertising")
#' bin
#' # summary optimal_bins class
#' summary(bin)
#' # visualize optimal_bins class
#' plot(bin, sub = "bins of Advertising variable")
#' @importFrom smbinning smbinning
#' @export
binning_by <- function(df, y, x, p = 0.05, ordered = TRUE, labels = NULL) {
  df <- as.data.frame(df)

  uniq_y <- length(unique(df[, y]))
  type_y <- class(df[, y])[1]
  type_x <- class(df[, x])[1]

  if (!type_x %in% c("integer", "numeric")) {
    stop("The variable x is not numerical type.")
  }

  if (uniq_y != 2) {
    stop("The number of levels of the y variable is not 2.")
  }

  if (type_y %in% c("integer", "numeric")) {
    if (!all(unique(df[, y]) %in% c(0, 1))) {
      stop("If y is a numeric variable, it can have only values of 0 and 1.")
    }
  }

  if (type_y %in% c("factor", "ordered")) {
    df[, y] <- as.integer(df[, y]) - 1
    warning("The factor y has been changed to a numeric vector consisting of 0 and 1.")
  }

  if (type_y %in% c("character")) {
    df[, y] <- as.integer(factor(df[, y]))- 1
    warning("The character y has been changed to a numeric vector consisting of 0 and 1.")
  }

  tab <- which(names(df) %in% "table")

  if (length(tab) > 0) {
    if (x == "table") {
      x <- "table_elbat_x"
      names(df)[tab] <- x
    } else if (y == "table") {
      y <- "table_elbat_y"
      names(df)[tab] <- y
    }
  }

  smb <- smbinning::smbinning(as.data.frame(df), y, x)

  if (!is.character(smb)) {
    breaks <- smb$bands
    tab <- table(breaks)

    dup <- as.numeric(names(tab)[which(tab == 2)])
    if (length(dup) > 0) {
      breaks <- unique(c(ifelse(min(breaks) == dup, dup - 1, dup + 1), breaks))
    }

    fct <- cut(df[, x], breaks = breaks, labels = labels, include.lowest = TRUE)

    if (ordered == TRUE)
      fct <- ordered(fct)

    results <- fct
    attr(results, "type") <- "optimal"
    attr(results, "breaks") <- breaks
    attr(results, "levels") <- levels(fct)
    attr(results, "raw") <- df[, x]
    class(results) <- append("bins", class(results))

    attr(results, "ivtable") <- smb$ivtable
    attr(results, "iv") <- smb$iv
    attr(results, "target") <- df[, y]

    class(results) <- append("optimal_bins", class(results))
  } else {
    results <- smb
    class(results) <- append("optimal_bins", class(results))
    warning(smb, "\n")
  }

  results
}

#' Visualize Distribution for an "optimal_bins" Object
#'
#' @description
#' It generates plots for distribution, bad rate, and weight of evidence after
#' running smbinning and saving its output.
#'
#' See vignette("transformation") for an introduction to these concepts.
#'
#' @param x an object of class "optimal_bins", usually,
#' a result of a call to binning_by().
#' @param type options for visualization. Distribution ("dist"), Good Rate ("goodrate"),
#' Bad Rate ("badrate"), and Weight of Evidence ("WoE").
#' @param sub subtitle for the chart (optional).
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' only applies to the first graph that is implemented with the boxplot() function.
#' @seealso \code{\link{binning_by}}, \code{\link{plot.bins}}, \code{\link{smbinning.plot}}.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # optimal binning
#' bin <- binning_by(carseats, "US", "Advertising")
#' bin
#'
#' # summary optimal_bins class
#' summary(bin)
#'
#' # information value
#' attr(bin, "iv")
#'
#' # information value table
#' attr(bin, "ivtable")
#'
#' # visualize optimal_bins class
#' plot(bin, sub = "bins of Advertising variable")
#' @importFrom graphics boxplot mtext
#' @importFrom smbinning smbinning.plot
#' @export
#' @method plot optimal_bins
plot.optimal_bins <- function(x, type = c("dist", "goodrate", "badrate", "WoE"),
                              sub = "", ...) {
  if (is.character(x)) {
    cat("binn is the optimal_bins object that can not be binned : \n",
        x, "\n", sep = "")
  } else {
    op <- par(no.readonly = TRUE)
    on.exit(op)

    par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))

    mat <- list(ivtable = attr(x, "ivtable"))

    boxplot(attr(x, "raw") ~ attr(x, "target"),
            horizontal = TRUE, frame = FALSE, col = "lightgray",
            main = "Distribution", ...)
    mtext(sub, 3)
    smbinning.plot(mat, option = "dist", sub = sub)
    smbinning.plot(mat, option = "badrate", sub = sub)
    if (!any(is.infinite(attr(x, "ivtable")$WoE)))
      smbinning.plot(mat, option = "WoE", sub = sub)
  }
}
