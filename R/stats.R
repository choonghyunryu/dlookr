#' Calculate Information Value
#'
#' @description The ivtable() calculate a information value and information value.
#'
#' @details When developing the scorecard, use WoE(weight of evidence) and IV(information value). 
#' This information is useful when binning continuous variables.
#' @param .data a data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param y character or name. the name of target variable in .data.
#' @param x character or name. the name of indicator variable in .data.
#' @param p numeric. ratio of records per bin. Default is 0.05. This parameter only accepts values that range is (0, 0.5].
#' @return list. information value table and information value.
#' Attributes of list is as follows.
#' \itemize{
#' \item ivtable : data.frame. information value table. 
#'   \itemize{
#'     \item Cutpoint : character. class by binned using cut points.
#'     \item CntRec : numeric. frequency.
#'     \item CntGood : numeric. frequency with positive(good) cases.
#'     \item CntBad : numeric. frequency with negative(bad) cases.
#'     \item CntCumRec : numeric. cumulate frequency.  
#'     \item CntCumGood : numeric. cumulate frequency with positive(good) cases.
#'     \item CntCumBad : numeric. cumulate frequency with negative(bad) cases.
#'     \item PctRec : numeric. relative frequency. 
#'     \item GoodRate : numeric. relative frequency with positive(good) cases.
#'     \item BadRate : numeric. relative frequency with negative(bad) cases.
#'     \item Odds : numeric. odds.
#'     \item LnOdds : numeric. ln(odds)
#'     \item WoE : numeric. weight of evidence.
#'     \item IV : numeric. information value.
#'   }
#' \item iv : numeric. information value.
#' }
#' @seealso \code{\link{binning_by}}, \code{\link{print.bins}}, \code{\link{summary.bins}}.
#' @export
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' 
#' # Prepare the target variable (positive : 1, negative : 0)
#' carseats$US2 <- as.integer(carseats$US) - 1
#' 
#' ivtable(carseats, y = "US2", x = "Advertising")
#' ivtable(carseats, y = US2, x = Advertising)
#' 
#' @importFrom tibble is_tibble
#' @importFrom partykit ctree
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
ivtable <- function (.data, y, x, p = 0.05) 
{
  y <- tidyselect::vars_select(names(.data), !! enquo(y))
  x <- tidyselect::vars_select(names(.data), !! enquo(x))
  
  if (tibble::is_tibble(.data)) {
    .data <- as.data.frame(.data)
  }
  
  library(dplyr)
  
  if (!is.data.frame(.data)) 
    stop("Data not a data.frame")

  if (!is.numeric(.data[, x])) 
    stop("x is not numeric value")
  
  if (!is.numeric(.data[, y])) 
    stop("y is not numeric value")

  if (!all(range(.data[, y], na.rm = TRUE) == c(0, 1))) 
    stop("y must be minimum is 1 and maximum is 1")

  if (any(is.infinite(.data[, x]))) 
    stop("x with an Inf. Replace by NA")
  
  if (p <= 0 | p > 0.5)
    stop("p must be, 0 < p <= 0.5")

  if (length(unique(.data[, x])) < 5) 
    stop("x must be number of unique values greater then 4")
  
  ctree <- ctree(formula(paste(y, "~", x)), data = .data, na.action = na.exclude,
                 control = ctree_control(minbucket = ceiling(round(p * nrow(.data)))))
  
  bins <- width(ctree)
  if (bins < 2) {
    return("No significant splits")
  }

  cutvct <- seq(ctree) %>%
    map_dbl(function(x) {
      breaks <- ctree[x]$node$split$breaks
      ifelse(is.null(breaks), NA, breaks)
    }) %>% 
    sort() 
  max_cutpoint <- max(cutvct)
    
  x_idx <- which(names(.data) %in% x)
  y_idx <- which(names(.data) %in% y)
    
  ivt <- cutvct %>%
    map_dfr(function(cutpoint) {
      .data %>% 
        select(x = all_of(x_idx), y = all_of(y_idx)) %>% 
        filter(!is.na(x) & !is.na(y)) %>% 
        mutate(Cutpoint = paste0("<= ", cutpoint)) %>% 
        mutate(CntCumRec = ifelse(x <= cutpoint & y %in% c(0, 1), 1, 0)) %>% 
        mutate(CntCumGood = ifelse(x <= cutpoint & y == 1, 1, 0)) %>% 
        mutate(CntCumBad = ifelse(x <= cutpoint & y == 0, 1, 0)) %>% 
        group_by(Cutpoint) %>% 
        summarise(CntRec = NA,
                  CntGood = NA,
                  CntBad = NA,
                  CntCumRec = sum(CntCumRec),
                  CntCumGood = sum(CntCumGood),
                  CntCumBad = sum(CntCumBad), .groups = 'drop')
    }) %>% 
    bind_rows(
      .data %>%
        select(x = all_of(x_idx), y = all_of(y_idx)) %>%
        filter(!is.na(x) & !is.na(y)) %>% 
        mutate(Cutpoint = paste0("> ", max_cutpoint)) %>% 
        mutate(CntCumRec = ifelse(y %in% c(0, 1), 1, 0)) %>% 
        mutate(CntCumGood = ifelse(y == 1, 1, 0)) %>% 
        mutate(CntCumBad = ifelse(y == 0, 1, 0)) %>% 
        group_by(Cutpoint) %>% 
        summarise(CntRec = NA,
                  CntGood = NA,
                  CntBad = NA,
                  CntCumRec = sum(CntCumRec),
                  CntCumGood = sum(CntCumGood),
                  CntCumBad = sum(CntCumBad), .groups = 'drop')
    ) %>% 
    mutate(CntRec = CntCumRec - lag(CntCumRec, default = 0)) %>% 
    mutate(CntGood = CntCumGood - lag(CntCumGood, default = 0)) %>% 
    mutate(CntBad = CntCumBad - lag(CntCumBad, default = 0)) %>% 
    bind_rows(
      .data %>% 
        select(x = all_of(x_idx), y = all_of(y_idx)) %>% 
        filter(!is.na(y)) %>% 
        mutate(Cutpoint = "Missing") %>% 
        mutate(CntRec = ifelse(is.na(x) & y %in% c(0, 1), 1, 0)) %>% 
        mutate(CntGood = ifelse(is.na(x) & y == 1, 1, 0)) %>% 
        mutate(CntBad = ifelse(is.na(x) & y == 0, 1, 0)) %>% 
        mutate(CntCumGood = ifelse(y == 1, 1, 0)) %>% 
        mutate(CntCumBad = ifelse(y == 0, 1, 0)) %>%                
        group_by(Cutpoint) %>% 
        summarise(CntRec = sum(CntRec),
                  CntGood = sum(CntGood),
                  CntBad = sum(CntBad),
                  CntCumRec = n(),
                  CntCumGood = sum(CntCumGood),
                  CntCumBad = sum(CntCumBad), .groups = 'drop')
      )
    
  ivt <- bind_rows(
    ivt,
    ivt %>% 
      mutate(Cutpoint = "Total") %>% 
      group_by(Cutpoint) %>% 
      summarise(CntRec = sum(CntRec),
                CntGood = sum(CntGood),
                CntBad = sum(CntBad),
                CntCumRec = NA,
                CntCumGood = NA,
                CntCumBad = NA, .groups = 'drop')
    )
    
  ivt$PctRec <- ivt$CntRec / pull(ivt[ivt$Cutpoint %in% "Total", "CntRec"])
  ivt$GoodRate <- ivt$CntGood / ivt$CntRec
  ivt$BadRate <- ivt$CntBad / ivt$CntRec
  ivt$Odds <- ivt$GoodRate / ivt$BadRate
  ivt$LnOdds <- log(ivt$Odds)

  G <- pull(ivt[ivt$Cutpoint %in% "Total", "CntGood"])
  B <- pull(ivt[ivt$Cutpoint %in% "Total", "CntBad"])
  LnGB <- log(G/B)
  ivt$WoE <- log(ivt$CntGood/ivt$CntBad) - LnGB
    
  ivt$IV <- ivt$WoE * (ivt$CntGood / G - ivt$CntBad / B)
    
  n <- nrow(ivt)
    
  tmp <- sum(ifelse(is.finite(ivt$IV[1:(n - 1)]), ivt$IV[1:(n - 1)], 0))
  iv <- ivt$IV[n] <- round(tmp, 4)
  
  ivt$GoodRate <- round(ivt$GoodRate, 4)
  ivt$BadRate <- round(ivt$BadRate, 4)
  ivt$Odds <- round(ivt$Odds, 4)
  ivt$LnOdds <- round(ivt$LnOdds, 4)
  ivt$WoE <- round(ivt$WoE, 4)
  ivt$IV <- round(ivt$IV, 4)                     
  
  min_max <- range(.data[, x], na.rm = TRUE)
  bands <- c(min_max[1], cutvct, min_max[2])
  
  list(ivtable = as.data.frame(ivt), iv = iv)
}