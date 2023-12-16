#' Sales of Child Car Seats
#' 
#' @description 
#' A simulated data set containing sales of child car seats at 400 different stores.
#' 
#' @format A data frame with 400 rows and 11 variables. The variables are as follows:
#' \describe{
#'   \item{Sales}{Unit sales (in thousands) at each location.}
#'   \item{CompPrice}{Price charged by competitor at each location.}
#'   \item{Income}{Community income level (in thousands of dollars).}
#'   \item{Advertising}{Local advertising budget for company at each location (in thousands of dollars).}
#'   \item{Population}{Population size in region (in thousands).}
#'   \item{Price}{Price company charges for car seats at each site.}
#'   \item{ShelveLoc}{A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site.}
#'   \item{Age}{Average age of the local population.}
#'   \item{Education}{Education level at each location.}
#'   \item{Urban}{A factor with levels No and Yes to indicate whether the store is in an urban or rural location.}
#'   \item{US}{A factor with levels No and Yes to indicate whether the store is in the US or not.}
#' }
#' @docType data
#' @keywords datasets
#' @name Carseats
#' @usage data(Carseats)
#' @source 
#' "Sales of Child Car Seats" in ISLR package <https://CRAN.R-project.org/package=ISLR>, License : GPL-2
NULL
# Carseats <- ISLR::Carseats
# save(Carseats, file = "data/Carseats.rda", version = 2)
